# =============================================================================
# KORREKTUR & ERWEITERUNG  -  R-Auswertung EFH-Leitungswasser
# Datei : Korrektur_Leitungswasser_Exposure.R
# Autor : Claude / Aktuariat
# Stand : 22.05.2026
# -----------------------------------------------------------------------------
# Diese Datei enthaelt NUR die geaenderten / neuen Bloecke. Sie ist als
# Drop-in fuer "2021_Analyse_LW.R" bzw. "Leitungswasser.R" gedacht.
# Jeder Block ist mit [ERSETZEN] oder [NEU] markiert.
#
# Kernproblem (siehe Bericht, Abschnitt 3 + 7):
#   Bisher  : exposure_years = `Haltedauer in Jahren`  (= Vertrags-Exposure)
#             claim_count    = ANZAHL_SCHAEDEN          (= alle Gefahren)
#   Folge   : Zaehler (Schaeden) und Nenner (Exposure) gehoeren zu
#             UNTERSCHIEDLICHEN Risikokollektiven  ->  Frequenz/Severity/
#             Burning Cost der Gefahr Leitungswasser sind verzerrt.
#   Loesung : Beide Groessen MUESSEN sich auf dieselbe Gefahr beziehen.
#             Exposure  -> Spalte "JE Leitungswasser"  (neue SQL-Spalte)
#             Schaeden  -> "Anzahl Schäden Leitungswasser" /
#                          "Durchschnittsschaden Leitungswasser"
# =============================================================================

library(dplyr)
library(tidyr)


# =============================================================================
# [ERSETZEN]  A-2 | Datenaufbereitung  -  Block "df2 <- df_lw %>% mutate(...)"
# -----------------------------------------------------------------------------
# Nur die schaden-/exposurebezogenen mutate-Zeilen werden geaendert.
# =============================================================================
df2 <- df_lw %>%
  mutate(
    across(everything(), ~ if (is.character(.x)) char_clean(.x) else .x),

    # ---- Numerische Felder robust parsen ------------------------------------
    # FRUEHER:  exposure_years_raw = num_clean(`Haltedauer in Jahren`)
    # JETZT  :  gefahrenscharfe Exposure aus der neuen SQL-Spalte
    exposure_years_raw = num_clean(`JE Leitungswasser`),

    # Vertrags-Haltedauer nur noch als Kontroll-/Plausibilitaetsgroesse
    haltedauer_vertrag = num_clean(`Haltedauer in Jahren`),

    # Schadenfelder: gefahrenscharf, falls die SQL-Erweiterung (Variante A/B)
    # umgesetzt ist; sonst Fallback auf die Vertragsspalten (mit Warnung).
    claim_count_raw = if ("Anzahl Schäden Leitungswasser" %in% names(df_lw)) {
      num_clean(`Anzahl Schäden Leitungswasser`)
    } else {
      warning("Spalte 'Anzahl Schäden Leitungswasser' fehlt - ",
              "Fallback auf ANZAHL_SCHAEDEN (alle Gefahren, verzerrt!).")
      num_clean(ANZAHL_SCHAEDEN)
    },
    avg_claim_raw = if ("Durchschnittsschaden Leitungswasser" %in% names(df_lw)) {
      num_clean(`Durchschnittsschaden Leitungswasser`)
    } else {
      warning("Spalte 'Durchschnittsschaden Leitungswasser' fehlt - ",
              "Fallback auf DURCHSCHNITTSSCHADEN (alle Gefahren, verzerrt!).")
      num_clean(DURCHSCHNITTSSCHADEN)
    },

    wohnflaeche_raw = num_clean(WOHNFLAECHEQM_WGB),
    baujahr_raw     = num_clean(BAUJAHR_WGB),

    # ---- Exposure absichern -------------------------------------------------
    # WICHTIG: NICHT mehr pauschal 0 -> 1/12 setzen!
    # JE Leitungswasser == 0 / NA bedeutet "Gefahr am Stichtag nicht versichert"
    # -> diese Vertraege gehoeren NICHT in das Leitungswasser-Kollektiv und
    #    werden weiter unten herausgefiltert (nicht kuenstlich aufgefuellt).
    # Der 1-Monats-Floor gilt nur fuer echte, sehr kurze LW-Deckungen.
    exposure_years = case_when(
      is.na(exposure_years_raw)         ~ NA_real_,
      exposure_years_raw <= 0           ~ NA_real_,        # Gefahr nicht aktiv
      exposure_years_raw <  1/12        ~ 1/12,            # Mindest-Exposure
      TRUE                              ~ exposure_years_raw
    ),

    # ---- Schadenvariablen ---------------------------------------------------
    claim_count      = coalesce(claim_count_raw, 0),
    avg_claim        = avg_claim_raw,
    total_loss_proxy = coalesce(claim_count_raw, 0) * coalesce(avg_claim_raw, 0),

    # ---- Zielgroessen annualisiert -  jetzt GEFAHRENSCHARF ------------------
    freq_pa = safe_rate(claim_count,      exposure_years),   # LW-Schaeden / LW-JE
    loss_pa = safe_rate(total_loss_proxy, exposure_years),   # LW-Aufwand / LW-JE

    # ---- Gebaeudealter / Wohnflaechenklassen (unveraendert) -----------------
    current_year = as.integer(format(Sys.Date(), "%Y")),
    building_age = case_when(
      !is.na(baujahr_raw) & baujahr_raw > 1700 & baujahr_raw <= current_year ~
        current_year - baujahr_raw,
      TRUE ~ NA_real_
    )
    # ... wohnflaeche_band etc. unveraendert ...
  ) %>%
  # ---- NEU: Vertraege ohne aktive LW-Deckung konsequent ausschliessen -------
  filter(!is.na(exposure_years), exposure_years > 0)

# Plausibilitaet: LW-Exposure darf nie groesser als Vertrags-Haltedauer sein
stopifnot(all(df2$exposure_years <= df2$haltedauer_vertrag + 1e-6, na.rm = TRUE))


# =============================================================================
# [ERSETZEN]  A-10/A-11  -  Frequenz- und Severity-Modell
# -----------------------------------------------------------------------------
# Der Offset MUSS die gefahrenscharfe Exposure sein. Inhaltlich unveraendert,
# aber model_df erbt jetzt die korrigierte exposure_years.
# =============================================================================
model_df <- df2 %>%
  transmute(
    claim_count    = pmax(0, round(claim_count)),
    exposure_years = pmax(exposure_years, 1/12),   # >0 ist durch Filter sicher
    state          = top_n_other(state, n = 12),
    age_band       = as.factor(age_band),
    wohnflaeche_band = as.factor(wohnflaeche_band),
    ZUERSZONE_WGB  = top_n_other(ZUERSZONE_WGB, n = 8),
    Tarif          = top_n_other(`Tarifausprägung`, n = 8),
    Bauart         = top_n_other(BAUARTGEBAEUDE_WGB, n = 8),
    Dach           = top_n_other(MATERIALDACH_WGB,   n = 8),
    Bauklasse      = top_n_other(BAUARTKLASSE_WGB,   n = 8),
    Nutzung        = top_n_other(NUTZUNGSART_WGB,    n = 8),
    NACHHALTIGKEIT = top_n_other(NACHHALTIGKEIT,     n = 5)
  ) %>%
  filter(!is.na(exposure_years), exposure_years > 0)

# Negativ-Binomial (offset = log der LW-Exposure, NICHT der Vertrags-Exposure)
freq_nb <- MASS::glm.nb(
  claim_count ~ offset(log(exposure_years)) + state + age_band +
    wohnflaeche_band + ZUERSZONE_WGB + Tarif + Bauart + Dach +
    Bauklasse + Nutzung + NACHHALTIGKEIT,
  data = model_df
)
summary(freq_nb)


# =============================================================================
# [NEU]  A-13 | Long-Format ueber ALLE Gefahren  (Basis fuer Mehrdimensionalitaet)
# -----------------------------------------------------------------------------
# Erzeugt aus den breiten SQL-Spalten "JE <Gefahr>" (+ optional
# "Anzahl Schäden <Gefahr>" / "Durchschnittsschaden <Gefahr>") ein tidy
# Long-Format mit GENAU EINER Zeile je (Vertrag x Gefahr).
# Das ist die Datenstruktur fuer das mehrdimensionale Credibility-Modell
# (Buch, Kapitel 7): je Gefahr eine Komponente, je Gefahr ein Gewicht.
# =============================================================================
gefahren <- c("Wohngebäude","Feuer","Leitungswasser","Sturm/Hagel","Glas",
              "Elementar","HUG","Gewässerschadenhaftpflicht",
              "Allgefahren Wohngebäude","Allgefahren erneuerbare Energien",
              "Allgefahren Haustechnik","Nachhaltigkeit","Erdbebenzone 3",
              "Teilüberschwemmung","Feuerrohbau","Konditionsdifferenzdeckung",
              "Marktgarantie")

je_long <- df2 %>%
  select(VSNR_V, starts_with("JE ")) %>%
  pivot_longer(starts_with("JE "),
               names_to = "gefahr", values_to = "exposure_years",
               names_prefix = "JE ")

# Schadenanzahl/-hoehe je Gefahr (nur falls SQL-Variante A/B umgesetzt)
if (any(grepl("^Anzahl Schäden ", names(df2)))) {
  cnt_long <- df2 %>%
    select(VSNR_V, starts_with("Anzahl Schäden ")) %>%
    pivot_longer(starts_with("Anzahl Schäden "),
                 names_to = "gefahr", values_to = "claim_count",
                 names_prefix = "Anzahl Schäden ")
  sev_long <- df2 %>%
    select(VSNR_V, starts_with("Durchschnittsschaden ")) %>%
    pivot_longer(starts_with("Durchschnittsschaden "),
                 names_to = "gefahr", values_to = "avg_claim",
                 names_prefix = "Durchschnittsschaden ")
  peril_df <- je_long %>%
    left_join(cnt_long, by = c("VSNR_V","gefahr")) %>%
    left_join(sev_long, by = c("VSNR_V","gefahr")) %>%
    mutate(
      claim_count = coalesce(claim_count, 0),
      loss        = coalesce(claim_count, 0) * coalesce(avg_claim, 0)
    ) %>%
    filter(!is.na(exposure_years), exposure_years > 0)   # nur aktive Deckungen
} else {
  peril_df <- je_long %>% filter(!is.na(exposure_years), exposure_years > 0)
  warning("Schadenspalten je Gefahr fehlen - peril_df enthaelt nur Exposure.")
}


# =============================================================================
# [NEU]  A-14 | Buehlmann-Straub-Credibility  (Buch, Kapitel 4 -> Kapitel 7)
# -----------------------------------------------------------------------------
# Eindimensionale BS-Credibility je Gefahr. Risiko i = z.B. ZUERS-Zone,
# Bauartklasse o.ae.; Beobachtung X_i = Burning Cost = Schadenaufwand / JE;
# Gewicht w_i = Summe der Jahreseinheiten (JE) dieser Gefahr.
#
#   B_i  = sum_j w_ij * X_ij / w_i.            (individueller, gewichteter Mittelwert)
#   mu^  = sum_i w_i. * B_i / sum_i w_i.       (Kollektivmittel)
#   sigma^2 (erwartete Within-Varianz) und tau^2 (Between-Varianz) -> s. unten
#   alpha_i = w_i. / ( w_i. + sigma^2 / tau^2 )
#   Credibility-Schaetzer:  P_i^cred = alpha_i * B_i + (1 - alpha_i) * mu^
# =============================================================================
buehlmann_straub <- function(risk, weight, observation) {
  d <- data.frame(risk = risk, w = weight, X = observation) |>
    dplyr::filter(is.finite(w), w > 0, is.finite(X))

  agg <- d |>
    dplyr::group_by(risk) |>
    dplyr::summarise(w_i = sum(w),
                     B_i = sum(w * X) / sum(w),
                     .groups = "drop")

  w_tot <- sum(agg$w_i)
  mu    <- sum(agg$w_i * agg$B_i) / w_tot                  # Kollektivmittel

  # erwartete Within-Varianz sigma^2 (gewichteter MSE innerhalb der Risiken)
  ss_within <- d |>
    dplyr::left_join(agg, by = "risk") |>
    dplyr::summarise(s = sum(w * (X - B_i)^2)) |>
    dplyr::pull(s)
  I_eff  <- nrow(agg)
  n_obs  <- nrow(d)
  sigma2 <- ss_within / (n_obs - I_eff)

  # Between-Varianz tau^2 (klassischer BS-Schaetzer, auf >= 0 gekappt)
  ss_between <- sum(agg$w_i * (agg$B_i - mu)^2) - (I_eff - 1) * sigma2
  c_const    <- w_tot - sum(agg$w_i^2) / w_tot
  tau2       <- max(ss_between / c_const, 0)

  agg |>
    dplyr::mutate(
      sigma2  = sigma2,
      tau2    = tau2,
      alpha   = if (tau2 > 0) w_i / (w_i + sigma2 / tau2) else 0,
      mu_coll = mu,
      P_cred  = alpha * B_i + (1 - alpha) * mu          # Credibility-Praemie
    )
}

# Beispielanwendung: Burning Cost Leitungswasser je ZUERS-Zone
lw <- peril_df |>
  dplyr::filter(gefahr == "Leitungswasser") |>
  dplyr::left_join(dplyr::select(df2, VSNR_V, ZUERSZONE_WGB), by = "VSNR_V") |>
  dplyr::mutate(burning_cost = loss / exposure_years)

cred_lw <- buehlmann_straub(
  risk        = lw$ZUERSZONE_WGB,
  weight      = lw$exposure_years,     # <- Gewicht = Jahreseinheiten
  observation = lw$burning_cost
)
print(cred_lw)


# =============================================================================
# [NEU - optional]  A-15 | Mehrdimensionale Sicht (Kapitel 7)
# -----------------------------------------------------------------------------
# Sollen mehrere Gefahren GEMEINSAM geschaetzt werden (mehrdim. Buehlmann-
# Straub), wird je Risiko i ein Beobachtungsvektor X_i = (BC_Gefahr1, ...)
# mit zugehoerigem Gewichtsvektor w_i = (JE_Gefahr1, ...) gebildet.
# peril_df liefert dafuer die korrekte Datenstruktur. Eine fertige
# Implementierung bietet das Paket 'actuar' (Funktion cm(), formula = ~zone,
# Argumente 'ratios' und 'weights'); siehe Bericht, Abschnitt 4.3.
# =============================================================================
# library(actuar)
# wide_X <- peril_df %>% ... pivot_wider(... burning_cost ...)
# wide_w <- peril_df %>% ... pivot_wider(... exposure_years ...)
# fit <- cm(~risk, data = ..., ratios = ..., weights = ...)