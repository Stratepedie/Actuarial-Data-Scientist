# =============================================================================
#  Schadenauswertung EFH 2023-2025 — Automatisierte Datenaufbereitung
# -----------------------------------------------------------------------------
#  Was macht das Skript?
#    1. Liest Buchungsliste_2 + Buchungsliste_3 ein und hängt sie zusammen
#       -> Buchung_gesamt
#    2. Liest die jährlichen EFH-Stammdaten (EFH20231231/24/25) ein
#    3. Ergänzt in Buchung_gesamt + Schadenliste die Spalte "Tarifausprägung"
#       per Year-aware Join über NUMMER_GESAMTVERTRAG_MV
#       -> 2023er Buchungen ziehen aus EFH20231231, 2024er aus EFH20241231 usw.
#    4. Wo kein Treffer: "Null" eintragen
#    5. Datenqualitäts-Check (NA / leer / #NV pro Spalte)
#    6. PivotTabellen Tarifausprägung × YEAR_ID
#    7. Alles als Excel + CSV exportieren
#
#  Wartung beim nächsten Quartal:
#    -> Nur den Block EINSTELLUNGEN unten anpassen, sonst nichts.
# =============================================================================


# -----------------------------------------------------------------------------
# 0) Pakete (einmalig installieren falls noch nicht vorhanden)
# -----------------------------------------------------------------------------
# install.packages(c("data.table", "openxlsx"))

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})


# =============================================================================
#  EINSTELLUNGEN  --  HIER UND NUR HIER ÄNDERN, WENN SICH DIE QUELLE ÄNDERT
# =============================================================================

# Pfad zu den CSV-Dateien (laut Screenshot):
quelle_pfad <- "O:/Produktmanagement/Aktuariat/Gesellschaftsgespräche/2026/NN Group/2026-04 Schadenauswertung EFH 2023-2025/R_Daten"

# Dateinamen (ohne .csv-Endung — die hängt das Skript selbst an):
datei_buchungsliste_2 <- "Buchungsliste_2"
datei_buchungsliste_3 <- "Buchungsliste_3"
datei_schadenliste    <- "Schadenliste_3"   # heißt im Excel-Sheet "Schadenliste_4"

# EFH-Stammdaten je Jahr:
efh_dateien <- list(
  "2023" = "EFH20231231",
  "2024" = "EFH20241231",
  "2025" = "EFH20251231"
)

# Header-Zeilen (1-basiert):
#   Buchungs-/Schadenlisten: Zeile 1 = Legende, Zeile 2 = Produkt-Sparte,
#                            Zeile 3 = Spaltennamen, ab Zeile 4 = Daten
#   EFH-Dateien:             Zeile 1 = Spaltennamen, ab Zeile 2 = Daten
header_zeile_buchung <- 3L
header_zeile_schaden <- 3L
header_zeile_efh     <- 1L

# CSV-Format (deutsches Excel: ; als Trenner, , als Dezimalkomma):
csv_separator <- ";"
csv_dezimal   <- ","
csv_encoding  <- "Latin-1"   # Windows-1252, wie Excel speichert

# Schlüsselspalte für den Join:
schluessel_spalte <- "NUMMER_GESAMTVERTRAG_MV"

# Spalte mit der Tarifausprägung in den EFH-Dateien
# (Schreibweise kann variieren — Skript probiert alle durch):
mögliche_efh_tarifspalten <- c("Tarifausprägung",
                               "Tarifauspraegung",
                               "Art des Schutzes",
                               "Art_des_Schutzes",
                               "ART_DES_SCHUTZES",
                               "TARIFAUSPRAEGUNG")

# Ausgabeverzeichnis (wird angelegt falls nicht vorhanden):
ausgabe_pfad <- file.path(quelle_pfad, "R_Output")
if (!dir.exists(ausgabe_pfad)) dir.create(ausgabe_pfad, recursive = TRUE)


# =============================================================================
#  AB HIER GENERISCHER CODE  --  NORMALERWEISE NICHTS ÄNDERN
# =============================================================================


# -----------------------------------------------------------------------------
# 1) Helferfunktionen
# -----------------------------------------------------------------------------

# Liest eine CSV mit n Schrottzeilen oberhalb des Headers ein.
csv_einlesen <- function(pfad_ohne_endung, header_zeile,
                         sep = csv_separator,
                         dec = csv_dezimal,
                         encoding = csv_encoding) {
  
  pfad <- paste0(pfad_ohne_endung, ".csv")
  if (!file.exists(pfad)) stop("Datei nicht gefunden: ", pfad)
  
  message(">> Lese ", basename(pfad), " ...")
  zeit <- system.time(
    dt <- fread(
      pfad,
      sep         = sep,
      dec         = dec,
      header      = TRUE,
      skip        = header_zeile - 1L,    # n Zeilen oberhalb des Headers überspringen
      encoding    = encoding,
      na.strings  = c("", "NA", "#NV", "#N/A", "NULL", "null"),
      check.names = FALSE,
      showProgress = FALSE
    )
  )
  message(sprintf("   %s Zeilen, %s Spalten (%.1f s)",
                  format(nrow(dt), big.mark = "."),
                  ncol(dt),
                  zeit["elapsed"]))
  dt
}

# Findet die Tarifausprägungs-Spalte in einer EFH-Datei robust gegenüber
# Schreibvarianten.
finde_tarifspalte <- function(dt, kandidaten = mögliche_efh_tarifspalten) {
  treffer <- intersect(kandidaten, names(dt))
  if (length(treffer) == 0) {
    stop("Keine Tarifausprägungs-Spalte in EFH-Datei gefunden.\n",
         "  Erwartet: ", paste(kandidaten, collapse = ", "), "\n",
         "  Vorhanden: ", paste(names(dt), collapse = ", "))
  }
  treffer[1]
}

# Datenqualitäts-Check: Zählt je Spalte NA / leer / #NV und gibt betroffene
# Zeilen-Indizes zurück.
qualitaet_pruefen <- function(dt, name = deparse(substitute(dt))) {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("DATENQUALITÄT: ", name, "\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
  
  zaehlung <- data.table(
    Spalte        = names(dt),
    NAs           = sapply(dt, function(x) sum(is.na(x))),
    Leer          = sapply(dt, function(x) {
      if (is.character(x)) sum(x == "" | trimws(x) == "", na.rm = TRUE) else 0L
    }),
    NV_Markierung = sapply(dt, function(x) {
      if (is.character(x)) sum(x %in% c("#NV", "#N/A"), na.rm = TRUE) else 0L
    })
  )
  zaehlung[, Gesamt_problematisch := NAs + Leer + NV_Markierung]
  print(zaehlung)
  
  betroffene_idx <- which(rowSums(is.na(dt)) > 0)
  cat(sprintf("\n%s Zeilen haben in mind. einer Spalte einen NA-Wert.\n",
              format(length(betroffene_idx), big.mark = ".")))
  
  if (length(betroffene_idx) > 0) {
    cat("Erste 10 betroffene Zeilen:\n")
    print(dt[head(betroffene_idx, 10)])
  }
  
  # Hinweis für die fachliche Klärung:
  cat("\nHINWEIS für die fachliche Klärung:\n")
  cat("  - Liste der Verträge ohne Tarifausprägung wurde als CSV exportiert.\n")
  cat("  - Nach Klärung gezielt nachtragen z.B. mit:\n")
  cat("       buchung_gesamt[NUMMER_GESAMTVERTRAG_MV == 'XXX' & YEAR_ID == 2024,\n")
  cat("                      Tarifausprägung := 'Standard']\n")
  
  invisible(list(zaehlung = zaehlung, betroffene_zeilen_idx = betroffene_idx))
}

# Hilfsfunktion: Text mit deutscher Zahlenformatierung -> numerisch.
#   "1.234,56 €"  ->  1234.56
text_zu_zahl <- function(x) {
  if (!is.character(x)) return(x)
  as.numeric(gsub(",", ".", gsub("\\.", "", gsub("\\s|€", "", x))))
}


# -----------------------------------------------------------------------------
# 2) Buchungslisten einlesen und untereinander hängen -> Buchung_gesamt
# -----------------------------------------------------------------------------

bl2 <- csv_einlesen(file.path(quelle_pfad, datei_buchungsliste_2),
                    header_zeile_buchung)
bl3 <- csv_einlesen(file.path(quelle_pfad, datei_buchungsliste_3),
                    header_zeile_buchung)

# rbindlist(fill=TRUE) ist tolerant, falls eine Datei mal eine Spalte mehr/weniger hat:
buchung_gesamt <- rbindlist(list(bl2, bl3), use.names = TRUE, fill = TRUE)

cat(sprintf("\n>> Buchung_gesamt: %s Zeilen aus Buchungsliste_2 (%s) + Buchungsliste_3 (%s)\n",
            format(nrow(buchung_gesamt), big.mark = "."),
            format(nrow(bl2), big.mark = "."),
            format(nrow(bl3), big.mark = ".")))

rm(bl2, bl3); gc(verbose = FALSE)


# -----------------------------------------------------------------------------
# 3) EFH-Dateien einlesen, in EINEN Lookup bündeln (YEAR_ID + Vertragsnummer)
# -----------------------------------------------------------------------------

efh_lookup_list <- lapply(names(efh_dateien), function(jahr) {
  efh <- csv_einlesen(file.path(quelle_pfad, efh_dateien[[jahr]]),
                      header_zeile_efh)
  
  tarif_col <- finde_tarifspalte(efh)
  
  if (!schluessel_spalte %in% names(efh)) {
    stop("In EFH-Datei für Jahr ", jahr, " fehlt Spalte '", schluessel_spalte, "'.\n",
         "Vorhandene Spalten: ", paste(names(efh), collapse = ", "))
  }
  
  data.table(
    YEAR_ID                 = as.integer(jahr),
    NUMMER_GESAMTVERTRAG_MV = efh[[schluessel_spalte]],
    Tarifausprägung         = efh[[tarif_col]]
  )
})

efh_lookup <- rbindlist(efh_lookup_list, use.names = TRUE)
setkey(efh_lookup, YEAR_ID, NUMMER_GESAMTVERTRAG_MV)

cat(sprintf("\n>> EFH-Lookup gebaut: %s Einträge aus %d Jahresdateien\n",
            format(nrow(efh_lookup), big.mark = "."),
            length(efh_dateien)))

rm(efh_lookup_list); gc(verbose = FALSE)


# -----------------------------------------------------------------------------
# 4) Spalte Tarifausprägung an Buchung_gesamt anhängen (Year-aware Join)
# -----------------------------------------------------------------------------

buchung_gesamt[, YEAR_ID := as.integer(YEAR_ID)]
setkey(buchung_gesamt, YEAR_ID, NUMMER_GESAMTVERTRAG_MV)

buchung_gesamt[efh_lookup,
               Tarifausprägung := i.Tarifausprägung,
               on   = c("YEAR_ID", "NUMMER_GESAMTVERTRAG_MV"),
               mult = "first"]

# Wo kein Treffer: "Null" (Text, damit es im Pivot als eigene Kategorie erscheint)
buchung_gesamt[is.na(Tarifausprägung) | Tarifausprägung == "" | Tarifausprägung == "#NV",
               Tarifausprägung := "Null"]

cat("\n>> Tarifausprägung in Buchung_gesamt ergänzt:\n")
print(buchung_gesamt[, .N, by = Tarifausprägung][order(-N)])


# -----------------------------------------------------------------------------
# 5) Datenqualität BUCHUNG prüfen + fehlende Verträge exportieren
# -----------------------------------------------------------------------------

qa_buchung <- qualitaet_pruefen(buchung_gesamt, name = "buchung_gesamt")

fehlende_buchung <- buchung_gesamt[Tarifausprägung == "Null",
                                   .N,
                                   by = .(YEAR_ID, NUMMER_GESAMTVERTRAG_MV)]
fwrite(fehlende_buchung,
       file.path(ausgabe_pfad, "Buchung_fehlende_Tarifausprägung.csv"),
       sep = ";", dec = ",", bom = TRUE)
cat(sprintf("\n>> %s Verträge ohne Tarifausprägung exportiert.\n",
            format(nrow(fehlende_buchung), big.mark = ".")))


# -----------------------------------------------------------------------------
# 6) PivotTabelle für Buchung_gesamt
#     Zeilen   = Tarifausprägung
#     Spalten  = YEAR_ID
#     Werte    = Summe Jahresnetto
# -----------------------------------------------------------------------------

# Jahresnetto numerisch machen (CSV liefert evtl. Text mit Komma):
if (is.character(buchung_gesamt$Jahresnetto)) {
  buchung_gesamt[, Jahresnetto := text_zu_zahl(Jahresnetto)]
}

pivot_buchung <- dcast(
  buchung_gesamt,
  Tarifausprägung ~ YEAR_ID,
  value.var      = "Jahresnetto",
  fun.aggregate  = sum,
  na.rm          = TRUE,
  margins        = "Tarifausprägung"
)

cat("\n>> PivotTabelle Buchung_gesamt:\n")
print(pivot_buchung)


# =============================================================================
#  WIEDERHOLUNG VON SCHRITT 3-6 FÜR SCHADENLISTE
# =============================================================================

# -----------------------------------------------------------------------------
# 7) Schadenliste einlesen
# -----------------------------------------------------------------------------

schaden <- csv_einlesen(file.path(quelle_pfad, datei_schadenliste),
                        header_zeile_schaden)


# -----------------------------------------------------------------------------
# 8) Tarifausprägung an Schadenliste anhängen (gleicher Lookup wie oben)
# -----------------------------------------------------------------------------

schaden[, YEAR_ID := as.integer(YEAR_ID)]
setkey(schaden, YEAR_ID, NUMMER_GESAMTVERTRAG_MV)

schaden[efh_lookup,
        Tarifausprägung := i.Tarifausprägung,
        on   = c("YEAR_ID", "NUMMER_GESAMTVERTRAG_MV"),
        mult = "first"]

schaden[is.na(Tarifausprägung) | Tarifausprägung == "" | Tarifausprägung == "#NV",
        Tarifausprägung := "Null"]

cat("\n>> Tarifausprägung in Schadenliste ergänzt:\n")
print(schaden[, .N, by = Tarifausprägung][order(-N)])


# -----------------------------------------------------------------------------
# 9) Datenqualität SCHADEN prüfen + fehlende Verträge exportieren
# -----------------------------------------------------------------------------

qa_schaden <- qualitaet_pruefen(schaden, name = "Schadenliste")

fehlende_schaden <- schaden[Tarifausprägung == "Null",
                            .N,
                            by = .(YEAR_ID, NUMMER_GESAMTVERTRAG_MV)]
fwrite(fehlende_schaden,
       file.path(ausgabe_pfad, "Schaden_fehlende_Tarifausprägung.csv"),
       sep = ";", dec = ",", bom = TRUE)
cat(sprintf("\n>> %s Schaden-Verträge ohne Tarifausprägung exportiert.\n",
            format(nrow(fehlende_schaden), big.mark = ".")))


# -----------------------------------------------------------------------------
# 10) PivotTabelle für Schadenliste
#      Zeilen  = Tarifausprägung
#      Spalten = YEAR_ID
#      Werte   = Summe Reserve Gesamt   (analog Pivot_4 in der Excel-Datei)
# -----------------------------------------------------------------------------

# Numerische Spalten der Schadenliste sicher numerisch machen:
zahl_spalten_schaden <- c("Aufwand Gesamt", "Zahlung Gesamt",
                          "Reserve Gesamt", "Kosten Gesamt",
                          "Regressforderung Gesamt")
for (col in zahl_spalten_schaden) {
  if (col %in% names(schaden) && is.character(schaden[[col]])) {
    schaden[, (col) := text_zu_zahl(get(col))]
  }
}

pivot_schaden <- dcast(
  schaden,
  Tarifausprägung ~ YEAR_ID,
  value.var      = "Reserve Gesamt",
  fun.aggregate  = sum,
  na.rm          = TRUE,
  margins        = "Tarifausprägung"
)

cat("\n>> PivotTabelle Schadenliste:\n")
print(pivot_schaden)


# =============================================================================
#  EXPORT: alles in EINE Excel-Datei mit mehreren Blättern
# =============================================================================

ausgabe_xlsx <- file.path(ausgabe_pfad,
                          paste0("Auswertung_EFH_",
                                 format(Sys.Date(), "%Y%m%d"), ".xlsx"))

wb <- createWorkbook()

addWorksheet(wb, "Buchung_gesamt")
writeData(wb, "Buchung_gesamt", buchung_gesamt)

addWorksheet(wb, "Pivot_Buchung")
writeData(wb, "Pivot_Buchung", pivot_buchung)

addWorksheet(wb, "Schadenliste")
writeData(wb, "Schadenliste", schaden)

addWorksheet(wb, "Pivot_Schaden")
writeData(wb, "Pivot_Schaden", pivot_schaden)

addWorksheet(wb, "QA_Buchung")
writeData(wb, "QA_Buchung", qa_buchung$zaehlung)

addWorksheet(wb, "QA_Schaden")
writeData(wb, "QA_Schaden", qa_schaden$zaehlung)

# Falls Buchung_gesamt das Excel-Limit von 1.048.576 Zeilen sprengt -> als CSV:
if (nrow(buchung_gesamt) > 1048575) {
  removeWorksheet(wb, "Buchung_gesamt")
  csv_buchung <- file.path(ausgabe_pfad,
                           paste0("Buchung_gesamt_",
                                  format(Sys.Date(), "%Y%m%d"), ".csv"))
  fwrite(buchung_gesamt, csv_buchung, sep = ";", dec = ",", bom = TRUE)
  cat(sprintf("\n>> Buchung_gesamt zu groß für Excel (%s Zeilen) -> als CSV exportiert.\n",
              format(nrow(buchung_gesamt), big.mark = ".")))
}

saveWorkbook(wb, ausgabe_xlsx, overwrite = TRUE)
cat(sprintf("\n>> Fertige Auswertung gespeichert:\n   %s\n", ausgabe_xlsx))


# =============================================================================
#  ENDE
# =============================================================================
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Lauf erfolgreich abgeschlossen am ", format(Sys.time()), "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")