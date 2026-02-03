# ────────────────────────────────────────────────────────────────
#   IBLI Marsabit Household Survey – Visualization Code (Round 5 focus)
#   Actualized: February 2026 – FIXED VERSION
# ────────────────────────────────────────────────────────────────

# 1. Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# 2. Working directory is already correct (your Desktop folder)
# The CSVs are inside the subfolder → keep using full relative path

# 3. Load CSV files from subfolder
s15a <- read.csv("IBLIData_CSV_PublicZipped/S15A Groups, IBLI, HSNP.csv")
s15b <- read.csv("IBLIData_CSV_PublicZipped/S15B IBLI Contracts.csv")
s15c <- read.csv("IBLIData_CSV_PublicZipped/S15C Indemnity Spending.csv")
s16 <- read.csv("IBLIData_CSV_PublicZipped/S16 Experiment on Risk Preference.csv")
s17 <- read.csv("IBLIData_CSV_PublicZipped/S17 Household Feedback.csv")
s1 <- read.csv("IBLIData_CSV_PublicZipped/S1 Household Information.csv")
s0a <- read.csv("IBLIData_CSV_PublicZipped/S0A Household Identification information.csv")
loc <- read.csv("IBLIData_CSV_PublicZipped/HH_location_shifted.csv")
s5b <- read.csv("IBLIData_CSV_PublicZipped/S5B Child Health.csv")

# 4. Filter round 5 and prepare data

s0a_r5  <- s0a  %>% filter(round == 5)
s1_r5   <- s1   %>% filter(round == 5)
s15a_r5 <- s15a %>% filter(round == 5)
s15b_r5 <- s15b %>% filter(round == 5)
s15c_r5 <- s15c %>% filter(round == 5)
s16_r5  <- s16  %>% filter(round == 5)
s17_r5  <- s17  %>% filter(round == 5)
loc_r5  <- loc  %>% filter(round == 5)

# Coerce spending columns to numeric
s15c_r5 <- s15c_r5 %>%
  mutate(across(c(s15q47c, s15q47g, s15q47h, s15q47i, s15q47k, s15q47l,
                  s15q53aI, s15q53bI, s15q53cU),
                ~ suppressWarnings(as.numeric(as.character(.x)))))

# Aggregate spending using Ind_ID as category label
s15c_agg <- s15c_r5 %>%
  rowwise() %>%
  mutate(total_amount = sum(c_across(c(s15q47c, s15q47g, s15q47h, s15q47i, s15q47k,
                                       s15q47l, s15q53aI, s15q53bI, s15q53cU)),
                            na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(hhid, Ind_ID) %>%
  summarise(total_spend = sum(total_amount, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Ind_ID, values_from = total_spend, values_fill = 0)

# Fix child weight: convert to numeric BEFORE mean
s5b <- s5b %>%
  mutate(s5q16 = suppressWarnings(as.numeric(as.character(s5q16))))

child_agg <- s5b %>%
  filter(round == 5) %>%
  group_by(hhid) %>%
  summarise(avg_child_weight = mean(s5q16, na.rm = TRUE), .groups = "drop")

# 5. Main merge
full_r5 <- s15a_r5 %>%
  full_join(s15b_r5,  by = c("hhid", "round")) %>%
  full_join(s15c_agg, by = "hhid") %>%
  full_join(s16_r5,   by = c("hhid", "round")) %>%
  full_join(s17_r5,   by = c("hhid", "round")) %>%
  full_join(s1_r5,    by = c("hhid", "round")) %>%
  full_join(s0a_r5,   by = c("hhid", "round")) %>%
  full_join(loc_r5,   by = c("hhid", "round")) %>%
  full_join(child_agg, by = "hhid")

# Derived variables
full_r5 <- full_r5 %>%
  mutate(
    bought_ibli       = if_else(ibli_purchase_count > 0, "Yes", "No", missing = "No") %>% factor(),
    risk_score        = as.numeric(s16q1),
    aware_of_IBLI     = factor(ibli_aware, levels = c("Yes", "No")),
    received_payout   = factor(ibli_payout_yn, levels = c("Yes", "No")),
    settlement_type   = factor(s1q3, levels = c("Fully settled", "Partially settled", "Nomadic")),
    ethnic_group      = as.factor(hh_head_ethnic_group)
  )

# ────────────────────────────────────────────────────────────────
# 6. Visualizations – 4 plots
# ────────────────────────────────────────────────────────────────

# Viz 1: Risk preference vs IBLI purchase (ties to "The Human Side")
ggplot(full_r5 %>% filter(!is.na(risk_score), !is.na(bought_ibli)),
       aes(x = bought_ibli, y = risk_score)) +
  geom_boxplot(fill = "#a6cee3") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2, colour = "#1f78b4") +
  labs(title = "Risk Preference vs Index Based Livestock Insurance (IBLI) Purchase",
       subtitle = "Higher = more risk-seeking | Behavioral demand insights",
       x = "Purchased IBLI?", y = "Risk Choice (0 safe – 5 risky)") +
  theme_minimal(base_size = 14)
----------------
# Viz 2: IBLI Awareness by settlement type (ties to "Real-World Wins")

  # Round 5 – Ties to up-scaling in developing countries
  ggplot(full_r5,
         aes(x = fct_explicit_na(settlement_type),
             fill = fct_explicit_na(aware_of_IBLI))) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2", na.value = "grey70", direction = 1) +
  labs(title = "Awareness of Index-Based Livestock Insurance (IBLI) by Settlement Type",
       subtitle = "Round 5 – Marsabit Pastoral Households (2013)",
       x = "Settlement Type (incl. NA)",
       y = "Number of Households",
       fill = "Aware of IBLI? (incl. NA)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))  
# Viz 3: Premium vs Coverage, colored by payout (ties to "Kenyan Cowboy Story")

full_r5 <- full_r5 %>%
  mutate(s15q19_2 = suppressWarnings(as.numeric(s15q19_2))) %>%
  filter(!is.na(s15q19_2), !is.na(s15q22))

# Now create the plot
ggplot(full_r5, aes(x = s15q19_2, y = s15q22, color = received_payout)) +
  geom_point(size = 2, alpha = 0.7) +  # Reduced point size for clarity
  scale_color_manual(values = c("Yes" = "#33a02c", "No" = "#e31a1c")) +
  scale_x_continuous(
    breaks = seq(min(full_r5$s15q19_2), max(full_r5$s15q19_2), by = 5000),  # Adjust the 'by' value for spacing
    labels = scales::comma  # Format labels as commas (optional, for clarity)
  ) +
  labs(
    title = "IBLI Premium vs Animals Covered",
    subtitle = "Points colored by payout received | NDVI triggers in Marsabit",
    x = "Premium (in Kenyan Shillings - KES)",
    y = "Animals Covered",
    color = "Payout Received?"
  ) +
  theme_minimal(base_size = 10) +  # Smaller base size for a compact look
  theme(
    legend.position = "bottom",  # Move legend to bottom
    legend.title = element_text(size = 8),  # Adjust legend title size
    legend.text = element_text(size = 7),  # Adjust legend text size
    axis.text = element_text(size = 8),  # Smaller axis text
    axis.title = element_text(size = 9),  # Slightly smaller axis titles
    plot.title = element_text(size = 11, face = "bold"),  # Make title slightly smaller and bold
    plot.subtitle = element_text(size = 9)  # Smaller subtitle
  )


ggplot(full_r5 %>%
         mutate(s15q19_2 = suppressWarnings(as.numeric(s15q19_2))) %>%
         filter(!is.na(s15q19_2), !is.na(s15q22)),
       aes(x = s15q19_2, y = s15q22)) +
  geom_point(size = 3, alpha = 0.7, color = "#1f78b4") +
  labs(
    title = "IBLI Premium Paid vs Animals Covered / Tropical Livestock Units (TLU) (Round 5)",
    subtitle = "No payout data available in this round",
    x = "Premium ((in Kenyan Shillings - KES)",
    y = "Animals / TLU Covered"
  ) +
  theme_minimal(base_size = 12)









