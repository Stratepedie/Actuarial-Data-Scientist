# ───────────────────────────────────────────────────────────────
# Bitcoin 2-Year Average Adoption Curve – Historical + Extrapolation to 2040
# ───────────────────────────────────────────────────────────────
install.packages("randomForest")
library(dplyr)
library(zoo)
library(lubridate)
library(readxl)

# ───────────────────────────────────────────────────────────────
# 1. Load and clean data
# ───────────────────────────────────────────────────────────────

df <- read_excel("coin_metric_date.xlsx", col_names = c("date", "price"))

df <- df %>%
  mutate(
    date  = ymd(date),
    price = as.numeric(price)
  ) %>%
  mutate(
    price = case_when(
      date == as.Date("2017-05-10") & price > 1000000 ~ price / 1000,
      price > 500000 ~ NA_real_,
      TRUE ~ price
    )
  ) %>%
  filter(!is.na(price)) %>%
  arrange(date)

# Quick check
cat("Price summary after cleaning:\n")
summary(df$price)
print("First two rows:")
print(head(df, 2))
print("Last two rows:")
print(tail(df, 2))

# ───────────────────────────────────────────────────────────────
# 2. Compute 2-year rolling average
# ───────────────────────────────────────────────────────────────

df <- df %>%
  mutate(
    price_2yr_avg = zoo::rollapply(
      price,
      width = 731,               # ≈ 2 years (accounting for leap years)
      FUN   = mean,
      fill  = NA,
      align = "right"
    )
  )

# Filter to rows where rolling avg is available
hist_data <- df %>% filter(!is.na(price_2yr_avg))

# ───────────────────────────────────────────────────────────────
# 3. Period averages for annotation points
# ───────────────────────────────────────────────────────────────

df <- df %>%
  mutate(
    period = case_when(
      date >= as.Date("2016-01-01") & date < as.Date("2018-01-01") ~ "2016–2018",
      date >= as.Date("2018-01-01") & date < as.Date("2020-01-01") ~ "2018–2020",
      date >= as.Date("2020-01-01") & date < as.Date("2022-01-01") ~ "2020–2022",
      date >= as.Date("2022-01-01") & date < as.Date("2024-01-01") ~ "2022–2024",
      date >= as.Date("2024-01-01") & date < as.Date("2026-01-01") ~ "2024–2026",
      TRUE ~ NA_character_
    )
  )

period_plot <- df %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    mid_date  = as.Date(mean(as.numeric(date)), origin = "1970-01-01"),
    .groups   = "drop"
  )

# ───────────────────────────────────────────────────────────────
# 4. Log-linear extrapolation to 2040
# ───────────────────────────────────────────────────────────────

model_df <- hist_data %>%
  mutate(
    days_since_start = as.numeric(date - min(date)),
    log_avg          = log(price_2yr_avg)
  ) %>%
  filter(!is.na(log_avg))

lm_model <- lm(log_avg ~ days_since_start, data = model_df)
summary(lm_model)

# Future dates
future_dates <- seq(max(df$date) + 1, as.Date("2040-12-31"), by = "day")
future_days  <- as.numeric(future_dates - min(df$date))

pred_log   <- predict(lm_model, newdata = data.frame(days_since_start = future_days))
pred_price <- exp(pred_log)

extrap_df <- data.frame(
  date              = future_dates,
  price_2yr_avg     = pred_price
)

# Combine historical + extrapolated
full_df <- bind_rows(
  hist_data %>% select(date, price_2yr_avg),
  extrap_df
)

# ───────────────────────────────────────────────────────────────
# 5. 2040 projection & market cap
# ───────────────────────────────────────────────────────────────

price_2040 <- full_df %>%
  filter(date >= as.Date("2040-01-01")) %>%
  slice_tail(n = 1) %>%
  pull(price_2yr_avg)

circ_supply_2040 <- 20000000  # ≈ final supply
marketcap_2040   <- price_2040 * circ_supply_2040
marketcap_text   <- paste0("$", format(round(marketcap_2040 / 1e12, 1), nsmall = 1), " Trillion")

cat("\nProjected 2-year average price around 2040:", format(round(price_2040), big.mark = ","), "\n")
cat("Implied Bitcoin market cap around 2040:", marketcap_text, "\n\n")

# ───────────────────────────────────────────────────────────────
# 6. Final plot with 2040 markers
# ───────────────────────────────────────────────────────────────

plot(
  full_df$date,
  full_df$price_2yr_avg,
  type = "l",
  lwd  = 2,
  col  = "black",
  log  = "y",
  xlab = "Year",
  ylab = "Bitcoin 2-Year Average Price (log scale)",
  main = "Bitcoin Monetary Adoption (2-Year Average) – Historical + Extrapolation to 2040",
  xaxt = "n",
  ylim = c(min(full_df$price_2yr_avg, na.rm = TRUE) * 0.7,
           max(full_df$price_2yr_avg, na.rm = TRUE) * 1.8)  # extra headroom
)

# Yearly x-axis
axis(1, at = seq(min(full_df$date), max(full_df$date), by = "years"),
     labels = format(seq(min(full_df$date), max(full_df$date), by = "years"), "%Y"))

# Period average points + labels
points(period_plot$mid_date, period_plot$avg_price,
       pch = 21, bg = "lightgreen", col = "darkgreen", cex = 2.2)
text(period_plot$mid_date, period_plot$avg_price * 1.18,
     labels = paste0("$", round(period_plot$avg_price / 1000, 1), "k"),
     pos = 3, cex = 1.0)

# Paper publication reference line
abline(v = as.Date("2022-06-02"), col = "red", lwd = 2, lty = 2)

# ── 2040 vertical line ──
abline(v = as.Date("2040-01-01"), col = "#006400", lwd = 2, lty = "dashed")
text(as.Date("2040-01-01"), par("usr")[4] * 0.9,
     labels = "2040", col = "#006400", cex = 1.2, pos = 3, font = 2)

# ── 2040 horizontal line + price + market cap ──
abline(h = price_2040, col = "blue", lwd = 2, lty = "solid")

# Price label
text(par("usr")[2] * 0.98, price_2040 * 1.12,
     labels = paste0("~2040: $", format(round(price_2040 / 1e6, 1), nsmall = 1), "M"),
     col = "blue", cex = 1.1, pos = 2, font = 2)

# Market cap in Trillion (bigger & bold)
text(par("usr")[2] * 0.98, price_2040 * 0.90,
     labels = marketcap_text,
     col = "darkblue", cex = 1.4, font = 2, pos = 2)

# Reference $9.6M target from original chart
abline(h = 9.6e6, col = "#8B0000", lty = 3, lwd = 1.5)
text(as.Date("2032-01-01"), 9.6e6 * 1.25,
     "$9.6M target (ref chart)", col = "#8B0000", cex = 0.9)

# Captions
mtext("Stratepedie • Trend analysis • Historical data + log-linear extrapolation",
      side = 1, line = 4, cex = 0.85)
mtext("Market cap assumes ~19.8 million BTC circulating supply in 2040",
      side = 1, line = 5, cex = 0.75)

# Optional: save plot
# png("bitcoin_adoption_2040.png", width = 1200, height = 800, res = 120)
# ... (plot code here) ...
# dev.off()

###############################################################################
###############################################################################
# ───────────────────────────────────────────────────────────────
# Bitcoin 2-Year Average – Multi-Model Projection to 2040
# (Econometric + Machine Learning Scenarios)
# ───────────────────────────────────────────────────────────────

# Libraries
library(dplyr)
library(lubridate)
library(zoo)
library(readxl)
library(forecast)
library(randomForest)

# ───────────────────────────────────────────────────────────────
# 1. Load & clean data
# ───────────────────────────────────────────────────────────────

df <- read_excel(
  "coin_metric_date.xlsx",
  col_names = c("date", "price")
)

df <- df %>%
  mutate(
    date  = ymd(date),
    price = as.numeric(price)
  ) %>%
  mutate(
    price = case_when(
      date == as.Date("2017-05-10") & price > 1000000 ~ price / 1000,
      price > 500000 ~ NA_real_,
      TRUE ~ price
    )
  ) %>%
  filter(!is.na(price)) %>%
  arrange(date)

# ───────────────────────────────────────────────────────────────
# 2. Compute 2-year rolling average
# ───────────────────────────────────────────────────────────────

df <- df %>%
  mutate(
    price_2yr_avg = zoo::rollapply(
      price,
      width = 731,
      FUN = mean,
      fill = NA,
      align = "right"
    )
  )

hist_data <- df %>% filter(!is.na(price_2yr_avg))

# ───────────────────────────────────────────────────────────────
# 3. Prepare modeling data
# ───────────────────────────────────────────────────────────────

model_df <- hist_data %>%
  mutate(
    t = as.numeric(date - min(date)),
    log_price = log(price_2yr_avg)
  )

future_dates <- seq(max(df$date) + 1, as.Date("2040-12-31"), by = "day")
future_t     <- as.numeric(future_dates - min(df$date))

# ───────────────────────────────────────────────────────────────
# 4. Models
# ───────────────────────────────────────────────────────────────

# 4.1 Log-linear (optimistic)
lm_lin <- lm(log_price ~ t, data = model_df)
pred_lin <- exp(predict(lm_lin, newdata = data.frame(t = future_t)))

# 4.2 Log-quadratic (slowing growth)
lm_quad <- lm(log_price ~ poly(t, 2, raw = TRUE), data = model_df)
pred_quad <- exp(predict(lm_quad, newdata = data.frame(t = future_t)))

# 4.3 ARIMA (time-series realism)
arima_fit <- auto.arima(model_df$log_price)
pred_arima <- exp(forecast(arima_fit, h = length(future_dates))$mean)

# 4.4 Random Forest (ML, conservative)
rf_fit <- randomForest(
  log_price ~ t,
  data = model_df,
  ntree = 500
)
pred_rf <- exp(predict(rf_fit, newdata = data.frame(t = future_t)))

# ───────────────────────────────────────────────────────────────
# 5. Combine projections
# ───────────────────────────────────────────────────────────────

proj_df <- data.frame(
  date = future_dates,
  LogLinear = pred_lin,
  LogQuadratic = pred_quad,
  ARIMA = as.numeric(pred_arima),
  RandomForest = pred_rf
)

# ───────────────────────────────────────────────────────────────
# 6. Plot
# ───────────────────────────────────────────────────────────────

plot(
  hist_data$date,
  hist_data$price_2yr_avg,
  type = "l",
  lwd = 2,
  col = "black",
  log = "y",
  xlab = "Year",
  ylab = "Bitcoin 2-Year Average Price (log scale)",
  main = "Bitcoin Monetary Adoption – Multi-Model Projections to 2040"
)

# Model projections
lines(proj_df$date, proj_df$LogLinear, col = "blue", lwd = 2)
lines(proj_df$date, proj_df$LogQuadratic, col = "darkgreen", lwd = 2)
lines(proj_df$date, proj_df$ARIMA, col = "purple", lwd = 2)
lines(proj_df$date, proj_df$RandomForest, col = "brown", lwd = 2)

# Reference line: paper publication
abline(v = as.Date("2022-06-02"), col = "red", lwd = 2, lty = 2)

# Legend
legend(
  "topleft",
  legend = c(
    "Historical 2-Year Avg",
    "Log-linear (optimistic)",
    "Log-quadratic (slowing growth)",
    "ARIMA (time-series)",
    "Random Forest (ML)"
  ),
  col = c("black", "blue", "darkgreen", "purple", "brown"),
  lwd = c(2, 2, 2, 2, 2),
  bty = "n",
  cex = 0.9
)

# Caption
mtext(
  "Stratepedie • Trend analysis • Multiple models shown to avoid optimistic bias",
  side = 1, line = 4, cex = 0.85
)

# ───────────────────────────────────────────────────────────────
# 7. 2040 comparison table (important)
# ───────────────────────────────────────────────────────────────

proj_2040 <- proj_df %>%
  summarise(
    LogLinear_2040     = last(LogLinear),
    LogQuadratic_2040 = last(LogQuadratic),
    ARIMA_2040        = last(ARIMA),
    RandomForest_2040 = last(RandomForest)
  )

print("2040 projected 2-year average prices by model:")
print(round(proj_2040, 0))
