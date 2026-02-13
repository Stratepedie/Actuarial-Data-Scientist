# ───────────────────────────────────────────────────────────────
#   Bitcoin historical data from local CSV + naive 30-day linear forecast
#   File: btc-usd-max.csv (CoinGecko style export)
#   → NO log transformation – direct linear fit on price
# ───────────────────────────────────────────────────────────────

# 1. Install/load required packages
packages <- c("ggplot2", "dplyr", "lubridate", "scales")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ──── 2. Read the local CSV file
csv_file <- "btc-usd-max.csv"

if (!file.exists(csv_file)) {
  stop("CSV file not found: ", csv_file, 
       "\nCurrent working directory: ", getwd())
}

cat("Reading local CSV file:", csv_file, "...\n")

df <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE) |>
  mutate(
    snapped_at = as.POSIXct(snapped_at, format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    date       = as.Date(snapped_at),
    price      = as.numeric(price)
  ) |>
  filter(!is.na(date) & !is.na(price) & price > 0) |>
  arrange(date) |>
  select(date, price, 
         volume = total_volume, mcap = market_cap)

cat("Data loaded successfully.\n")
cat("Date range:     ", format(min(df$date), "%Y-%m-%d"), 
    " to ", format(max(df$date), "%Y-%m-%d"), "\n")
cat("Number of days: ", nrow(df), "\n")
cat("Latest price:   ", scales::dollar(tail(df$price, 1)), 
    " on ", format(max(df$date), "%Y-%m-%d"), "\n\n")

# ──── 3. Quick historical plot (normal scale)
p_hist <- ggplot(df, aes(date, price)) +
  geom_line(color = "#f7931a", linewidth = 0.8) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, scale = 1)) +
  labs(
    title    = "Bitcoin Historical Price",
    subtitle = paste("From", format(min(df$date), "%Y-%m-%d"), 
                     "to", format(max(df$date), "%Y-%m-%d")),
    y        = "Price (USD)",
    x        = NULL,
    caption  = "Analysis by Stratepedie | Source: CoinGecko (CSV export)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p_hist)

# ──── 4. Naive linear regression: price ~ time (no log)
df <- df |>
  mutate(days_since_start = as.numeric(date - min(date)))

model <- lm(price ~ days_since_start, data = df)

cat("Model summary (naive linear fit on raw price):\n")
print(summary(model))

# Last known values
last_date  <- max(df$date)
last_price <- tail(df$price, 1)

# ──── 5. Predict next 30 days + 95% confidence interval
future_days <- 1:30

future_df <- data.frame(
  days_since_start = as.numeric(last_date - min(df$date)) + future_days,
  date             = last_date + future_days
)

pred <- predict(model, newdata = future_df, interval = "confidence", level = 0.95)

future_df <- future_df |>
  mutate(
    pred_price = pred[,"fit"],
    pred_lower = pred[,"lwr"],
    pred_upper = pred[,"upr"]
  )

# ──── 6. Plot history + forecast with 95% CI ribbon
p_forecast <- ggplot() +
  geom_line(data = df, aes(date, price), color = "#f7931a", linewidth = 0.8) +
  geom_ribbon(data = future_df, aes(x = date, ymin = pred_lower, ymax = pred_upper),
              fill = "#f7931a", alpha = 0.15) +
  geom_line(data = future_df, aes(date, pred_price), 
            color = "#00ba38", linewidth = 1.1, linetype = "dashed") +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1, scale = 1)) +
  labs(
    title    = "Bitcoin Price + Naive 30-day Linear Forecast",
    subtitle = paste("Extrapolation starting from", format(last_date, "%Y-%m-%d"),
                     "with 95% confidence interval • NOT financial advice"),
    y        = "Price (USD)",
    x        = NULL,
    caption  = "Analysis by Stratepedie | Source: CoinGecko (CSV export)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p_forecast)

# ──── 7. Print selected point forecasts (with CI)
cat("\nNaive 30-day point forecasts (linear extrapolation) + 95% CI:\n\n")

future_df |>
  filter(row_number() %% 5 == 1 | row_number() == n()) |>   # ~every 5 days + last
  select(date, pred_price, pred_lower, pred_upper) |>
  mutate(
    pred_price = round(pred_price, 0),
    pred_lower = round(pred_lower, 0),
    pred_upper = round(pred_upper, 0)
  ) |>
  print(n = 20, row.names = FALSE)

#######################################################################################
# ───────────────────────────────────────────────────────────────
#   Bitcoin historical data from local CSV + naive 30-day linear forecast
#   File: btc-usd-max.csv (CoinGecko style export)
#   → NO log transformation – direct linear fit on price
# ───────────────────────────────────────────────────────────────

# 1. Install/load required packages
packages <- c("ggplot2", "dplyr", "lubridate", "scales")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ──── 2. Read the local CSV file
csv_file <- "btc-usd-max.csv"

if (!file.exists(csv_file)) {
  stop(
    "CSV file not found: ", csv_file,
    "\nCurrent working directory: ", getwd()
  )
}

cat("Reading local CSV file:", csv_file, "...\n")

df <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE) |>
  mutate(
    snapped_at = as.POSIXct(
      snapped_at,
      format = "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    date  = as.Date(snapped_at),
    price = as.numeric(price)
  ) |>
  filter(!is.na(date) & !is.na(price) & price > 0) |>
  arrange(date) |>
  select(
    date,
    price,
    volume = total_volume,
    mcap   = market_cap
  )

cat("Data loaded successfully.\n")
cat(
  "Date range:     ",
  format(min(df$date), "%Y-%m-%d"),
  " to ",
  format(max(df$date), "%Y-%m-%d"),
  "\n"
)
cat("Number of days: ", nrow(df), "\n")
cat(
  "Latest price:   ",
  scales::dollar(tail(df$price, 1)),
  " on ",
  format(max(df$date), "%Y-%m-%d"),
  "\n\n"
)

# ──── 3. Quick historical plot (normal scale)
p_hist <- ggplot(df, aes(date, price)) +
  geom_line(color = "#f7931a", linewidth = 0.8) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1)) +
  labs(
    title    = "Bitcoin Historical Price",
    subtitle = paste(
      "From",
      format(min(df$date), "%Y-%m-%d"),
      "to",
      format(max(df$date), "%Y-%m-%d")
    ),
    y        = "Price (USD)",
    x        = NULL,
    caption  = "Analysis by Stratepedie | Source: CoinGecko (CSV export)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p_hist)

# ──── 4. Naive linear regression: price ~ time (no log)
df <- df |>
  mutate(days_since_start = as.numeric(date - min(date)))

model <- lm(price ~ days_since_start, data = df)

cat("Model summary (naive linear fit on raw price):\n")
print(summary(model))

# Last known values
last_date  <- max(df$date)
last_price <- tail(df$price, 1)

# ──── 5. Predict next 30 days + 95% confidence interval
future_days <- 1:30

future_df <- data.frame(
  days_since_start = as.numeric(last_date - min(df$date)) + future_days,
  date             = last_date + future_days
)

pred <- predict(
  model,
  newdata  = future_df,
  interval = "confidence",
  level    = 0.95
)

future_df <- future_df |>
  mutate(
    pred_price = pred[, "fit"],
    pred_lower = pred[, "lwr"],
    pred_upper = pred[, "upr"]
  )

# ──── 6. Plot history + forecast with 95% CI ribbon
p_forecast <- ggplot() +
  geom_line(data = df, aes(date, price),
            color = "#f7931a", linewidth = 0.8) +
  geom_ribbon(
    data = future_df,
    aes(x = date, ymin = pred_lower, ymax = pred_upper),
    fill = "#f7931a",
    alpha = 0.15
  ) +
  geom_line(
    data = future_df,
    aes(date, pred_price),
    color = "#00ba38",
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1)) +
  labs(
    title    = "Bitcoin Price + Naive 30-day Linear Forecast",
    subtitle = paste(
      "Extrapolation starting from",
      format(last_date, "%Y-%m-%d"),
      "with 95% confidence interval • NOT financial advice"
    ),
    y        = "Price (USD)",
    x        = NULL,
    caption  = "Analysis by Stratepedie | Source: CoinGecko (CSV export)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p_forecast)

#################################
# ──── 7. Print selected point forecasts (with CI)
cat("\nNaive 30-day point forecasts (linear extrapolation) + 95% CI:\n\n")

future_df |>
  filter(row_number() %% 5 == 1 | row_number() == n()) |>
  select(date, pred_price, pred_lower, pred_upper) |>
  mutate(
    pred_price = round(pred_price, 0),
    pred_lower = round(pred_lower, 0),
    pred_upper = round(pred_upper, 0)
  ) |>
  as.data.frame() |>
  print(row.names = FALSE)

##########################################
label_df <- future_df |>
  filter(row_number() %% 5 == 1 | row_number() == n()) |>
  mutate(
    label_upper = paste0("Upper: $", format(round(pred_upper, 0), big.mark = ",")),
    label_lower = paste0("Lower: $", format(round(pred_lower, 0), big.mark = ","))
  )

###########################################
p_forecast <- p_forecast +
  # CI vertical markers
  geom_linerange(
    data = label_df,
    aes(x = date, ymin = pred_lower, ymax = pred_upper),
    color = "#00ba38",
    linewidth = 0.6,
    alpha = 0.8
  ) +
  # Upper CI labels
  geom_text(
    data = label_df,
    aes(date, pred_upper, label = label_upper),
    vjust = -0.6,
    size = 3,
    color = "#2b8cbe"
  ) +
  # Lower CI labels
  geom_text(
    data = label_df,
    aes(date, pred_lower, label = label_lower),
    vjust = 1.4,
    size = 3,
    color = "#2b8cbe"
  )


##########################################
print(p_forecast)



