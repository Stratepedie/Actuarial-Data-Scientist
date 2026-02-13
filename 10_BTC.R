# ───────────────────────────────────────────────────────────────
#   Bitcoin historical data from CoinGecko + naive 30-day forecast
#   (as of February 2026 example – adjust dates if needed)
# ───────────────────────────────────────────────────────────────

# 1. Install missing packages if you don't have them yet
if (!require(httr))       install.packages("httr")
if (!require(jsonlite))   install.packages("jsonlite")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(dplyr))      install.packages("dplyr")
if (!require(lubridate))  install.packages("lubridate")

library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)

# ──── 2. Download BTC/USD daily data from CoinGecko (last 3650 days ≈ 10 years)
coin_id     <- "bitcoin"
vs_currency <- "usd"
days        <- "max"          # or numeric e.g. 3650

url <- paste0(
  "https://api.coingecko.com/api/v3/coins/", coin_id,
  "/market_chart?vs_currency=", vs_currency,
  "&days=", days, "&interval=daily&precision=2"
)

cat("Fetching data from CoinGecko...\n")
response <- GET(url)

if (status_code(response) != 200) {
  stop("Error: Could not fetch data. Status: ", status_code(response))
}

data_raw <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Convert to clean data frame
df <- data.frame(
  date     = as.POSIXct(data_raw$prices[,1]/1000, origin = "1970-01-01", tz = "UTC"),
  price    = data_raw$prices[,2],
  volume   = data_raw$total_volumes[,2],
  mcap     = data_raw$market_caps[,2]
) |>
  mutate(
    date     = as.Date(date),
    log_price = log(price + 1e-6)   # avoid log(0) edge case
  ) |>
  arrange(date)

cat("Data range:", range(df$date), "\n")
cat("Number of days:", nrow(df), "\n\n")

# ──── 3. Quick plot – log scale usually better for BTC
p <- ggplot(df, aes(date, price)) +
  geom_line(color = "#f7931a", linewidth = 0.9) +
  scale_y_log10(labels = scales::dollar) +
  labs(
    title = "Bitcoin Price – logarithmic scale (CoinGecko data)",
    subtitle = paste("From", min(df$date), "to", max(df$date)),
    y = "Price (USD, log scale)",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p)

# ──── 4. Very naive linear trend on log(price) → exponential growth assumption
df$days_since_start <- as.numeric(df$date - min(df$date))

model <- lm(log_price ~ days_since_start, data = df)

summary(model)

# Last known price & date
last_date  <- max(df$date)
last_price <- tail(df$price, 1)

cat("\nLast closing price:", dollar(last_price), "on", as.character(last_date), "\n")

# Predict next 30 days
future_days <- 1:30
future_df <- data.frame(
  days_since_start = as.numeric(last_date - min(df$date)) + future_days
)

pred_log <- predict(model, newdata = future_df, interval = "confidence")
future_df$pred_price     <- exp(pred_log[,"fit"])
future_df$pred_lower     <- exp(pred_log[,"lwr"])
future_df$pred_upper     <- exp(pred_log[,"upr"])
future_df$date           <- last_date + future_days

# ──── 5. Plot history + naive forecast
p_forecast <- ggplot() +
  geom_line(data = df, aes(date, price), color = "#f7931a", linewidth = 0.8) +
  geom_ribbon(data = future_df, aes(x = date, ymin = pred_lower, ymax = pred_upper),
              fill = "#f7931a", alpha = 0.15) +
  geom_line(data = future_df, aes(date, pred_price), color = "#00ba38", linewidth = 1.1, linetype = "dashed") +
  scale_y_log10(labels = scales::dollar) +
  labs(
    title = "Bitcoin – Historical + Naive 30-day log-linear extrapolation",
    subtitle = "⚠️ This is NOT a serious prediction – just a toy linear trend on log(price)",
    y = "Price (USD, log scale)",
    x = NULL,
    caption = "Data: CoinGecko • Model: lm(log(price) ~ days)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(p_forecast)

# ──── 6. Print point estimates for next 30 days (select some dates)
cat("\nNaive 30-day point forecasts (log-linear trend):\n\n")
future_df |>
  filter(days_since_start %% 5 == 0 | row_number() == n()) |>   # every ~5 days + last day
  select(date, pred_price, pred_lower, pred_upper) |>
  mutate(across(where(is.numeric), ~round(., 0))) |>
  print(n = 20)