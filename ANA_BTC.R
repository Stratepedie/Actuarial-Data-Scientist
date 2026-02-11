# Required packages: Install if not already installed
# install.packages(c("quantmod", "forecast", "rugarch", "ggplot2", "PerformanceAnalytics", "xts", "zoo", "TTR"))
 install.packages(c("corrplot"))
# ===============================
# Libraries
# ===============================
library(quantmod)
library(forecast)
library(rugarch)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(TTR)
library(gridExtra)
library(corrplot)
# ===============================
# SECTION 1: GEOPOLITICAL ANALYSIS VISUALS
# ===============================

# 1️⃣ BTC Price + STL Decomposition
btc_df <- data.frame(
  Date  = index(btc_close),
  Price = as.numeric(btc_close)
)
ggplot(btc_df, aes(x = Date, y = Price)) +
  geom_line(color = "#FF6F61") +
  ggtitle("BTC Price (Last 1 Year)") +
  ylab("Price (USD)") +
  theme_minimal()

# STL components for BTC returns
btc_stl_df <- data.frame(
  Date = index(na.omit(btc_returns)),
  Observed = as.numeric(na.omit(btc_returns)),
  Trend = btc_ret_decomp$time.series[, "trend"],
  Seasonal = btc_ret_decomp$time.series[, "seasonal"],
  Remainder = btc_ret_decomp$time.series[, "remainder"]
)

p1 <- ggplot(btc_stl_df, aes(x = Date)) +
  geom_line(aes(y = Observed), color = "#1f77b4") + ggtitle("Observed BTC Returns") +
  theme_minimal()

p2 <- ggplot(btc_stl_df, aes(x = Date)) +
  geom_line(aes(y = Trend), color = "#ff7f0e") + ggtitle("BTC Returns Trend") +
  theme_minimal()

p3 <- ggplot(btc_stl_df, aes(x = Date)) +
  geom_line(aes(y = Seasonal), color = "#2ca02c") + ggtitle("BTC Returns Seasonal") +
  theme_minimal()

p4 <- ggplot(btc_stl_df, aes(x = Date)) +
  geom_line(aes(y = Remainder), color = "#d62728") + ggtitle("BTC Returns Remainder") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 1)

# 2️⃣ ARIMA Forecast: BTC
autoplot(btc_fc) + ggtitle("BTC 30-Day Forecast") +
  theme_minimal()

# 3️⃣ GARCH Volatility: BTC
btc_vol <- as.numeric(sigma(btc_garch))
btc_vol_df <- data.frame(
  Date = index(na.omit(btc_returns)),
  Volatility = btc_vol
)
ggplot(btc_vol_df, aes(x = Date, y = Volatility)) +
  geom_line(color = "#9467bd") +
  ggtitle("Bitcoin Daily Volatility Over Time") +
  ylab("Volatility") +
  theme_minimal()

# 4️⃣ Correlation Heatmap: BTC, Oil, Gold, SP500
# Step 1: Merge all returns into a single xts object (aligned by date)
returns_aligned <- merge(btc_returns, oil_returns, gold_returns, sp500_returns, join = "inner")

# Step 2: Remove any remaining NAs
returns_aligned <- na.omit(returns_aligned)

# Step 3: Convert to numeric matrix for correlation
returns_matrix <- coredata(returns_aligned)
colnames(returns_matrix) <- c("BTC", "Oil", "Gold", "SP500")

# Step 4: Compute correlation matrix
corr_matrix <- cor(returns_matrix)

# Step 5: Plot correlation heatmap
corrplot(
  corr_matrix,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  number.cex = 0.8,
  title = "Geopolitical Return Correlations",
  mar = c(0, 0, 1, 0)
)


# ===============================
# SECTION 2: GEOECONOMIC ANALYSIS VISUALS
# ===============================

# 1️⃣ USD Index + STL Decomposition
usd_df <- data.frame(
  Date = index(usd_close),
  Price = as.numeric(usd_close)
)
ggplot(usd_df, aes(x = Date, y = Price)) +
  geom_line(color = "#17becf") +
  ggtitle("USD Index (Last 1 Year)") +
  ylab("Index Value") +
  theme_minimal()

# 2️⃣ Gold + STL Decomposition
gold_df <- data.frame(
  Date = index(gold_close),
  Price = as.numeric(gold_close)
)
ggplot(gold_df, aes(x = Date, y = Price)) +
  geom_line(color = "#bcbd22") +
  ggtitle("Gold Price (Last 1 Year)") +
  ylab("Price (USD)") +
  theme_minimal()

# 3️⃣ USD vs BTC rolling correlation
btc_usd_aligned <- na.omit(merge(btc_returns, usd_returns, join="inner"))
rolling_corr <- rollapply(
  btc_usd_aligned, width = 30,
  FUN = function(x) cor(x[,1], x[,2]), by.column = FALSE, align = "right"
)
rolling_df <- data.frame(
  Date = index(rolling_corr),
  Corr = coredata(rolling_corr)
)
ggplot(rolling_df, aes(x = Date, y = Corr)) +
  geom_line(color = "#e377c2") +
  ggtitle("BTC–USD Rolling 30-Day Correlation") +
  ylab("Correlation") +
  theme_minimal()

# 4️⃣ Correlation Heatmap: BTC, Oil, Gold, SP500
# Step 1: Merge returns pairwise (inner join)
returns_aligned <- merge(btc_returns, oil_returns, join = "inner")
returns_aligned <- merge(returns_aligned, gold_returns, join = "inner")
returns_aligned <- merge(returns_aligned, sp500_returns, join = "inner")

# Step 2: Remove any remaining NAs (just in case)
returns_aligned <- na.omit(returns_aligned)

# Step 3: Convert to numeric matrix
returns_matrix <- coredata(returns_aligned)
colnames(returns_matrix) <- c("BTC", "Oil", "Gold", "SP500")

# Step 4: Compute correlation
corr_matrix <- cor(returns_matrix)

# Step 5: Plot correlation heatmap
corrplot(corr_matrix, method = "color", addCoef.col = "black",
         tl.col = "black", number.cex = 0.8,
         title = "Geoeconomic Return Correlations", mar=c(0,0,1,0))

###################################################################
###################################################################
# ────────────────────────────────────────────────────────────────
# Improved STL Decomposition Plot – Single faceted ggplot
# ────────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)      # for pivot_longer
library(lubridate)  # for nicer date handling

# Prepare long-format data for faceting
btc_stl_long <- btc_stl_df %>%
  pivot_longer(
    cols = c(Observed, Trend, Seasonal, Remainder),
    names_to = "Component",
    values_to = "Value"
  ) %>%
  mutate(
    Component = factor(Component, 
                       levels = c("Observed", "Trend", "Seasonal", "Remainder"),
                       labels = c("Observed Returns (The actual data you see)", 
                                  "Trend Component (Overall direction the data is moving in over a long time)", 
                                  "Seasonal Component (The repeating pattern that happens at regular intervals)", 
                                  "Remainder (Unpredictable part of the song (background noise, mistakes, one-off events)"))
  )

# Create the faceted plot
ggplot(btc_stl_long, aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Component, ncol = 1, scales = "free_y") +
  
  # Better time axis formatting
  scale_x_date(
    date_breaks = "2 months",              # or "1 month", "3 months"
    date_labels = "%b %Y",                 # → Jan 2025, Mar 2025, ...
    expand = c(0.01, 0.01)
  ) +
  
  # Y-axis: clear label + zero line for better reference
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4, linetype = "dashed") +
  
  labs(
    title    = "STL Decomposition of Bitcoin Daily Log-Returns",
    subtitle = "Last 12 months – shows trend, seasonal pattern and irregular component",
    y        = "Daily Log-Return",
    x        = NULL,   # remove redundant x title since it's dates
    caption  = "Stratepeide • Data: daily closing prices • STL decomposition (loess-based)"
  ) +
  
  # Consistent & nicer theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey50"),
    strip.text       = element_text(face = "bold", size = 11, hjust = 0),
    strip.background = element_rect(fill = "grey96", color = NA),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.spacing    = unit(1.2, "lines")   # more breathing room between facets
  ) +
  
  # Optional: component-specific colors (like your original)
  scale_color_manual(
    values = c(
      "Observed Returns (The actual data you see)"     = "#1f77b4",
      "Trend Component (Overall direction the data is moving in over a long time)"      = "#ff7f0e",
      "Seasonal Component (The repeating pattern that happens at regular intervals)"   = "#2ca02c",
      "Remainder (Unpredictable part of the song (background noise, mistakes, one-off events))" = "#d62728"
    ),
    guide = "none"   # we don't need legend because facet labels are clear
  )

# Alternative: if you prefer separate colors per panel → remove scale_color_manual
# and use geom_line(aes(color = Component)) instead

############################################################################################
############################################################################################
# ────────────────────────────────────────────────────────────────
# Improved ARIMA Forecast Plot – Bitcoin
# ────────────────────────────────────────────────────────────────

library(ggplot2)
library(forecast)    # assuming btc_fc is a forecast object
library(lubridate)   # for nicer date handling

autoplot(btc_fc, 
         # Make prediction intervals more transparent
         PI = TRUE, 
         fcol = "#FF6F61",      # point forecast color (coral red – nice for BTC)
         shaded = TRUE,
         alpha = 0.25) +        # lighter confidence bands
  
  # Better time axis formatting
  scale_x_continuous(
    breaks = seq(from = min(time(btc_fc$x)), 
                 to   = max(time(btc_fc$mean)), 
                 by   = 1/12 * 3),          # every 3 months
    labels = function(x) {
      year <- floor(x)
      month <- round((x - year) * 12) + 1
      paste(month.abb[month], year)
    }
  ) +
  
  # Or alternative: if you prefer date-based (recommended if index is Date)
  # scale_x_date(
  #   date_breaks = "3 months",
  #   date_labels = "%b %Y",
  #   expand = c(0.02, 0.02)
  # ) +
  
  # Clear labels
  labs(
    title    = "Bitcoin Price – ARIMA Forecast (Next 30 Days)",
    subtitle = paste("Historical daily closing prices + 30-day point & interval forecast • Model:", 
                     deparse(substitute(btc_fc$method))),
    x        = "Date",
    y        = "BTC Closing Price (USD)",
    caption  = "Shaded area: 80% & 95% prediction intervals • Last data point: " %+% 
      format(max(index(btc_fc$x)), "%d %b %Y")
  ) +
  
  # Visual improvements
  geom_hline(yintercept = 0, color = "grey85", linewidth = 0.4, linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, margin = margin(b = 8)),
    plot.subtitle    = element_text(color = "grey50", size = 11, margin = margin(b = 12)),
    plot.caption     = element_text(color = "grey60", size = 9, hjust = 1),
    axis.title       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    legend.position  = "bottom"
  )

# Optional: zoom in on forecast region only (if you want to focus on future)
# + coord_cartesian(xlim = c(max(time(btc_fc$x)) - 180/365, max(time(btc_fc$mean))))

#################################################################################
#################################################################################
##################################################################################
######################################################################################
autoplot(btc_fc, 
         PI = TRUE, 
         fcol = "#E74C3C", 
         alpha = 0.18) +
  
  # Monthly labels on numeric time scale
  scale_x_continuous(
    breaks = seq(
      from = floor(min(time(btc_fc$x))),
      to   = ceiling(max(time(btc_fc$mean))),
      by   = 1/12
    ),
    labels = function(x) {
      year <- floor(x)
      month_num <- round((x - year) * 12) + 1
      month_num <- pmin(pmax(month_num, 1), 12)
      paste(month.abb[month_num], year)
    },
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    title    = "Bitcoin Price Forecast – ARIMA Model (Next 30 Days)",
    subtitle = "Daily closing prices (USD) + 30-day forecast with confidence intervals",
    x        = "Date",
    y        = "BTC Price (USD)",
    caption  = paste("Data up to:", 
                     # Format last time point as approx date
                     format(as.Date("1970-01-01") + max(time(btc_fc$x)) * 365.25, 
                            "%d %b %Y"))
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey50", size = 11),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
###########################################################################
###########################################################################
# ===============================
# Optimized ARIMA Forecast Visualization – End-of-Month Focus
# ===============================

# Make sure btc_fc is already created earlier with forecast(btc_fit, h = 30)

# 1. Prepare data for better labeling
forecast_df <- data.frame(
  Date     = as.Date(time(btc_fc$mean)),
  Forecast = as.numeric(btc_fc$mean),
  Lower80  = as.numeric(btc_fc$lower[,1]),   # 80% CI
  Upper80  = as.numeric(btc_fc$upper[,1]),
  Lower95  = as.numeric(btc_fc$lower[,2]),   # 95% CI
  Upper95  = as.numeric(btc_fc$upper[,2])
)

# Add last known actual price for reference line
last_actual <- tail(btc_df$Price, 1)
last_date   <- tail(btc_df$Date, 1)

# 2. Clean, modern, communicative ggplot
p_forecast <- ggplot() +
  
  # Historical price (up to last known point)
  geom_line(data = btc_df, aes(x = Date, y = Price), 
            color = "#1f77b4", linewidth = 0.8, alpha = 0.7) +
  
  # Forecast line
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), 
            color = "#e74c3c", linewidth = 1.1, linetype = "solid") +
  
  # 80% confidence interval (lighter)
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80), 
              fill = "#e74c3c", alpha = 0.15) +
  
  # 95% confidence interval (even lighter)
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95), 
              fill = "#e74c3c", alpha = 0.08) +
  
  # Vertical line at the end of known data
  geom_vline(xintercept = as.numeric(last_date), 
             linetype = "dashed", color = "grey50", linewidth = 0.6) +
  
  # Horizontal reference line from last known price
  geom_hline(yintercept = last_actual, 
             linetype = "dashed", color = "grey50", linewidth = 0.6) +
  
  # Labels & titles – clear and communicative
  labs(
    title    = "BTC 30-Day ARIMA Forecast – End of Month Outlook",
    subtitle = paste("Last known close:", format(last_date, "%b %d, %Y"), 
                     "– $", format(round(last_actual), big.mark = ",")),
    caption  = "Forecast from ARIMA model fitted on daily closing prices (last 365 days)\nShaded areas: 80% and 95% prediction intervals",
    x        = NULL,
    y        = "BTC Price (USD)"
  ) +
  
  # Better theme & scales
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(color = "#7f8c8d", margin = margin(b = 8)),
    plot.caption  = element_text(color = "#95a5a6", size = 9, hjust = 1),
    axis.title.y  = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  ) +
  
  # Limit y-scale to avoid extreme distortion
  scale_y_continuous(labels = scales::dollar_format(scale = 1, accuracy = 1))

# Display
print(p_forecast)

# Optional: Save for X / report
ggsave("btc_arima_forecast_end_of_month.png", p_forecast, width = 9, height = 5, dpi = 320)

#########################################################################################
#########################################################################################
# Assuming btc_fc is your forecast object from forecast::forecast()
# and it contains 30 days ahead

library(ggplot2)
library(forecast)  # for autoplot

# 1. Use autoplot as base
p <- autoplot(btc_fc) + 
  ggtitle("BTC 30-Day ARIMA Forecast") +
  ggtitle("BTC’s Next 30 Days: Moonshot or Margin Call Massacre?")+
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_blank(),           # remove default x label
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )

# 2. Customize x-axis to show months only (at key points)
p + 
  scale_x_continuous(
    breaks = seq(1, 30, by = 7),              # ticks every ~week
    labels = function(x) {
      # Approximate month labels based on current date (Feb 08, 2026)
      start_date <- as.Date("2025-12-08")
      forecast_dates <- start_date + x - 1
      months_short <- format(forecast_dates, "%b")   # e.g. Feb, Mar
      years <- format(forecast_dates, "%y")
      
      # Show month + year only when month changes
      ifelse(duplicated(months_short), months_short, paste(months_short, years, sep = "'"))
    },
    expand = c(0.02, 0.02)
  ) +
  labs(
    y = "BTC Price (USD)",
    caption = "Stratepedie • Forecast for: Feb 2026"
  )
################################################################################
################################################################################

# 1. Base autoplot + theme & titles
p <- autoplot(btc_fc,          # ← replace with btc_fc_long if you extended it
              PI = TRUE, 
              fcol = "#E74C3C",  # nice red for forecast line
              alpha = 0.20) +    # soft confidence bands
  
  ggtitle("BTC’s Next 6 Months: Moonshot or Margin Call Massacre?") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, color = "grey50", hjust = 0.5),
    axis.title.x     = element_blank(),          # clean look
    axis.title.y     = element_text(margin = margin(r = 12), face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3)
  )

# 2. Customize x-axis to show months only (Feb '26 → Aug '26)
p +
  scale_x_continuous(
    # Roughly daily scale → breaks ≈ every 30 days (monthly)
    breaks = seq(0, 180, by = 30),  # 0 = today-ish, up to ~180 days
    
    labels = function(x) {
      # Start from early February 2026
      start_date <- as.Date("2026-02-08")
      plot_dates <- start_date + x
      
      # Format: short month + short year when month changes
      months_short <- format(plot_dates, "%b")
      years_short  <- format(plot_dates, "'%y")
      
      # Only show month + year on major changes, otherwise just month
      ifelse(duplicated(months_short) & !grepl("01", format(plot_dates, "%d")),
             months_short,
             paste(months_short, years_short, sep = " "))
    },
    
    expand = expansion(mult = c(0.02, 0.08))  # more space on right for future
  ) +
  
  # Clear labels & caption
  labs(
    y        = "BTC Price (USD)",
    caption  = "Stratepedie • ARIMA Forecast • Feb 08, 2026 – Aug 2026 • Shaded: 80%/95% intervals • Not financial advice"
  ) +
  
  # Optional: add subtle today line
  geom_vline(xintercept = 0, color = "grey60", linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = 5, y = max(btc_fc$x, na.rm = TRUE) * 1.02, 
           label = "Today", color = "grey50", size = 3.5, hjust = 0)
