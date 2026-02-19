library(dplyr)
library(zoo)
library(lubridate)   # just in case

library(readxl)

df <- read_excel(
  "coin_metric_date.xlsx",
  col_names = c("date", "price")
)

df <- df %>%
  mutate(
    date  = ymd(date),
    price = as.numeric(price)
  )
df <- df %>%
  mutate(
    price = case_when(
      date == as.Date("2017-05-10") & price > 1000000 ~ price / 1000,
      price > 500000 ~ NA_real_,
      TRUE ~ price
    )
  ) %>%
  filter(!is.na(price)) %>%
  arrange(date)

summary(df$price)
df

print(head(df, 2))

print(tail(df, 2))

df <- df %>%
  arrange(date) %>%
  mutate(
    price_2yr_avg = zoo::rollapply(
      price,
      width = 731,                     # ~2 years of daily data (2-year moving average)
      FUN = function(x) mean(x, na.rm = TRUE),
      fill = NA,
      align = "right"
    )
  )
df_2016_2024 <- df %>%
  filter(date >= as.Date("2016-01-01"),
         date <= as.Date("2024-12-31"))
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


period_averages <- df %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  )
period_averages

#as we can the 2 yrs av Price for the first 3 peiodes matches exactly what we have on the picture. 
#This may be because by the time the wrote The paper the historical data for this 3 periods were available
#The 2 year average price thereafter was mainling based on prediction for more than the 3/4 of the periode because the 
#paper was  publisched on the 02.06.2022  

####################################################################################################
####################################################################################################

