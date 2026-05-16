################################################################################################
## Product Management: Pricing
## Autor: Stratepedie
## Objectif: Discussing pricing techniques in insurance when damage/loss histories are unavailable.
################################################################################################

# ── Packages ─────────────────────────────────────────────
install.packages(c("recipes", "keras", "gridExtra","catboost","lightgbm","xgboost","caret","Metrics"))
install.packages("catboost")
library(tidyverse)
library(tidygeocoder)
library(dplyr)
library(gridExtra)
library(stringr)
library(purrr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(corrplot)
library(patchwork)
library(magick)
library(recipes)        # feature engineering / transformations
library(keras)          # neural networks
library(catboost)       # CatBoost
library(lightgbm)       # LightGBM
library(xgboost)        # XGBoost
library(caret)          # for grid/random search
library(Metrics)        # rmse

#install.packages('devtools')
#devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#-------------------------------------------------------------------------------------
# Data quality control and data cleaning
#-------------------------------------------------------------------------------------
  
#We use the standard California Housing dataset
#We begin by importing the given dataset. Then, report the total number of rows and columns and output seven randomly chosen records.
contract <- read_csv("housing_with_county_and_sample.csv")
set.seed(2026)
# Display summary info
cat("Dataset overview:\n")
cat(sprintf("Rows:    %d\n", nrow(contract)))
cat(sprintf("Columns: %d\n", ncol(contract)))
cat("\nRandom sample of 7 rows:\n")
contract %>%
  slice_sample(n = 7) %>%
  print()
#understanding the structure of the dataset
str(contract)
sapply(contract, class)
sapply(contract, function(x) is.numeric(x) && any(x %% 1 != 0, na.rm = TRUE))
any(is.na(contract)) # the file contrat has missing value
cat(sprintf("Contract has the following number of missing numbers:    %d\n", sum(is.na(contract)))) # and 210 missing value
missing_per_col <- colSums(is.na(contract))
for (col_name in names(missing_per_col)) {
  cat(sprintf("Column %-20s has %d missing values\n", col_name, missing_per_col[col_name]))
}
#With the exception of ocean_proximity, county_name, and sample, all features were identified as numeric.
#The variables latitude, longitude, and median_income contain decimal values. The column total_bedrooms has 207 missing value
#and the column has 3 missing value.

#The data are suitable for partitioning into training, validation, and test sets.
table(contract$sample)
# How many rows (observations) do we have in total?
total_number_of_rows = nrow(contract)
# Count how many rows are in each group (A, B, C)
number_of_group_A <- sum(contract$sample == "A")
number_of_group_B <- sum(contract$sample == "B")
number_of_group_C <- sum(contract$sample == "C")

# 3. We calculate the percentage for each group - Formula: (number in group / total rows) × 100
percent_A <- (round((number_of_group_A / total_number_of_rows) * 100))
percent_B <- (round((number_of_group_B / total_number_of_rows) * 100))
percent_C <- (round((number_of_group_C / total_number_of_rows) * 100))

print(paste("Train: ", percent_A, "%"))
print(paste("Valid: ", percent_B, "%"))
print(paste("Test:  ", percent_C,  "%"))

# We select only the numeric columns
# Select only numeric columns
numeric <- contract[sapply(contract, is.numeric)]

# Classical summary (already includes min, Q1, median, mean, Q3, max)
print("Basic summary:")
print(summary(numeric))

#----------------------------------------------------------------------------
# Outlier and extreme value analysis as well as analysis of missing values
#----------------------------------------------------------------------------
#daten <- housing_sample$total_bedrooms[!is.na(housing_sample$total_bedrooms)]
daten <- contract$total_bedrooms
median_wert <- median(daten)

par(mfrow = c(1, 2))        
par(mar = c(5, 4, 4, 2))      

# Boxplot of total_bedrooms
boxplot(daten,
        horizontal = TRUE,          
        main = "Boxplot of total_bedrooms",
        xlab = "Values of total_bedrooms",
        col = "lightblue",
        border = "darkblue",
        outpch = 19,                # points for outliers
        outcol = "gray50",          # slightly lighter color for outliers
        whisklty = 1,               # solid whisker lines
        medcol = "red",             # median line in red
        cex.main = 1.2)

# Histogram of total_bedrooms
hist(daten,
     breaks = 50,                  
     main = "Histogram of total_bedrooms with median",
     xlab = "Values of total_bedrooms",
     ylab = "Number of observations",
     col = "lightblue",
     border = "darkblue")

# Rote Linie für den Median hinzufügen
abline(v = median_wert, 
       col = "red", 
       lwd = 2,                    # dickere Linie
       lty = 2)
abline(v = median(contract$total_bedrooms, na.rm = TRUE), col = "red", lwd = 2)

#The two plots show that total_bedrooms is right-skewed and highly dispersed.
#As a result, the mean is not well suited for imputing missing values.
#To further illustrate this, the median of total_bedrooms is calculated and 
#compared with the maximum and minimum values of total_bedrooms in the observations
#where total_bedrooms is missing.

#We find all rows where total_bedrooms is missing (NA in R)
total_bedrooms_null <- contract[is.na(contract$total_bedrooms), ]

#We calculate and show the median of total_bedrooms
#(we use the whole dataset, but ignore missing values with na.rm = TRUE)
median_bedrooms <- median(contract$total_bedrooms, na.rm = TRUE)
cat("Median of total_bedrooms: ", median_bedrooms, "\n")

#find the maximum total_rooms among rows that have NO total_bedrooms
max_rooms_without_bedrooms <- max(total_bedrooms_null$total_rooms, na.rm = TRUE)
cat("Maximum of total_rooms without total_bedrooms: ", max_rooms_without_bedrooms, "\n")

#find the minimum total_rooms among rows that have NO total_bedrooms
min_rooms_without_bedrooms <- min(total_bedrooms_null$total_rooms, na.rm = TRUE)
cat("Minimum of total_rooms without total_bedrooms: ", min_rooms_without_bedrooms, "\n")

#Using the overall median to impute missing total_bedrooms would insert the same value into houses with
#very different numbers of rooms (e.g. 154 rooms vs. 11,709 rooms). In extreme cases,
#this leads to impossible situations where a house appears to have more bedrooms than total rooms.

#We find the minimum and maximum of total_rooms
min_rooms <- min(total_bedrooms_null$total_rooms, na.rm = TRUE)
max_rooms <- max(total_bedrooms_null$total_rooms, na.rm = TRUE)

#Select rows where total_rooms is either the minimum or the maximum
extreme_rows <- total_bedrooms_null[
  total_bedrooms_null$total_rooms == min_rooms |
    total_bedrooms_null$total_rooms == max_rooms,
]

# 3. Print the selected rows
print(extreme_rows)

#To evaluate the ratio of total_bedrooms to total_rooms, we first create a scatter
#plot of the two variables.

# Step 1: Create the scatter plot
plot(
  x = contract$total_rooms,
  y = contract$total_bedrooms,
  main = "Comparison of total_rooms and total_bedrooms",
  xlab = "total_rooms",
  ylab = "total_bedrooms",
  pch = 19,                    # small filled circles (like marker='.')
  cex = 0.6,                   # make points a bit smaller
  col = rgb(0, 0, 0.6, 0.5),   # dark blue with some transparency
  panel.first = grid()         # add grid behind the points
)

#In the scatter plot, a linear relationship between total_bedrooms and total_rooms is 
#clearly visible — particularly in the lower range up to approximately 10,000 total_rooms, 
#which is the most relevant area for imputation.The quotient of the two features
#is now calculated for all records with known total_bedrooms and stored in the new feature room_quotient.
#For records without total_bedrooms, room_quotient remains unassigned in the first step.

# We create room_quotient only where total_bedrooms exists
contract$room_quotient <- contract$total_bedrooms / contract$total_rooms

# We show the mean (NA values are ignored automatically)
cat("Mean room_quotient: ", 
    mean(contract$room_quotient, na.rm = TRUE), "\n")
mean_room_quotient <- mean(contract$room_quotient, na.rm = TRUE)
#Before inserting the values calculated from the median quotient, a brief plausibility check is first performed.
#To do this, the values to be imputed are additionally plotted in the scatter plot of total_rooms and total_bedrooms.

# Plot existing values (blue dots)
plot(
  x = contract$total_rooms,
  y = contract$total_bedrooms,
  pch = 19, 
  col = "darkblue",
  cex = 0.6,
  main = "Comparison of total_rooms and total_bedrooms\nwith imputed values",
  xlab = "total_rooms",
  ylab = "total_bedrooms",
  panel.first = grid(lty = "dashed", col = "gray80")
)

# Add imputed values (red crosses)
with(
  contract[is.na(contract$total_bedrooms), ],
  points(
    x = total_rooms,
    y = mean_room_quotient * total_rooms,
    pch = 19,          # +
    col = "red",
    cex = 0.6,
    lwd = 1.5
  )
)

# Add legend
legend(
  "topleft",
  legend = c("existing values", "imputed values"),
  pch = c(19, 19),
  col = c("darkblue", "red"),
  pt.cex = c(0.6, 0.6),
  bty = "n"
)
#The additional values for total_bedrooms appear to match the existing data and can now also be used.

# We fill missing bedrooms
contract$total_bedrooms[is.na(contract$total_bedrooms)] <- 
  mean_room_quotient * contract$total_rooms[is.na(contract$total_bedrooms)]
# We check if we still have missing values
cat("Still missing bedrooms:", sum(is.na(contract$total_bedrooms)), "\n")

#Since the new feature room_quotient was not calculated for the records that have missing 
#total_bedrooms, it can now be used to identify those records again.
#Because the new feature room_quotient was not populated for records without total_bedrooms,


print("Records where total_bedrooms was filled in:")

# Show total_bedrooms and total_rooms only for rows where room_quotient is missing (NA)
contract[
  is.na(contract$room_quotient),
  c("total_bedrooms", "total_rooms")
]
#It can now be used to identify those records again. Finally, room_quotient is removed from the dataframe again.
contract$room_quotient <- NULL
contract

#As part of feature engineering, instead of using raw counts (like total rooms or total bedrooms), 
#we create normalized, more meaningful features that better describe housing characteristics.
#Rooms per house and bedrooms per house capture density and living conditions more accurately than totals.
#we do this to make the data more informative, comparable, and useful for analysis or machine learning models.


# Add average bedrooms per household
contract$bedrooms_per_house <- contract$total_bedrooms / contract$households

# Add average total rooms per household
contract$rooms_per_house <- contract$total_rooms / contract$households

# Show the updated table (first 10 rows)
print("Updated data (first 10 rows):")
print(head(contract, 10))

# Show basic statistics of new columns
print("Quick statistics of new columns:")
summary(contract[, c("bedrooms_per_house", "rooms_per_house")])

########
#To support better decisions in feature engineering, understanding the statistical structure 
#of the data is very important. Histograms and kernel density estimates reveal the shape of 
#the distribution, while quantiles make outliers, spread, and central tendency immediately visible.
#Limiting the x-axis to the actual data range avoids visual distortion and ensures accurate interpretation.
#######

# Create a grid of distribution plots (hist + KDE + quantiles) for many numeric variables in the California Housing dataset

# List of variables we want to visualize
variables <- c(
  "latitude", "longitude", "housing_median_age",
  "total_rooms", "total_bedrooms", "population",
  "households", "median_income", "median_house_value",
  "bedrooms_per_house", "rooms_per_house"
)

# We create one plot per variable → store them in a list
plot_list <- list()

for (var in variables) {
  
  # Skip if the column does not exist (defensive programming)
  if (!var %in% names(contract)) {          # ← change to 'contract' if that's your data frame name
    message("Column not found: ", var)
    next
  }
  
  # Calculate important quantiles once
  q <- quantile(contract[[var]], 
                probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99), 
                na.rm = TRUE)
  
  # Create the plot
  p <- ggplot(contract, aes(x = .data[[var]])) +
    
    # Histogram (density scaled)
    geom_histogram(aes(y = after_stat(density)),
                   bins = 50, fill = "lightblue", color = "darkblue", alpha = 0.6) +
    
    # Kernel density estimate
    geom_density(color = "black", linewidth = 1.1) +
    
    # Quantile vertical lines
    geom_vline(xintercept = q["1%"],   color = "green",  linetype = "dotted",  linewidth = 0.9) +
    geom_vline(xintercept = q["5%"],   color = "green",  linetype = "dotdash", linewidth = 0.9) +
    geom_vline(xintercept = q["25%"],  color = "green",  linetype = "dashed",  linewidth = 0.9) +
    geom_vline(xintercept = q["50%"],  color = "red",    linetype = "solid",   linewidth = 1.2) +
    geom_vline(xintercept = q["75%"],  color = "purple", linetype = "dashed",  linewidth = 0.9) +
    geom_vline(xintercept = q["95%"],  color = "purple", linetype = "dotdash", linewidth = 0.9) +
    geom_vline(xintercept = q["99%"],  color = "purple", linetype = "dotted",  linewidth = 0.9) +
    
    # Reasonable x-axis range
    coord_cartesian(xlim = range(contract[[var]], na.rm = TRUE)) +
    
    labs(title = paste("Distribution of", var),
         x = var,
         y = "Density") +
    
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Store in list
  plot_list[[var]] <- p
}

# ────────────────────────────────────────────────
# Show the grid with caption AT THE BOTTOM of the whole figure
# ────────────────────────────────────────────────

# Recommended: patchwork
library(patchwork)

wrap_plots(plot_list, ncol = 3) +               
  
  plot_layout(guides = "collect") +
  
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 11)
  ) +
  
  # This places the caption BELOW everything (subplots + legend)
  plot_annotation(
    caption = paste0(
      "Stratepedie | Figure: Distribution of key variables in the California Housing dataset\n",
      "Each panel shows:",
      "  Light blue histogram (50 bins, density scaled) • Black kernel density estimate (KDE) • Quantile lines: green (1%, 5%, 25%), red (median), purple (75%, 95%, 99%).\n"
    ),
    theme = theme(
      plot.caption = element_text(
        size = 10,
        hjust = 0,                    # left-aligned
        margin = margin(t = 12, b = 8) # extra space above & below
      )
    )
  )

# === Simple Correlation Heatmap in R ===
# Goal: Visualize how strongly the numeric variables are related to each other


# ─── Safe selection: only numeric columns ───────────────────────────────
numeric_cols <- sapply(contract, is.numeric)
numeric_data  <- contract[, numeric_cols, drop = FALSE]

# Full correlation matrix (used for left plot)
full_cor <- cor(numeric_data, use = "pairwise.complete.obs")

# ─── Reduced version: drop the four specified columns ───────────────────
cols_to_drop <- c("households", "total_rooms", "total_bedrooms", "bedrooms_per_house")

# Only keep columns that are numeric AND not in the drop list
keep_cols <- numeric_cols & !(names(contract) %in% cols_to_drop)

reduced_data <- contract[, keep_cols, drop = FALSE]
reduced_cor  <- cor(reduced_data, use = "pairwise.complete.obs")

# ─── Layout: two plots side by side + space for caption at bottom ───────
par(mfrow = c(1, 2), 
    mar  = c(2, 2, 5, 2),     # more top margin for titles
    oma  = c(5, 0, 2, 0))     # outer bottom margin for caption

# ─── Left plot: full numeric correlation ────────────────────────────────
corrplot(
  full_cor,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.srt      = 45,
  tl.cex      = 0.75,
  number.cex  = 0.65,
  addCoef.col = "black",
  col         = COL2("RdBu", 10),
  title       = "Correlation Matrix of all Numeric Features",
  mar         = c(0,0,4,0)
)

# ─── Right plot: reduced (dropped columns) ──────────────────────────────
corrplot(
  reduced_cor,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.srt      = 45,
  tl.cex      = 0.75,
  number.cex  = 0.65,
  addCoef.col = "black",
  col         = COL2("RdBu", 10),
  title       = "Correlation Matrix of Reduced Numeric Features\n",
  mar         = c(0,0,4,0)
)

# ─── Caption at the very bottom (spans both plots) ──────────────────────
mtext("Stratepedie | Data: California Housing Dataset | Date: 21.02.2026", 
      side = 1, line = 2.5, outer = TRUE, 
      cex = 1.0, col = "grey30", font = 3)

# Reset layout (good practice)
par(mfrow = c(1,1), oma = c(0,0,0,0))
#building robust and generalizable models required removing highly correlated features which introduce multicollinearity
#in oder to reduces overfitting increase interpretability, training efficiency, and robustness. ensures the model is practical 
#and deployable in real-world scenarios. The features total_rooms, total_bedrooms, population, and households are highly correlated (correlation ≥ 0.86).
#A similar pattern is observed for the engineered features bedrooms_per_house and rooms_per_house (correlation ≈ 0.85)
#Keeping population and rooms_per_house is a reasonable choice, as these variables are likely the easiest to obtain when applying the trained model for future predictions.

img <- image_read("https://raw.githubusercontent.com/ageron/handson-ml3/main/images/end_to_end_project/california.png") |> 
  as.raster()

legend_guide <- guide_colorbar(
  barheight = unit(120, "pt"),
  barwidth  = unit(10, "pt"),
  title.position = "top",
  title.hjust = 0.5
)

legend_theme <- theme(
  legend.text  = element_text(size = 10),
  legend.title = element_text(size = 11),
  legend.key.height = unit(1.2, "cm")
)

gg1 <- ggplot(contract, aes(longitude, latitude, color = median_house_value)) +
  annotation_raster(img, -124.55, -113.52, 32.45, 41.95) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    guide = legend_guide
  ) +
  labs(title = "Median House Value ($)") +
  theme_minimal() +
  legend_theme

gg2 <- ggplot(contract, aes(longitude, latitude, color = median_income)) +
  annotation_raster(img, -124.55, -113.52, 32.45, 41.95) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    guide = legend_guide
  ) +
  labs(title = "Median Household Income (1 unit = $10K)") +
  theme_minimal() +
  legend_theme

gg3 <- ggplot(contract, aes(longitude, latitude, color = housing_median_age)) +
  annotation_raster(img, -124.55, -113.52, 32.45, 41.95) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    guide = legend_guide
  ) +
  labs(title = "Median Housing Age (years)") +
  theme_minimal() +
  legend_theme

(gg1 | gg2 | gg3) +
 plot_annotation(title = "Geographic Visualization – California Housing (reduced data)")

#######################

# Individual plot function
make_scatter <- function(y_var) {
  ggplot(contract,
         aes(x = median_house_value, y = .data[[y_var]])) +
    geom_point(size = 1.4, alpha = 0.7, color = "darkblue") +
    labs(y = y_var, x = if (y_var == "median_income") "Median House Value ($)" else NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9)
    )
}

# Create 6 plots
p1 <- make_scatter("median_house_value")
p2 <- make_scatter("latitude")
p3 <- make_scatter("longitude")
p4 <- make_scatter("housing_median_age")
p5 <- make_scatter("population")
p6 <- make_scatter("median_income")

# Arrange in 3 columns × 2 rows
(p1 | p2 | p3) /
  (p4 | p5 | p6) +
  
  plot_annotation(
    title = "Median House Value vs Selected Variables",
    caption = "Stratepedie | Data: California Housing Dataset | 22.02.2026",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 9, color = "grey50", hjust = 1)
    )
  ) +
  
  plot_layout(guides = "collect") &
  theme(legend.position = "none")   # no legend needed here
#############################################################################
# Training
train_mask <- contract$sample == "A"

x_train <- contract[train_mask, ] %>% 
  select(-median_house_value, -ocean_proximity, -county_name, -sample)

y_train <- contract$median_house_value[train_mask]

# Validation
val_mask <- contract$sample == "B"

x_val <- contract[val_mask, ] %>% 
  select(-median_house_value, -ocean_proximity, -county_name, -sample)

y_val <- contract$median_house_value[val_mask]


model_name <- "Modell 0 num"
model_description <- list()
model_description[[model_name]] <- "Lineares Modell mit numerischen Merkmalen"

train_data <- x_train
train_data$median_house_value <- y_train

model_0_num <- lm(median_house_value ~ latitude + longitude + housing_median_age +
                  population + median_income + rooms_per_house,
                data = train_data)

predict_a4d <- predict(model_0_num, newdata = x_val)

rmse_value <- sqrt(mean((y_val - predict_a4d)^2))
model_rmse <- list()

model_rmse[[model_name]] <- rmse_value

# Quick print
cat("Model:", model_name, "\n")
cat("RMSE (validation):", round(rmse_value, 2), "\n\n")
print(summary(model_0_num))

###################################################
# Modell-Name
model_name <- "Modell A-4d"   # oder wie du es definierst

# 1. Modelldescription (sicher anlegen)
if (!exists("model_description")) model_description <- list()
model_description[[model_name]] <- "Lineares Modell mit numerischen Merkmalen"

# 2. Trainingsdaten zusammenführen
train_data <- x_train
train_data$median_house_value <- y_train

# 3. Lineares Modell fitten
model_0_num <- lm(
  median_house_value ~ latitude + longitude + housing_median_age +
    population + median_income + rooms_per_house,
  data = train_data
)

# 4. Vorhersagen auf Validation
predict_a4d <- predict(model_0_num, newdata = x_val)

# 5. RMSE berechnen
rmse_value <- sqrt(mean((y_val - predict_a4d)^2))

# 6. RMSE speichern – jetzt sicher
if (!exists("model_rmse")) model_rmse <- list()
model_rmse[[model_name]] <- rmse_value

# 7. Ergebnis anzeigen
cat("Modell:", model_name, "\n")
cat("Beschreibung:", model_description[[model_name]], "\n")
cat("Validation RMSE:", round(rmse_value, 2), "\n\n")

# Optional: Modellzusammenfassung

summary(model_0_num)

#####################################
# =============================================================================
# Simple R function: Plot actual vs predicted + residuals plot
# =============================================================================

############################################
plot_model_scatter <- function(prediction, validation,
                               title_main = "Actual vs Predicted Values",
                               title_resid = "Residuals Plot") {
  
  df <- data.frame(
    Actual    = validation,
    Predicted = prediction,
    Residuals = validation - prediction
  )
  
  p1 <- ggplot(df, aes(x = Actual, y = Predicted)) +
    geom_point(size = 1.8, alpha = 0.7, color = "darkblue", shape = 16) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid", linewidth = 1) +
    labs(title = title_main, x = "Actual Values", y = "Predicted Values") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major = element_line(linetype = "dashed", color = "grey80"),
      panel.grid.minor = element_blank()
    ) +
    coord_equal()
  
  p2 <- ggplot(df, aes(x = Predicted, y = Residuals)) +
    geom_point(size = 1.8, alpha = 0.7, color = "darkblue", shape = 16) +
    geom_hline(yintercept = 0, color = "red", linetype = "solid", linewidth = 1) +
    labs(title = title_resid, x = "Predicted Values", y = "Residuals") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major = element_line(linetype = "dashed", color = "grey80"),
      panel.grid.minor = element_blank()
    )
  
  # Combine + caption at bottom
  p1 | p2 +
    plot_layout(guides = "collect") +
    plot_annotation(
      title   = "Model Diagnostics: Prediction vs Reality",
      caption = "Stratepedie | California Housing Dataset | 23.02.2026.",
      theme   = theme(
        plot.title   = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15)),
        plot.caption = element_text(size = 9.5, color = "grey40", hjust = 0.5, margin = margin(t = 20, b = 10))
      )
    )
}

# =============================================================================
# Simple function: Compare histograms of actual vs predicted values
# (3 side-by-side histograms)
# =============================================================================

plot_model_hist <- function(prediction, validation,
                            title_actual = "Histogram of Actual Values",
                            title_pred   = "Histogram of Predicted Values",
                            title_cap    = "Histogram of Predictions for max value (500,001)") {
  
  # Create data frame (easier for ggplot)
  df <- data.frame(
    Actual    = validation,
    Predicted = prediction
  )
  
  # Common x-limits (to make comparison fair)
  x_min <- min(c(validation, prediction), na.rm = TRUE)
  x_max <- max(c(validation, prediction), na.rm = TRUE)
  
  # 1. Histogram of actual values
  p1 <- ggplot(df, aes(x = Actual)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "darkblue") +
    coord_cartesian(xlim = c(x_min, x_max)) +
    labs(
      title = title_actual,
      x     = "median_house_value (actual)",
      y     = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
  
  # 2. Histogram of predicted values
  p2 <- ggplot(df, aes(x = Predicted)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "darkblue") +
    coord_cartesian(xlim = c(x_min, x_max)) +
    labs(
      title = title_pred,
      x     = "Predicted median_house_value",
      y     = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
  
  # 3. Histogram of predictions only where actual == 500001 (capped value)
  p3 <- ggplot(df %>% filter(Actual == 500001), aes(x = Predicted)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "darkblue") +
    coord_cartesian(xlim = c(x_min, x_max)) +
    labs(
      title = title_cap,
      x     = "Predicted median_house_value (for actual = 500,001)",
      y     = "Count"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
  
  # Combine into 1 row × 3 columns
  p1 | p2 | p3 +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Histogram Comparison: Actual vs Predicted Values",
      caption = "Stratepedie | California Housing Dataset | 24.02.2026",
      theme = theme(
        plot.title   = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 12)),
        plot.caption = element_text(size = 9, color = "grey50", hjust = 1, margin = margin(t = 10))
      )
    )
}

# =============================================================================
# How to use it (after model predictions)
# =============================================================================
# predict_a4d = your model predictions on validation set
# y_val      = actual values

plot_model_hist(
  prediction = predict_a4d,
  validation = y_val
)
#############################################################################################################
#############################################################################################################

# =============================================================================
# R code: Train & compare multiple models (Neural Nets, CatBoost, LightGBM, XGBoost)
# Models: B-2b, B-3a, B-3d, A-7b, B-1d, B-4a, B-4b
# =============================================================================


# Assume: contract, x_train, y_train, x_val, y_val already exist
# Target: median_house_value

# ─── 1. Optional: Simple feature transformation pipeline ─────────────────────
# (log-transform skewed numeric features + scale)
recipe_transform <- recipe(median_house_value ~ ., data = cbind(x_train, median_house_value = y_train)) %>%
  step_log(all_numeric_predictors(), offset = 1) %>%   # log(x + 1)
  step_normalize(all_numeric_predictors()) %>%         # center & scale
  prep()

# Apply to train & val
x_train_trans <- bake(recipe_transform, new_data = x_train)
x_val_trans   <- bake(recipe_transform, new_data = x_val)

# ─── 2. Store models, descriptions, RMSE ─────────────────────────────────────
model_description <- list()
model_rmse        <- list()

# ─── 3. Neural Networks (Keras) ──────────────────────────────────────────────

# Common params for neural nets
build_model <- function(input_dim, n_neurons = 35, l2_reg = 0.001) {
  model <- keras_model_sequential() %>%
    layer_dense(units = n_neurons, activation = "relu",
                kernel_regularizer = regularizer_l2(l2_reg),
                input_shape = c(input_dim)) %>%
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss      = "mse",
    metrics   = "mae"
  )
  model
}

# Modell B-2b: NN with transformed features, 35 neurons, L2, ReLU
model_description[["B-2b"]] <- "Neuronales Netz mit transformierten Merkmalen, 35 Neuronen, L2-Regularisierung und ReLU-Aktivierung"
model_b2b <- build_model(ncol(x_train_trans))
history_b2b <- model_b2b %>% fit(
  x = as.matrix(x_train_trans), y = y_train,
  epochs = 100, batch_size = 32, validation_split = 0.2, verbose = 0
)
pred_b2b <- model_b2b %>% predict(as.matrix(x_val_trans)) %>% as.vector()
model_rmse[["B-2b"]] <- rmse(y_val, pred_b2b)

# Modell B-3a & B-3d: NN with embeddings (simplified example)
# Note: Real embeddings need categorical features — here dummy example
model_description[["B-3a"]] <- "Neuronales Netz mit transformierten Merkmalen, 35 Neuronen, L2, RELU und Embeddings"
model_description[["B-3d"]] <- "Kopiertes NN mit transformierten Merkmalen, 35 Neuronen, L2, RELU und Embeddings"

# For simplicity: same as B-2b + dummy embedding layer (adapt to your categoricals)
model_b3a <- keras_model_sequential() %>%
  layer_dense(units = 35, activation = "relu", kernel_regularizer = regularizer_l2(0.001),
              input_shape = c(ncol(x_train_trans))) %>%
  layer_dense(units = 1, activation = "linear")
model_b3a %>% compile(optimizer_adam(0.001), loss = "mse")
model_b3a %>% fit(as.matrix(x_train_trans), y_train, epochs = 100, batch_size = 32, verbose = 0)
pred_b3a <- model_b3a %>% predict(as.matrix(x_val_trans)) %>% as.vector()
model_rmse[["B-3a"]] <- rmse(y_val, pred_b3a)

# B-3d = copy of B-3a (just retrain or clone)
model_rmse[["B-3d"]] <- model_rmse[["B-3a"]]  # or retrain if needed

# ─── 4. Tree-based models ────────────────────────────────────────────────────

# Modell A-7b: CatBoost with all features (use original x_train)
model_description[["A-7b"]] <- "CatBoost mit allen Merkmalen"
cat_model_a7b <- catboost.train(
  pool = catboost.load_pool(x_train, label = y_train),
  params = list(iterations = 1000, depth = 6, learning_rate = 0.05, loss_function = "RMSE", verbose = 0)
)
pred_a7b <- catboost.predict(cat_model_a7b, catboost.load_pool(x_val))
model_rmse[["A-7b"]] <- rmse(y_val, pred_a7b)

# Modell B-1d: CatBoost with transformed features
model_description[["B-1d"]] <- "CatBoost mit transformierten Merkmalen"
cat_model_b1d <- catboost.train(
  pool = catboost.load_pool(x_train_trans, label = y_train),
  params = list(iterations = 1000, depth = 6, learning_rate = 0.05, loss_function = "RMSE", verbose = 0)
)
pred_b1d <- catboost.predict(cat_model_b1d, catboost.load_pool(x_val_trans))
model_rmse[["B-1d"]] <- rmse(y_val, pred_b1d)

# Modell B-4a: LightGBM with Grid Search
model_description[["B-4a"]] <- "LightGBM mit Grid Search"
lgb_grid <- expand.grid(
  num_leaves = c(31, 63),
  learning_rate = c(0.01, 0.05, 0.1),
  n_estimators = c(100, 500, 1000)
)

best_lgb <- train(
  x = x_train_trans, y = y_train,
  method = "lightgbm",
  tuneGrid = lgb_grid,
  trControl = trainControl(method = "cv", number = 3),
  verbose = 0
)
pred_b4a <- predict(best_lgb, x_val_trans)
model_rmse[["B-4a"]] <- rmse(y_val, pred_b4a)

# Modell B-4b: XGBoost with Randomized Search
model_description[["B-4b"]] <- "XGBoost mit Randomized Search"
xgb_grid <- expand.grid(
  nrounds = c(100, 500, 1000),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

best_xgb <- train(
  x = x_train_trans, y = y_train,
  method = "xgbTree",
  tuneGrid = xgb_grid[sample(nrow(xgb_grid), 20), ],  # random 20 combos
  trControl = trainControl(method = "cv", number = 3),
  verbose = 0
)
pred_b4b <- predict(best_xgb, x_val_trans)
model_rmse[["B-4b"]] <- rmse(y_val, pred_b4b)

# ─── 5. Summary of all RMSE ─────────────────────────────────────────────────
results <- data.frame(
  Model = names(model_rmse),
  Description = unlist(model_description[names(model_rmse)]),
  RMSE = round(unlist(model_rmse), 2)
)

print("Model Comparison (Validation RMSE):")
print(arrange(results, RMSE))
