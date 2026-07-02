############################################################
# Case Study #2: Forecasting Walmart’s Quarterly Revenue
# Data: 673_case2.csv (Quarterly, $million), Q1-2006 to Q3-2025
# Goal: Forecast Q4-2025 and Q1-Q4 of 2026–2027 (9 quarters ahead)
############################################################

## =========================================================
## Q0. USE FORECAST LIBRARY + LOAD DATA
## =========================================================
library(forecast)

# (Optional) Set your working directory if needed
# setwd("~/Desktop/BAN_TS/module2_smoothing")

# Read data (columns: Quarter, Revenue)
wmt.data <- read.csv("673_case2.csv")

# Quick check
head(wmt.data)
str(wmt.data)

## =========================================================
## Q1. PLOT THE DATA + VISUALIZE TIME SERIES COMPONENTS
##   (a) Create time series with ts()
##   (b) Plot and describe components you can see
## =========================================================

# (Q1a) Create quarterly time series (freq = 4)
# Data runs from Q1-2006 through Q3-2025.
revenue.ts <- ts(wmt.data$Revenue,
                 start = c(2006, 1), end = c(2025, 3), freq = 4)

# (Q1b) Plot the time series
plot(revenue.ts,
     xlab = "Year", ylab = "Revenue ($ million)",
     main = "Walmart Quarterly Revenue (Q1-2006 to Q3-2025)",
     bty = "l")


# Notes (for your report):
# - Trend: long-run upward/downward movement over time
# - Seasonality: recurring quarterly pattern (Q1/Q2/Q3/Q4 effects)
# - Irregular: short-run random variation around trend/seasonality


## =========================================================
## Q2. APPLY REGRESSION MODELS USING DATA PARTITION
##   Models:
##     i)  Seasonality only
##     ii) Linear trend + seasonality
##     iii) Quadratic trend + seasonality
##   (a) Split into training + validation (validation = 19 quarters)
## =========================================================

# (Q2a) Data partition
nValid <- 19
nTrain <- length(revenue.ts) - nValid

# Training and validation sets using indices
train.ts <- window(revenue.ts, end = time(revenue.ts)[nTrain])
valid.ts <- window(revenue.ts, start = time(revenue.ts)[nTrain + 1])

length(train.ts)  # should be 60
length(valid.ts)  # should be 19


## =========================================================
## Q2b. FIT 3 REGRESSION MODELS ON TRAINING + FORECAST VALIDATION
##   Use tslm() + summary() + forecast()
## =========================================================

# ----- Model i: Regression with seasonality only
train.season <- tslm(train.ts ~ season)
summary(train.season)

train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred


# ----- Model ii: Regression with linear trend + seasonality
train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)

train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred


# ----- Model iii: Regression with quadratic trend + seasonality
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.season)

train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred


# (Optional) Plot each model’s validation forecast vs actual validation
plot(train.season.pred$mean, xlab="Year", ylab="Revenue ($ million)",
     main="Model i: Seasonality Only (Train + Validation Forecast)",
     bty="l", lwd=2, lty=2, xaxt="n")
axis(1, at = seq(2006, 2028, 1), labels = seq(2006, 2028, 1))
lines(train.season.pred$fitted, lwd=2)
lines(train.ts, col="black")
lines(valid.ts, col="black")

plot(train.lin.season.pred$mean, xlab="Year", ylab="Revenue ($ million)",
     main="Model ii: Linear Trend + Seasonality (Train + Validation Forecast)",
     bty="l", lwd=2, lty=2, xaxt="n")
axis(1, at = seq(2006, 2028, 1), labels = seq(2006, 2028, 1))
lines(train.lin.season.pred$fitted, lwd=2)
lines(train.ts, col="black")
lines(valid.ts, col="black")

plot(train.quad.season.pred$mean, xlab="Year", ylab="Revenue ($ million)",
     main="Model iii: Quadratic Trend + Seasonality (Train + Validation Forecast)",
     bty="l", lwd=2, lty=2, xaxt="n")
axis(1, at = seq(2006, 2028, 1), labels = seq(2006, 2028, 1))
lines(train.quad.season.pred$fitted, lwd=2)
lines(train.ts, col="black")
lines(valid.ts, col="black")

## =========================================================
## Q2c. COMPARE ACCURACY MEASURES (MAPE + RMSE) ON VALIDATION
## =========================================================

acc.season     <- accuracy(train.season.pred$mean, valid.ts)
acc.lin.season <- accuracy(train.lin.season.pred$mean, valid.ts)
acc.quad.season<- accuracy(train.quad.season.pred$mean, valid.ts)

round(acc.season, 3)
round(acc.lin.season, 3)
round(acc.quad.season, 3)

# Extract RMSE and MAPE directly (only one row exists)

acc.table <- data.frame(
  Model = c("Seasonality Only",
            "Linear Trend + Seasonality",
            "Quadratic Trend + Seasonality"),
  RMSE = c(acc.season[,"RMSE"],
           acc.lin.season[,"RMSE"],
           acc.quad.season[,"RMSE"]),
  MAPE = c(acc.season[,"MAPE"],
           acc.lin.season[,"MAPE"],
           acc.quad.season[,"MAPE"])
)

acc.table

# Rank by MAPE first, then RMSE
acc.table[order(acc.table$MAPE, acc.table$RMSE), ]

## =========================================================
## Q3. USE ENTIRE DATA SET TO FORECAST Q4-2025 and 2026–2027
##   (a) Fit the two best regression models on FULL series
##   (b) Compare with Naive and Seasonal Naive using accuracy()
## =========================================================

# (Q3a) Forecast horizon:
# From last observed quarter Q3-2025, forecast:
#   Q4-2025 (1 quarter) + 2026–2027 (8 quarters) = 9 quarters total
h.future <- 9

# --- Refit Model ii on full data: Linear Trend + Seasonality
full.lin.season <- tslm(revenue.ts ~ trend + season)
summary(full.lin.season)

full.lin.season.pred <- forecast(full.lin.season, h = h.future, level = 0)
full.lin.season.pred

plot(full.lin.season.pred$mean,
     xlab="Year", ylab="Revenue ($ million)",
     main="Forecast: Linear Trend + Seasonality (Full Data)",
     bty="l", lwd=2, lty=2, xaxt="n")
axis(1, at = seq(2006, 2028, 1), labels = seq(2006, 2028, 1))
lines(full.lin.season.pred$fitted, lwd=2)
lines(revenue.ts, col="black")


# --- Refit Model iii on full data: Quadratic Trend + Seasonality
full.quad.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)
summary(full.quad.season)

full.quad.season.pred <- forecast(full.quad.season, h = h.future, level = 0)
full.quad.season.pred

plot(full.quad.season.pred$mean,
     xlab="Year", ylab="Revenue ($ million)",
     main="Forecast: Quadratic Trend + Seasonality (Full Data)",
     bty="l", lwd=2, lty=2, xaxt="n")
axis(1, at = seq(2006, 2028, 1), labels = seq(2006, 2028, 1))
lines(full.quad.season.pred$fitted, lwd=2)
lines(revenue.ts, col="black")


# (Q3a) Present forecasts for Q4-2025 and Q1-Q4 of 2026–2027
# (These are the out-of-sample forecast means)
full.lin.season.pred$mean
full.quad.season.pred$mean

## =========================================================
## Q3b. COMPARE ACCURACY OF FULL-DATA REGRESSION FIT VS NAIVE BASELINES
##   NOTE: This compares IN-SAMPLE fitted values to actual series.
## =========================================================

# Regression model in-sample accuracy
round(accuracy(full.lin.season.pred$fitted, revenue.ts), 3)
round(accuracy(full.quad.season.pred$fitted, revenue.ts), 3)

# Naive and Seasonal Naive in-sample accuracy
round(accuracy(naive(revenue.ts)$fitted, revenue.ts), 3)
round(accuracy(snaive(revenue.ts)$fitted, revenue.ts), 3)

# Put key measures into one table (MAPE + RMSE)
acc.full <- rbind(
  LinTrend_Season = accuracy(full.lin.season.pred$fitted, revenue.ts),
  QuadTrend_Season= accuracy(full.quad.season.pred$fitted, revenue.ts),
  Naive           = accuracy(naive(revenue.ts)$fitted, revenue.ts),
  SeasonalNaive   = accuracy(snaive(revenue.ts)$fitted, revenue.ts)
)

round(acc.full[ , c("RMSE","MAPE")], 3)

# In your report:
# - Compare RMSE and MAPE across the 4 methods above.
# - Identify which method is most accurate (lowest RMSE/MAPE) for forecasting.
############################################################
