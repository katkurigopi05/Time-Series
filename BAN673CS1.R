
## USE FORECAST LIBRARY.
# install.packages(c("forecast","zoo"))  
library(forecast)
library(zoo)

## INPUT DATA

# Set working directory for locating files

setwd("~/Desktop/BAN_TS/module2_smoothing")

# Create data frame.
sales.data <- read.csv("673_case1.csv")

# See the first 6 and last 6 records of the file.
head(sales.data)
tail(sales.data)

## QUESTION 1. IDENTIFY TIME SERIES COMPONENTS AND PLOT DATA

## 1a. CREATE TIME SERIES DATA SET sales.ts USING ts()
# Monthly data => frequency = 12
# Data period: Jan 2016 through Dec 2025
sales.ts <- ts(sales.data$Sales, start = c(2016, 1), end = c(2025, 12), freq = 12)
sales.ts
length(sales.ts)

## 1b. PLOT HISTORICAL DATA
plot(sales.ts,
     xlab = "Time", ylab = "Sales (Millions of $)",
     xaxt = "n", bty = "l", lwd = 2,
     main = "Worldwide Monthly Grocery Sales (2016–2025)")
axis(1, at = seq(2016, 2026 ,1), labels = seq(2016, 2026 ,1))

## (OPTIONAL) STL DECOMPOSITION PLOT (TREND/SEASONAL/REMAINDER)
sales.stl <- stl(sales.ts, s.window = "periodic")
plot(sales.stl)
title("STL Decomposition: Sales = Trend + Seasonal + Remainder")

## 1c. APPLY Acf() TO IDENTIFY POSSIBLE TIME SERIES COMPONENTS
# Use lag.max = 12 to see up to 1 years of monthly lags.
lag.max <- 12
autocor <- Acf(sales.ts, lag.max = lag.max,
               main = "Autocorrelation (ACF) for Grocery Sales")

# Display autocorrelation coefficients for various lags.
acf.table <- data.frame(
  Lag = 0:lag.max,
  ACF = round(as.numeric(autocor$acf), 3)
)
acf.table

## QUESTION 2. USE TRAILING MA FOR FORECASTING TIME SERIES

## 2a. DEVELOP DATA PARTITION (TRAIN = 84, VALID = 36)
# Total months = 120 (2016–2025)
# Validation = 36 months (3 years): Jan 2023 – Dec 2025
# Training   = 84 months (7 years): Jan 2016 – Dec 2022
nValid <- 36
nTrain <- length(sales.ts) - nValid

train.ts <- window(sales.ts, start = c(2016, 1), end = c(2016, nTrain))
valid.ts <- window(sales.ts, start = c(2016, nTrain + 1), end = c(2016, nTrain + nValid))

train.ts
valid.ts
length(train.ts)
length(valid.ts)
# Plot time series and visualize partitions. #AI QWEN model Qwen3-Coder prompt: provide plot for sales data for training and validation partitions 
plot(train.ts,
     xlab = "Time", ylab = "Sales (Millions of $)", ylim = c(min(sales.ts)*0.9, max(sales.ts)*1.1),
     bty = "l", xlim = c(2016, 2026.25), xaxt = "n",
     main = "Sales Data: Training and Validation Partitions", lwd = 2)
axis(1, at = seq(2016, 2026, 1), labels = seq(2016, 2026, 1))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Vertical lines for partition boundaries.
lines(c(2023, 2023), c(0, max(sales.ts)*1.2))
lines(c(2026, 2026), c(0, max(sales.ts)*1.2))
text(2019.3, max(sales.ts)*1.15, "Training")
text(2024.3, max(sales.ts)*1.15, "Validation")
text(2026.8, max(sales.ts)*1.15, "Future")
arrows(2016.0, max(sales.ts)*1.11, 2022.9, max(sales.ts)*1.11, code = 3, length = 0.08, lwd = 1, angle = 30)
arrows(2023.1, max(sales.ts)*1.11, 2025.9, max(sales.ts)*1.11, code = 3, length = 0.08, lwd = 1, angle = 30)
arrows(2026.1, max(sales.ts)*1.11, 2027.2, max(sales.ts)*1.11, code = 3, length = 0.08, lwd = 1, angle = 30)

## 2b. DEVELOP 3 TRAILING MAs (k = 3, 5, 12) FOR TRAINING
# rollmean(..., align="right") => trailing moving average
ma.trailing_3  <- rollmean(train.ts, k = 3,  align = "right")
ma.trailing_5  <- rollmean(train.ts, k = 5,  align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Show first 6 and last 6 values for one MA (optional display)
head(ma.trailing_3)
tail(ma.trailing_3)

head(ma.trailing_5)
tail(ma.trailing_5)

head(ma.trailing_12)
tail(ma.trailing_12)

## 2c. FORECAST TRAILING MA IN VALIDATION PERIOD (SHOW ONE: k = 3,5,12)
ma.trail_3.pred  <- forecast(ma.trailing_3,  h = nValid, level = 0)
ma.trail_5.pred  <- forecast(ma.trailing_5,  h = nValid, level = 0)
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)

# Show forecast output for k = 3,5,12
ma.trail_3.pred
ma.trail_5.pred
ma.trail_12.pred

# Plot trailing MA forecast (k = 3) for training + validation partitions #AI Qwen model: Qwen3-Coder prompt  :  provide plot for sales data for training and validation partitions 
plot(sales.ts,
     xlab = "Time", ylab = "Sales (Millions of $)", ylim = c(min(sales.ts)*0.9, max(sales.ts)*1.1),
     bty = "l", xlim = c(2016, 2026.25), xaxt = "n",
     main = "Trailing Moving Average Forecast (k = 3)")
axis(1, at = seq(2016, 2026, 1), labels = seq(2016, 2026, 1))
lines(ma.trailing_3, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_3.pred$mean, col = "blue", lwd = 2, lty = 2)
lines(train.ts, col = "black", lwd = 1)
lines(valid.ts, col = "black", lwd = 1)

lines(c(2023, 2023), c(0, max(sales.ts)*1.2))
lines(c(2026, 2026), c(0, max(sales.ts)*1.2))

## 2d. COMPARE ACCURACY (MAPE, RMSE) FOR k = 3, 5, 12 IN VALIDATION

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_3.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_5.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)

acc.ma3  <- accuracy(ma.trail_3.pred$mean,  valid.ts)
acc.ma5  <- accuracy(ma.trail_5.pred$mean,  valid.ts)
acc.ma12 <- accuracy(ma.trail_12.pred$mean, valid.ts)

acc.trailingMA.valid <- round(rbind(
  MA_k3  = acc.ma3[,  c("RMSE","MAPE")],
  MA_k5  = acc.ma5[,  c("RMSE","MAPE")],
  MA_k12 = acc.ma12[, c("RMSE","MAPE")]
), 3)

acc.trailingMA.valid

## QUESTION 3. TWO-LEVEL FORECAST: REGRESSION + TRAILING MA (RESIDUALS)

## 3a. REGRESSION MODEL (LINEAR TREND + SEASONALITY) ON TRAINING
reg.trend.seas <- tslm(train.ts ~ trend + season)
summary(reg.trend.seas)   # Include output in report

# Forecast monthly sales in validation period using regression model
reg.pred <- forecast(reg.trend.seas, h = nValid, level = 0)
reg.pred

# Plot regression forecast for training + validation #AI QWEN model Qwen3-Coder prompt : give plot for sales.ts using training and validation 
plot(sales.ts,
     xlab = "Time", ylab = "Sales (Millions of $)", ylim = c(min(sales.ts)*0.9, max(sales.ts)*1.1),
     bty = "l", xlim = c(2016, 2026.25), xaxt = "n",
     main = "Regression Forecast (Trend + Seasonality): Training & Validation")
axis(1, at = seq(2016, 2026, 1), labels = seq(2016, 2026, 1))
lines(reg.trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(reg.pred$mean, col = "blue", lwd = 2, lty = 2)
lines(train.ts, col = "black", lwd = 1)
lines(valid.ts, col = "black", lwd = 1)
lines(c(2023, 2023), c(0, max(sales.ts)*1.2))
lines(c(2026, 2026), c(0, max(sales.ts)*1.2))

## 3b. RESIDUALS (TRAINING), TRAILING MA (k=3), RESIDUAL FORECAST (VALIDATION)
reg.res.train <- reg.trend.seas$residuals
reg.res.train

ma.res.train <- rollmean(reg.res.train, k = 3, align = "right")
ma.res.train

ma.res.pred <- forecast(ma.res.train, h = nValid, level = 0)
ma.res.pred

## 3c. TWO-LEVEL (COMBINED) FORECAST IN VALIDATION
fst.2level.valid <- reg.pred$mean + ma.res.pred$mean

valid.df <- round(data.frame(
  Sales = as.numeric(valid.ts),
  Regression.Fst = as.numeric(reg.pred$mean),
  MA.Residuals.Fst = as.numeric(ma.res.pred$mean),
  Combined.Fst = as.numeric(fst.2level.valid)
), 3)

# Add time labels (helpful for report tables)
time.labels.valid <- time(valid.ts)
valid.df$Time <- time.labels.valid
valid.df <- valid.df[, c("Time","Sales","Regression.Fst","MA.Residuals.Fst","Combined.Fst")]
valid.df

# Accuracy comparison: regression vs 2-level in validation #AI Qwen3 model :Qwen3-Coder  cmd : compare RMSE and MAPE for both regression and 2 level and provide code for it
acc.reg.valid <- accuracy(reg.pred$mean, valid.ts)
acc.two.valid <- accuracy(fst.2level.valid, valid.ts)

acc.models.valid <- round(rbind(
  Regression = acc.reg.valid[, c("RMSE","MAPE")],
  TwoLevel   = acc.two.valid[, c("RMSE","MAPE")]
), 3)

acc.models.valid

## 3d. ENTIRE DATA SET: REGRESSION + TRAILING MA (RESIDUALS k=3)  #AI QWEN3-coder prompt: make clean and correct code 
##     FORECAST 12 FUTURE MONTHS OF 2026 + TWO-LEVEL COMBINATION
tot.reg <- tslm(sales.ts ~ trend + season)
summary(tot.reg)

tot.reg.pred <- forecast(tot.reg, h = 12, level = 0)
tot.reg.pred

tot.res <- tot.reg$residuals

# Use trailing MA on residuals (k=3) for forecasting residual component
tot.ma.res <- rollmean(tot.res, k = 3, align = "right")
tot.ma.res.pred <- forecast(tot.ma.res, h = 12, level = 0)

tot.fst.2level <- tot.reg.pred$mean + tot.ma.res.pred$mean

future12.df <- round(data.frame(
  Time = time(tot.reg.pred$mean),
  Regression.Fst = as.numeric(tot.reg.pred$mean),
  MA.Residuals.Fst = as.numeric(tot.ma.res.pred$mean),
  Combined.Fst = as.numeric(tot.fst.2level)
), 3)

future12.df



## 3e. SEASONAL NAIVE + ACCURACY COMPARISON (ENTIRE DATA SET)
# Seasonal naive fitted values (in-sample) come from snaive(sales.ts)$fitted
sn.full <- snaive(sales.ts)
sn.full
# Build in-sample combined fitted = regression fitted + trailing MA residuals (k=3)
# Use fill=NA so lengths align for in-sample combination
tot.ma.res.full <- rollmean(tot.res, k = 3, align = "right", fill = NA)
combined.fitted.full <- tot.reg$fitted + tot.ma.res.full

# Compare accuracy on common periods where all fitted values exist (avoid NAs). #AI QWEN3-coder prompt: compare all 3 models and provide me the code
fits.all <- cbind(
  SeasonalNaive = sn.full$fitted,
  Regression    = tot.reg$fitted,
  TwoLevel      = combined.fitted.full
)

idx.common <- complete.cases(fits.all)

acc.sn.full   <- accuracy(fits.all[idx.common, "SeasonalNaive"], sales.ts[idx.common])
acc.reg.full  <- accuracy(fits.all[idx.common, "Regression"],    sales.ts[idx.common])
acc.two.full  <- accuracy(fits.all[idx.common, "TwoLevel"],      sales.ts[idx.common])

acc.models.full.Q3 <- round(rbind(
  SeasonalNaive = acc.sn.full[,  c("RMSE","MAPE")],
  Regression    = acc.reg.full[, c("RMSE","MAPE")],
  TwoLevel      = acc.two.full[, c("RMSE","MAPE")]
), 3)

acc.models.full.Q3


## QUESTION 4. ADVANCED EXPONENTIAL SMOOTHING (ETS / HOLT-WINTERS)

## 4a. TRAINING PARTITION: ETS WITH AUTOMATED (ZZZ) OPTIONS + FORECAST VALIDATION
hw.train <- ets(train.ts, model = "ZZZ")
hw.train       # Model summary (include in report)

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.train.pred <- forecast(hw.train, h = nValid, level = 0)
hw.train.pred  

# Plot HW(ETS) forecast for training + validation #AI QWEN3-coder prompt: draw plot for holt-winter model
plot(sales.ts,
     xlab = "Time", ylab = "Sales (Millions of $)", ylim = c(min(sales.ts)*0.9, max(sales.ts)*1.1),
     bty = "l", xlim = c(2016, 2026.25), xaxt = "n",
     main = "ETS (Auto ZZZ) Forecast: Training & Validation")
axis(1, at = seq(2016, 2026, 1), labels = seq(2016, 2026, 1))
lines(hw.train$fitted, col = "blue", lwd = 2, lty = 1)
lines(hw.train.pred$mean, col = "blue", lwd = 2, lty = 2)
lines(sales.ts, col = "black", lwd = 1)
lines(c(2023, 2023), c(0, max(sales.ts)*1.2))
lines(c(2026, 2026), c(0, max(sales.ts)*1.2))

# Accuracy of ETS on validation (optional, but useful)
acc.hw.valid <- accuracy(hw.train.pred$mean, valid.ts)
round(acc.hw.valid[, c("RMSE","MAPE")], 3)

## 4b. ENTIRE DATA SET: ETS WITH AUTOMATED (ZZZ) OPTIONS + FORECAST 12 MONTHS OF 2026
hw.full <- ets(sales.ts, model = "ZZZ")
hw.full       # Model summary (include in report)

hw.full.pred <- forecast(hw.full, h = 12, level = 0)
hw.full.pred  # 2026 forecast (include in report)

# Plot HW(ETS) forecast for entire data + 12 months ahead #AI QWEN3-coder prompt: draw the plot to forecast 2026   
plot(hw.full.pred$mean,
     xlab = "Time", ylab = "Sales (Millions of $)", ylim = c(min(sales.ts)*0.9, max(c(sales.ts, hw.full.pred$mean))*1.1),
     bty = "l", xlim = c(2016, 2026.25), xaxt = "n",
     main = "ETS (Auto ZZZ) Model: Entire Data + Forecast for 2026",
     col = "blue", lwd = 2)
axis(1, at = seq(2016, 2026, 1), labels = seq(2016, 2026, 1))
lines(hw.full$fitted, col = "blue", lwd = 2)
lines(sales.ts, col = "black", lwd = 1)
lines(c(2026, 2026), c(0, max(c(sales.ts, hw.full.pred$mean))*1.2))

## 4c. COMPARE ACCURACY: SEASONAL NAIVE vs ETS (FULL DATA, COMMON PERIODS)
fits.4c <- cbind(
  SeasonalNaive = sn.full$fitted,
  ETS           = hw.full$fitted
)
idx.4c <- complete.cases(fits.4c)

acc.sn.4c <- accuracy(fits.4c[idx.4c, "SeasonalNaive"], sales.ts[idx.4c])
acc.hw.4c <- accuracy(fits.4c[idx.4c, "ETS"],           sales.ts[idx.4c])

acc.models.full.Q4 <- round(rbind(
  SeasonalNaive = acc.sn.4c[, c("RMSE","MAPE")],
  ETS_AutoZZZ   = acc.hw.4c[, c("RMSE","MAPE")]
), 3)

acc.models.full.Q4

## 4d. COMPARE BEST FORECASTS FROM Q3e (3 models) AND Q4c (2 models) #AI QWEN3-coder prompt: compare best forcasts from 3e and 4c all models
# Put ALL candidate fitted values into one table (common periods only)
fits.final <- cbind(
  SeasonalNaive = sn.full$fitted,
  Regression    = tot.reg$fitted,
  TwoLevel      = combined.fitted.full,
  ETS_AutoZZZ   = hw.full$fitted
)

idx.final <- complete.cases(fits.final)

acc.final <- round(rbind(
  SeasonalNaive = accuracy(fits.final[idx.final, "SeasonalNaive"], sales.ts[idx.final])[, c("RMSE","MAPE")],
  Regression    = accuracy(fits.final[idx.final, "Regression"],    sales.ts[idx.final])[, c("RMSE","MAPE")],
  TwoLevel      = accuracy(fits.final[idx.final, "TwoLevel"],      sales.ts[idx.final])[, c("RMSE","MAPE")],
  ETS_AutoZZZ   = accuracy(fits.final[idx.final, "ETS_AutoZZZ"],   sales.ts[idx.final])[, c("RMSE","MAPE")]
), 3)

acc.final

# OPTIONAL: Identify “best” by RMSE and by MAPE (lower is better) #AI QWEN3-coder prompt:find best RMSE amd MAPE
best.by.RMSE <- rownames(acc.final)[which.min(acc.final[, "RMSE"])]
best.by.MAPE <- rownames(acc.final)[which.min(acc.final[, "MAPE"])]

best.by.RMSE
best.by.MAPE

