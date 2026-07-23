
## USE FORECAST AND ZOO LIBRARIES.

library(forecast)
library(zoo)


## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module2_smoothing/case#1")

# Create data frame.
sales.data <- read.csv("673_Case1_12.csv")

# See the first 6 records of the file.
head(sales.data)
tail(sales.data)

## 1a.
# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
sales.ts <- ts(sales.data$Sales, 
                   start = c(2016, 1), end = c(2025, 12), freq = 12)
sales.ts

## 1b. 
# Plot the time series data. 
plot(sales.ts, 
     xlab = "Time", ylab = "Food Production in the U.S., Mln. Tons", 
     ylim = c(100, 700), bty = "l",
     xaxt = "n", xlim = c(2016, 2026.25), 
     main = "Sales Data", lwd = 3, col="blue") 
axis(1, at = seq(2016, 2026, 1), labels = format(seq(2016, 2026, 1)))

## 1c.
# Use acf() function to identify autocorrealtion and plot autocorrelation
# for different lags (up to maximum of 12).
autocor <- Acf(sales.ts, lag.max = 12, 
           main = "Autocorrelation for Sales Data")

## 2a.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 36
nTrain <- length(sales.ts) - nValid
nTrain
train.ts <- window(sales.ts, start = c(2016, 1), end = c(2016, nTrain))
valid.ts <- window(sales.ts, start = c(2016, nTrain + 1), 
                   end = c(2016, nTrain + nValid))

## 2b.
# Create trailing moving average with window widths of k = 3, 5, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_3 <- rollmean(train.ts, k = 3, align = "right")
ma.trailing_5 <- rollmean(train.ts, k = 5, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

## 2c.
## Create forecast for the validation data for the window widths 
# of k = 3, 5, and 12. 
ma.trail_3.pred <- forecast(ma.trailing_3, h = nValid, level = 0)
ma.trail_3.pred

ma.trail_5.pred <- forecast(ma.trailing_5, h = nValid, level = 0)
ma.trail_5.pred

ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

## 2d.
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
# round(accuracy((snaive(prod.ts))$fitted, prod.ts), 3)
round(accuracy(ma.trail_3.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_5.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)


## 3a.
# Fit a regression model with linear trend and seasonality for
# training partition. 
trend.seas <- tslm(train.ts ~ trend  + season)
summary(trend.seas)

# Create regression forecast with linear trend and seasonality for 
# validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred


## 3b.
# Identify and display residuals based on the regression model in
# training period.
trend.seas.res <- trend.seas$residuals
trend.seas.res

# Apply trailing MA for residuals with window width k = 3. 
ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res

# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

## 3c.
# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- data.frame(valid.ts, trend.seas.pred$mean, 
                       ma.trail.res.pred$mean, 
                       fst.2level)
names(valid.df) <- c("Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

## 3d.
# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(sales.ts ~ trend + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

## 3e.
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(tot.trend.seas.pred$fitted, sales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)



## 4a.
# Use ets() function with model = "ZZZ", i.e., automated selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## 4b.
## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## the FUTURE FOR 12 PERIODS.

# Create Holt-Winter's exponential smoothing (HW) for entire data set. 
# Use ets() function with model = "ZZZ", to identify the best hw option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this hw model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred


## 4c.
# Identify performance measures for hw forecast and compare it with seasonal naive
# forecast.
round(accuracy(snaive(sales.ts)$fitted, sales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)
