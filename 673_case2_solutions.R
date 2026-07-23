## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module3_regression/case#2")

# Create data frame.
revenue.data <- read.csv("673_case2.csv")

# See the first 6 records of the file.
head(revenue.data)
tail(revenue.data)

## 1a.
# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
revenue.ts <- ts(revenue.data$Revenue, 
            start = c(2006, 1), end = c(2025, 3), freq = 4)
revenue.ts
## 1b.	
#Apply the plot() function to create a data plot with the historical data.
plot(revenue.ts, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 190000), main = "Walmart Quarterly Revenues", 
     xaxt = "n",
    col = "blue", bty = "l", lwd = 2)
axis(1, at = seq(2006, 2026, 1), labels = format(seq(2006, 2026, 1)))


## 2a.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(revenue.ts) = 79.
# nvalid = 19 quarters for the last 19 quarters (Q1-21 to Q3-25).
# nTrain = 60 quarters, from Q1-06 to Q4-20.
nValid <- 19
length(revenue.ts)
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2006, 1), end = c(2006, nTrain))
train.ts
valid.ts <- window(revenue.ts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))
valid.ts


## 2b. 
# FIT REGRESSION MODEL WITH (1) SEASONALITY, 
# (2) LINEAR TREND AND SEASONALITY, AND
## (3) QUADRATIC TREND AND SEASONALITY.
## IDENTIFY FORECAST FOR VALIDATION PERIOD FOR EACH MODEL.

## (1) SEASONALITY MODEL.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

plot(train.season.pred, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 200000), main = "Seasonality  for Training and Validation Data", 
     xlim = c(2006, 2026),flty = 2, bty = "l", lwd = 2, xaxt = "n")
axis(1, at = seq(2006, 2026, 1), labels = format(seq(2006, 2026, 1)))
lines(train.season.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(valid.ts, col = "black", lty = 1, lwd = 2)

## (4) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

plot(train.lin.trend.season.pred, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 200000), 
     main = "Linear Trend and Seasonality  for Training and Validation Data", 
     xlim = c(2006, 2026),flty = 2, bty = "l", lwd = 2, xaxt = "n")
axis(1, at = seq(2006, 2026, 1), labels = format(seq(2006, 2026, 1)))
lines(train.lin.trend.season.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(valid.ts, col = "black", lty = 1, lwd = 2)


## (5) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
            forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

plot(train.quad.trend.season.pred, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 200000), 
     main = "Quadratic Trend and Seasonality  for Training and Validation Data", 
     xlim = c(2006, 2026),flty = 2, bty = "l", lwd = 2, xaxt = "n")
axis(1, at = seq(2006, 2026, 1), labels = format(seq(2006, 2026, 1)))
lines(train.quad.trend.season.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(valid.ts, col = "black", lty = 1, lwd = 2)


## 2c. 
# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)



## 3a. 
# FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY and 
# WITH QUADRATIC TREND AND SEASONALITY FOR ENTIRE DATA SET. 
# FORECAST DATA AND MEASURE ACCURACY.

## (1) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonality model.
lin.trend.season <- tslm(revenue.ts ~ trend + season)

# See summary of linear trend and seasonality equation 
# and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 9, level = 0)
lin.trend.season.pred

plot(lin.trend.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 200000), 
     main = "Linear Trend and Seasonality Model for Entire Data Set", 
     xlim = c(2006, 2027), lty = 2, bty = "l", lwd = 2, xaxt = "n", 
     col="blue")
axis(1, at = seq(2006, 2027, 1), labels = format(seq(2006, 2027, 1)))
lines(lin.trend.season.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(revenue.ts, col = "black", lty = 1, lwd = 2)


## (2) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonality model.
quad.trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality equation 
# and associated parameters.
summary(quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# quadratic trend and seasonality data in the future 8 quarters.  
quad.trend.season.pred <- forecast(quad.trend.season, h = 9, level = 0)
quad.trend.season.pred

plot(quad.trend.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in Millions)", 
     ylim = c(70000, 200000), 
     main = "Quadratic Trend and Seasonality Model for Entire Data Set", 
     xlim = c(2006, 2028), lty = 2, bty = "l", 
     lwd = 2, col = "blue", xaxt = "n")
axis(1, at = seq(2006, 2027, 1), labels = format(seq(2006, 2027, 1)))
lines(quad.trend.season.pred$fitted, lty = 1, lwd = 2, col = "blue")
lines(revenue.ts, col = "black", lty = 1, lwd = 2)

## 3b. 
## COMPARE ACCURACY MEASURES OF REGRESSION FORECAST 
## WITH QUANDRATIC TREND AND SEASONALITY, AND LINEAR TREND AND
## SEASONALITY FOR ENTIRE DATA SET WITH ACCURACY MEASURES 
## OF NAIVE FORECAST AND SEASONAL NAIVE 
## FORECAST FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures
# for naive model, seasonal naive, and regression models. 
# with linear trend and seasonality, and quadratic trend 
# and seasonality.
round(accuracy(lin.trend.season.pred$fitted, revenue.ts),3)
round(accuracy(quad.trend.season.pred$fitted, revenue.ts),3)
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)

