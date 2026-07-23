
## USE FORECAST LIBRARY.

library(forecast)

# Set working directory for locating files.
setwd("C:/misc/673_BAN/module3_regression/case#2")

# Create data frame.
revenue.data <- read.csv("673_case2.csv")

# See the first 6 records of the file.
head(revenue.data)
tail(revenue.data)

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
revenue.ts <- ts(revenue.data$Revenue, 
                 start = c(2006, 1), end = c(2025, 3), freq = 4)
revenue.ts

# Create data partitioning for training and validation 
# data sets.
nValid <- 19
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2006, 1), end = c(2006, nTrain))
train.ts
valid.ts <- window(revenue.ts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))
valid.ts


## PART 1.

## 1a.
# Use Arima() function to fit AR(1) model.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
revenue.ar1<- Arima(revenue.ts, order = c(1,0,0))
summary(revenue.ar1)

z.stat <- (0.9485 - 1)/0.0399
z.stat
p.value <- pnorm(z.stat)
p.value

## 1b.
# Create differenced revenue.ts data using lag-1.
diff.revenue <- diff(revenue.ts, lag = 1)

# Use Acf() function to identify autocorrelation for the model differencing. 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(diff.revenue, lag.max = 8, 
    main = "Autocorrelation for First Differencing (lag1) of Walmart Revenue")


## PART 2. 

## 2a.
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train.ts ~ trend + I(trend^2)+ season)
# See summary of quadratic trend and seasonal equation and associated parameters.
summary(train.trend.season)
# Apply forecast() function to make predictions for ts with 
# trend and seasonal model in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred


## 2b.
# Use Acf() function to identify autocorrealtion for the model residuals 
# (training set), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.trend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Walmart Revenue's Training Residuals")


## 2c.
# Use Arima() function to fit AR(1) model for training residuals. 
# The Arima model of order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Use Acf() function to identify autocorrealtion for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 8, 
    main = 
    "Autocorrelation for Walmart's Training Residuals of Residuals")


## 2d. 
# Create two-level modeling results, regression + AR(1) for validation period.
# Create data table with historical validation data, regression forecast
# for validation period, AR(1) for validation, and and two-level model results. 
valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean
valid.df <- data.frame(valid.ts, train.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Valid.Revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df



## 2e.
# Use tslm() function to create linear trend and seasonality model 
# for the entire data set.
trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)
# See summary of linear trend equation and associated parameters.
summary(trend.season)

# Apply forecast() function to make predictions with quadratic trend and seasonal 
# model into the future 8 quarters.  
trend.season.pred <- forecast(trend.season, h = 9, level = 0)
trend.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into 
# the future 8 quarters.
residual.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 9, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 8, 
    main = 
    "Autocorrelation for AR(1) Model Residuals for Entire Data Set")

# Identify two-level forecast for the 8 future periods 
# as sum of linear trend and seasonal model 
# and AR(1) model for residuals.
trend.season.ar1.pred <- trend.season.pred$mean + residual.ar1.pred$mean
trend.season.ar1.pred

# Create a data table with quadratic trend and seasonality forecast for 12 future periods,
# AR(1) model for residuals for 9 future periods, and combined two-level forecast for
# 9 future periods. 
table.df <- data.frame(trend.season.pred$mean, 
                       residual.ar1.pred$mean, trend.season.ar1.pred)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df


## PART 3. 

## 3a. 
# Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(train.arima)
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred


## 3b.
# Utilize auto.arima() function to automatically identify 
# the ARIMA model structure and parameters. 
# Develop the ARIMA forecast for the validation period. 
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred


## 3c.
# Accuracy measures for the two ARIMA models in questions 3a and 3b.
round(accuracy(train.arima.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

## 3d.
# Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the entire data set.
# Use summary() to show ARIMA model and its parameters.
# Apply forecast for the 9 periods in the future. 
entire.arima <- Arima(revenue.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(entire.arima)
entire.arima.pred <- forecast(entire.arima, h = 9, level = 0)
entire.arima.pred

# Use auto.arima() function for the entire data set..
# Use summary() to show ARIMA model and its parameters.
# Apply forecast for the 8 periods in the future. 
auto.arima <- auto.arima(revenue.ts)
summary(auto.arima)
auto.arima.pred <- forecast(auto.arima, h = 9, level = 0)
auto.arima.pred


## 3e.
# Use accuracy() function to identify common accuracy measures for:
# (1) Regression model with quadratic trend and seasonality
# (2) Two-level model (regression model + AR(1) for regression residuals)
# (3) ARIMA(1,1,1)(1,1,1)
# (4) Auto ARIMA model
# (5) Seasonal naive forecast.
round(accuracy(trend.season$fitted, revenue.ts), 3)
round(accuracy(trend.season$fitted + residual.ar1$fitted, revenue.ts), 3)
round(accuracy(entire.arima.pred$fitted, revenue.ts), 3)
round(accuracy(auto.arima.pred$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)
