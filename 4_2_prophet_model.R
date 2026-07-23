
## USE FORECAST LIBRARY.

library(forecast)

## INSTALL REQUIRED PACKAGES FOR PROPHET. 
# (one-time installation).
install.packages("vctrs", dependencies = TRUE)
install.packages('prophet')

library(prophet)

## CREATE DATA FRAME. 
# Set working directory for locating files.
setwd("C:/misc/673_BAN/module3_regression")

# Create data frame.
Amtrak.data <- read.csv("Amtrak_comp.csv")

# See the first 6 records of the file.
head(Amtrak.data)

# Specify Monthly Data explicitly as month/day/year.
Amtrak.data$Month <- as.Date(Amtrak.data$Month, 
                     format = "%m/%d/%Y")
head(data.frame(
  Month = format(Amtrak.data$Month, "%m/%d/%Y"),
  Ridership = Amtrak.data$Ridership
))

# DEVELOP DATA FRAME FOR PROPHET MODEL AND
# CREATE PARTITIONS. 

# Develop data frame.
# df = data frame,
# ds = historical dates,
# y = historical data.
df <- data.frame(
  ds = Amtrak.data$Month,
  y  = Amtrak.data$Ridership
)
head(df)
tail(df)

# (Optional) Make sure that data is in order of the
# data periods.
df <- df[order(df$ds), ] # optional for Amtrak Data.

# Split the data into train and validation partitions.
h <- 60
train <- head(df, nrow(df) - h)
valid <- tail(df, h) 
valid

# Ensure train and valid dates are used as Date.
train$ds    <- as.Date(train$ds)
valid$ds    <- as.Date(valid$ds)

##TRAIN FORECASTIN MODEL USING PROPHET.

# Develop Profit forecasting model for 
# training partition with monthly seasonality 
# (seasonality within a year). 
m_train <- prophet(
  train,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality  = FALSE)

## MAKE FORECAST FOR TRAINING AND VALIDATION
## PARTITIONS.

# Forecast for entire data set.
future_train <- make_future_dataframe(m_train, 
               periods = h, freq = "month")
forecast_train <- predict(m_train, future_train)
forecast_train$ds  <- as.Date(forecast_train$ds)

# Display forecast for the entire data set. 
forecast_train$yhat

# Make forecast for validation partition only
# and display this forecast.
last_train <- max(train$ds)
forecast_valid <- subset(forecast_train, 
                  ds > last_train)
forecast_valid$yhat

# Make sure the forecast_valid contains numeric values.
y_valid <- as.numeric(valid$y)
yhat_valid <- as.numeric(forecast_valid$yhat)

# Construct time series (ts) with correct 
# start from the first validation date.
start_year  <- as.integer(format(min(valid$ds), "%Y"))
start_month <- as.integer(format(min(valid$ds), "%m"))

## Create accuracy measures (accuracy metrics). 
# Convert the Prophet data and forecast into ts  
# (time series) format from the forecast package.
y_valid_ts <- ts(y_valid, start = c(start_year, start_month), 
                 frequency = 12)
yhat_valid_ts <- ts(yhat_valid, start = c(start_year, start_month), 
                 frequency = 12)

# Develop and display plot of of Prophet model in
# training and validation partitions. 
plot(train$ds, train$y, type="l", 
     col="black", lwd=2,xlab="Date", 
     main = "Prophet Model: Training and Validation Forecast",
     ylab="Ridership", ylim = c(1300, 3500),
     xlim = c(min(df$ds), max(forecast_train$ds)),
     xaxt="n")
lines(valid$ds, valid$y, col="black", lwd=2)
lines(forecast_train$ds,
      forecast_train$yhat,
      col="blue", lwd=2, lty=2)
#lines(forecast_valid$ds,
#      forecast_valid$yhat,
#      col="blue", lwd=2, lty=2)
abline(v = max(train$ds), lty=3)
# Create annual marks for the time series period.
start_year <- as.numeric(format(min(df$ds), "%Y"))
end_year   <- as.numeric(format(max(forecast_train$ds), "%Y"))
year_ticks <- seq(as.Date(paste0(start_year, "-01-01")),
                  as.Date(paste0(end_year, "-01-01")),
                  by = "1 year")
axis(1, at = year_ticks,
     labels = format(year_ticks, "%Y"))
legend("topleft",
       legend=c("Train and Validation Data",
                "Train and Validation Forecast"),
       col=c("black", "blue"),
       lty=c(1,2),
       lwd=2,
       bty="n")

# Accuracy measures for Prophet model in validation 
# partition. 
round(accuracy(y_valid_ts, yhat_valid_ts),3)


# USE PROPHET MODEL TO DEVELOP FORECAST FOR ENTIRE
# DATA SET. PLOT FORECAST AND IDENTIFY ACCURACY.

# Create forecasting model using Prophet for entire data set. 
m <- prophet(df,
yearly.seasonality = TRUE,
weekly.seasonality = FALSE,
daily.seasonality  = FALSE
)

# Make forecast for entire data set 
# and for one year into the future 
# (year 2019, periods = 12). 
future <- make_future_dataframe(m, periods = 12, freq = "month")
forecast <- predict(m, future)

# Display forecast for the entire data set. 
# and 12 periods into the future.  
forecast$yhat

# Ensure same dates for the forecast for the 
# entire data set.
df$ds <- as.Date(df$ds)
forecast$ds <- as.Date(forecast$ds)

# Develop forecast for the entire data set only.
idx <- match(df$ds, forecast$ds)
yhat_hist <- forecast$yhat[idx]
yhat_hist

# Develop point forecast for the future 12 periods.  
last_ds <- max(df$ds)
last_ds
yhat_future <- forecast$yhat[forecast$ds > last_ds]
yhat_future <- head(yhat_future, 12) #ensure 12 periods (optional)
yhat_future

## PLOT ENTIRE DATA SET< FITTED FORECAST, AND
## FORECAST FOR FUTURE 12 MONTHS.

# Last historical date.
last_ds <- max(df$ds)
# Plot historical data.
plot(df$ds, df$y,type = "l",
     col = "black", lwd = 2, lty = 1,
     xlab = "Date", ylab = "Ridership",
     main = "Prophet Model Forecast",
     xaxt = "n")
# Create annual marks for the time series period.
start_year <- as.numeric(format(min(df$ds), "%Y"))
end_year   <- as.numeric(format(max(forecast$ds), "%Y"))
year_ticks <- seq(as.Date(paste0(start_year, "-01-01")),
                  as.Date(paste0(end_year, "-01-01")),
                  by = "1 year")
axis(1, at = year_ticks,
     labels = format(year_ticks, "%Y"))
# Add forecast lines.
last_ds <- max(df$ds)
# Full fitted forecast (blue solid).
hist_idx <- forecast$ds <= last_ds
lines(forecast$ds[hist_idx],
      forecast$yhat[hist_idx],
      col = "blue",lwd = 2, lty = 1)
# Future only (blue dashed)
future_idx <- forecast$ds > last_ds
lines(forecast$ds[future_idx],
      forecast$yhat[future_idx],
      col = "blue", lwd = 2, lty = 2)
abline(v = last_ds, lty = 2)
legend("topleft",
       legend = c("Ridersip Data",
                  "Prophet Forecast (Fitted)",
                  "Prophet Forecast (Future)"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2),
       lwd = 2,  bty = "n")

## CREATED ACCURACY MEASURES FOR PROPHET FORECAST.

# Convert the Prophet data and forecast into ts  
# (time series) format from the forecast package.
y_ts <- ts(df$y, start = c(1991,1), frequency = 12)
yhat_ts <- ts(yhat_hist, start = c(1991,1), frequency = 12)

# Accuracy measures for Prophet model.
round(accuracy(y_ts, yhat_ts),3)


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, 
## AND MEASURE ACCURACY.

# Use ts() to create time series data for forecasting.
ridership.ts <- ts(Amtrak.data$Ridership, 
                   start = c(1991, 1), end = c(2018, 12), 
                   freq = 12)

# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(ridership.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months.  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df


# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(ridership.ts, 
     xlab = "Time", ylab = "Ridership (in 000s)", 
     ylim = c(1300, 3500), xaxt = "n",
     bty = "l", xlim = c(1991, 2020.25), lwd = 2,
     main = "Two-Level Forecast: Regression with Trend and Seasonlity + AR(1)
     for Residuals") 
axis(1, at = seq(1991, 2020, 1), labels = format(seq(1991, 2020, 1)))
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)
legend(1992,3200, legend = c("Ridership Series for Training and Valiadaton Periods", 
                             "Two-Level Forecast for Training and Valiadtion Periods", 
                             "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2014, 2014), c(0, 3500))
lines(c(2019, 2019), c(0, 3500))
text(2002, 3400, "Training")
text(2016.5, 3400, "Validation")
text(2020.2, 3400, "Future")
arrows(1991, 3300, 2013.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2014.1, 3300, 2018.9, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 3300, 2021.3, 3300, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common 
# accuracy measures for:
# (1) Two-level model (linear trend and seasonality model 
#     + AR(1) model for residuals),
# (2) Linear trend and seasonality model only,
# (3) Prophet model,
# (4) seasonal naive forecast. 
round(accuracy(lin.season$fitted + residual.ar1$fitted, ridership.ts), 3)
round(accuracy(lin.season$fitted, ridership.ts), 3)
round(accuracy(y_ts, yhat_ts),3)
round(accuracy((snaive(ridership.ts))$fitted, ridership.ts), 3)
