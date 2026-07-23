############################################################
# TWO-LEVEL FORECAST: Regression + Trailing MA of residuals #
# Answers (a)–(e) in one code cell with comments            #
############################################################

library(forecast)
library(zoo)

#-----------------------------------------------------------
# ASSUMPTION: you already created sales.ts (monthly ts)
# If not, uncomment and adjust to your file/columns:
sales.data <- read.csv("/mnt/data/673_case1.csv")
sales.data$Month <- as.Date(sales.data$Month, format="%m/%d/%Y")
sales.data <- sales.data[order(sales.data$Month), ]
sales.ts <- ts(sales.data$Sales,
             start=c(as.integer(format(min(sales.data$Month),"%Y")),
                        as.integer(format(min(sales.data$Month),"%m"))),
               frequency=12)
#-----------------------------------------------------------

############################################################
# (a) Training/Validation partition + regression with trend+season
############################################################
nTrain <- 84   # 7 years
nValid <- 36   # 3 years

train.ts <- window(sales.ts, end = time(sales.ts)[nTrain])
valid.ts <- window(sales.ts, start = time(sales.ts)[nTrain + 1])

# Regression model on TRAINING: linear trend + seasonality
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)  # <-- include this in report

# Forecast monthly sales in VALIDATION using regression model
reg.valid.fst <- forecast(trend.seas, h = nValid, level = 0)
reg.valid.fst  # <-- include this forecast in report

# Model equation (for report, in words):
# Sales_t = b0 + b1*t + (monthly seasonal dummy effects) + e_t


############################################################
# (b) Training residuals -> trailing MA(k=3) -> forecast residuals in validation
############################################################
# Residuals in TRAINING
train.res <- residuals(trend.seas)
train.res

# Trailing MA (k=3) on training residuals
ma.res.trail3 <- rollmean(train.res, k = 3, align = "right")
ma.res.trail3

# Convert residual MA to ts so forecast() handles time properly
ma.res.trail3.ts <- ts(ma.res.trail3,
                       end = time(train.ts)[length(train.ts)],
                       frequency = frequency(train.ts))

# Forecast residual MA into VALIDATION
res.valid.fst <- forecast(ma.res.trail3.ts, h = nValid, level = 0)
res.valid.fst  # <-- include this residual forecast in report


############################################################
# (c) Two-level combined forecast in validation + accuracy comparison
############################################################
# Two-level forecast = regression forecast + residual MA forecast
comb.valid.mean <- reg.valid.fst$mean + res.valid.fst$mean

# Table for report: Actual validation, regression forecast, residual forecast, combined
valid.df <- round(data.frame(
  Validation = as.numeric(valid.ts),
  Regression.Fst = as.numeric(reg.valid.fst$mean),
  MA.Residuals.Fst = as.numeric(res.valid.fst$mean),
  Combined.Fst = as.numeric(comb.valid.mean)
), 3)
valid.df  # <-- include in report

# Accuracy in VALIDATION: Regression vs Combined
acc.reg.valid  <- accuracy(reg.valid.fst$mean, valid.ts)
acc.comb.valid <- accuracy(comb.valid.mean, valid.ts)

round(acc.reg.valid[, c("RMSE","MAPE")], 3)
round(acc.comb.valid[, c("RMSE","MAPE")], 3)

# Identify best model in validation (lowest RMSE / MAPE)
best_valid_rmse <- ifelse(acc.reg.valid[,"RMSE"] < acc.comb.valid[,"RMSE"], "Regression", "Two-level Combined")
best_valid_mape <- ifelse(acc.reg.valid[,"MAPE"] < acc.comb.valid[,"MAPE"], "Regression", "Two-level Combined")
best_valid_rmse
best_valid_mape


############################################################
# (d) Entire dataset: fit regression + residual MA(k=3), forecast 12 months of 2026,
#     and build a 2026 table (reg forecast, residual forecast, combined)
############################################################
h2026 <- 12

# Regression on FULL dataset
tot.trend.seas <- tslm(sales.ts ~ trend + season)
summary(tot.trend.seas)

# Forecast regression 12 future months (2026)
tot.reg.fst <- forecast(tot.trend.seas, h = h2026, level = 0)

# Residuals on FULL dataset
tot.res <- residuals(tot.trend.seas)

# Trailing MA(k=3) on FULL residuals
tot.ma.res.trail3 <- rollmean(tot.res, k = 3, align = "right")

# Convert to ts aligned to end of sales.ts
tot.ma.res.trail3.ts <- ts(tot.ma.res.trail3,
                           end = time(sales.ts)[length(sales.ts)],
                           frequency = frequency(sales.ts))

# Forecast residual MA 12 months (2026)
tot.res.fst <- forecast(tot.ma.res.trail3.ts, h = h2026, level = 0)

# Two-level combined forecast for 2026
tot.comb.fst <- tot.reg.fst$mean + tot.res.fst$mean

# Build 2026 table for report
# Create labels like "2026-01", ..., "2026-12"
start_2026 <- c(as.integer(format(as.Date(paste0(floor(time(sales.ts)[length(sales.ts)]), "-12-01")), "%Y")) + 1), 1)
# Safer: just build labels from the forecast time indices
future_time <- time(tot.reg.fst$mean)
future_year <- floor(future_time)
future_month <- round((future_time - future_year) * 12 + 1)
future_label <- sprintf("%04d-%02d", future_year, future_month)

future.df <- round(data.frame(
  Month = future_label,
  Regression.Fst = as.numeric(tot.reg.fst$mean),
  MA.Residuals.Fst = as.numeric(tot.res.fst$mean),
  Combined.Fst = as.numeric(tot.comb.fst)
), 3)
future.df  # <-- include in report


############################################################
# (e) Seasonal naive vs Regression vs Two-level (in-sample historical accuracy)
############################################################
# Seasonal naive on full data (monthly seasonality)
sn <- snaive(sales.ts)

# In-sample fitted values
fit_reg <- fitted(tot.trend.seas)
fit_sn  <- fitted(sn)

# For combined fitted, we need same length and no NAs:
# residual MA has NAs at start (k-1). Align and compare on overlapping portion.
tot.ma.res.trail3.full <- rep(NA, length(sales.ts))
tot.ma.res.trail3.full[(3):length(sales.ts)] <- as.numeric(tot.ma.res.trail3)  # k=3 => first 2 NA
fit_comb <- fit_reg + tot.ma.res.trail3.full

# Compute accuracy on historical data (remove NA rows)
idx_ok <- which(!is.na(fit_comb) & !is.na(fit_reg) & !is.na(fit_sn))
y_ok <- sales.ts[idx_ok]

acc_reg_hist  <- accuracy(fit_reg[idx_ok],  y_ok)
acc_comb_hist <- accuracy(fit_comb[idx_ok], y_ok)
acc_sn_hist   <- accuracy(fit_sn[idx_ok],   y_ok)

hist.tbl <- rbind(
  SeasonalNaive = acc_sn_hist[, c("RMSE","MAPE")],
  Regression    = acc_reg_hist[, c("RMSE","MAPE")],
  TwoLevel      = acc_comb_hist[, c("RMSE","MAPE")]
)

round(hist.tbl, 3)

best_hist_rmse <- rownames(hist.tbl)[which.min(hist.tbl[,"RMSE"])]
best_hist_mape <- rownames(hist.tbl)[which.min(hist.tbl[,"MAPE"])]
best_hist_rmse
best_hist_mape