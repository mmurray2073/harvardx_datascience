# Install required packages and libraries
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(xts)
library(ggplot2)
library(forecast)

# Load raw data file containg day-ahead location marginal pricing data for PJM region
rawdata <- read.csv("https://raw.githubusercontent.com/mmurray2073/harvardx_datascience/master/pjm_da_lmp.csv")

# Assign name to dataset
df <- rawdata

# Set datetime field to enable hourly time-series analysis for hourly pricing data
df$DATETIME <- paste(df$OPR_DATE,df$OPR_HOUR)
df$DATETIME <- as.POSIXct(df$DATETIME, format = '%m/%d/%Y %H')
df <- data.frame(df$DATETIME, df$PRICE)
names(df) <- c("datetime","price")

# Day of the week
df$day <- as.factor(strftime(df$datetime, format = '%A'))
# Day of the year
df$yearday <- as.factor(strftime(df$datetime, format = '%m%d'))
# Final structure for the study
str(df)

# Adjust size of dataset improve model run-time
df <- subset(df, datetime >= strptime('01-01-2019 00:00', format = '%d-%m-%Y %H:%M'))
str(df)

# Create test data set
df_test <- subset(df, datetime >= strptime('18-04-2019 00:00', format = '%d-%m-%Y %H:%M'))
df <- subset(df, datetime < strptime('18-04-2019 00:00', format = '%d-%m-%Y %H:%M'))
str(df)
str(df_test)

# Create time-series objects
ts <- ts(df$price, frequency = 1)
da_lmp_ts <- xts(df$price, df$datetime)

# Plot the time series to visualize pricing across the time period
plot(da_lmp_ts, main = 'DA LMP time series', xlab = 'Date', ylab = 'DA LMP')

# Plot day ahead lmp by day of week
ggplot(df, aes(day, price)) + geom_boxplot() + xlab('Day') + ylab('DA LMP') + ggtitle('DA LMP by weekday')

# Aggregate an average day ahead lmp per day
avg_da_lmp_per_yearday <- aggregate(price ~ yearday, df, 'mean')
avg_da_lmp_per_yearday$yearday <- factor(avg_da_lmp_per_yearday$yearday)

# Smooth the curve for the time series
smooth_yearday <- rbind(avg_da_lmp_per_yearday, avg_da_lmp_per_yearday, avg_da_lmp_per_yearday, avg_da_lmp_per_yearday, avg_da_lmp_per_yearday)

smooth_yearday <- lowess(smooth_yearday$price, f = 1 / 60)
lts <- length(avg_da_lmp_per_yearday$price)
lts_0 <- 2 * lts + 1
lts_1 <- 3 * lts
smooth_yearday <- smooth_yearday$y[lts_0:lts_1]

# Create plot with smoothed curve
par(mfrow = c(1, 1))
plot(avg_da_lmp_per_yearday$yearday, avg_da_lmp_per_yearday$price, type = 'l', main = 'Average DA LMP', xlab = 'Date', ylab = 'DA LMP')
lines(avg_da_lmp_per_yearday$yearday, smooth_yearday, col = 'blue', lwd = 2)

# Create bar and box plot showing errors
par(mfrow = c(1, 2))
diff <- avg_da_lmp_per_yearday$price - smooth_yearday
abs_diff <- abs(diff)
barplot(diff[order(-abs_diff)], main = 'Error', ylab = 'Error')
boxplot(diff, main = 'Error', ylab = 'Error')

# Plot autocorrelation and partial autocorrelation to identify seasonality
par(mfrow = c(2, 2))
acf(df$price, 90, main = 'Autocorrelation')
acf(df$price, 1800, main = 'Autocorrelation')
pacf(df$price, 90, main = 'Partial autocorrelation')
pacf(df$price, 1800, main = 'Partial autocorrelation')

# Plot decomposition to analyse weekly seasonal patterns
weeklyts <- ts(ts, frequency = 7)
decomp_weeklyts <- decompose(weeklyts)
plot(decomp_weeklyts)

# Adjust for weekly seaonality
df$price_weeklyts <- df$price - as.numeric(decomp_weeklyts$season)

# Plot decomposition to analyse yearly seasonal patterns
yearlyts <- ts(df$price_weeklyts, frequency = 24)
decomp_yearlyts <- decompose(yearlyts)
plot(decomp_yearlyts)

# Adjust for yearly seasonality
df$price_weeklyts_yearlyts <- df$price_weeklyts - as.numeric(decomp_yearlyts$season)

# Plot day ahead lmp with seasonal data removed
par(mfrow = c(1, 1))
ts_weekly_yearly <- ts(df$price_weeklyts_yearlyts, frequency = 1)
pricets_weekly_yearly <- xts(df$price_weeklyts_yearlyts, df$datetime)
plot(pricets_weekly_yearly, main = 'DA LMP (seasonal data removed)', xlab = 'Date', ylab = 'DA LMP')

# Aggregating demand by day of the year (average)
avg_lmp_per_yearday <- aggregate(price_weeklyts_yearlyts ~ yearday, df, 'mean')

# Smooth curve for the time series
smooth_yearday <- rbind(avg_lmp_per_yearday, avg_lmp_per_yearday, avg_lmp_per_yearday, avg_lmp_per_yearday, avg_lmp_per_yearday)
smooth_yearday <- lowess(smooth_yearday$price_weeklyts_yearlyts, f = 1 / 60)
ltssea <- length(avg_lmp_per_yearday$price_weeklyts_yearlyts)
ltssea_0 <- 2 * ltssea + 1
ltssea_1 <- 3 * ltssea
smooth_yearday <- smooth_yearday$y[ltssea_0:ltssea_1]

# Plotting the seasonally corrected smoothed results
par(mfrow = c(1, 1))
plot(avg_da_lmp_per_yearday$yearday, avg_lmp_per_yearday$price_weeklyts_yearlyts, type = 'l', main = 'Average DA LMP (seasonal data removed)', xlab = 'Date', ylab = 'DA LMP')
lines(avg_da_lmp_per_yearday$yearday, smooth_yearday, col = 'blue', lwd = 2)

# Create bar and box plot showing errors without seasonality
par(mfrow = c(1, 2))
diff <- avg_lmp_per_yearday$price_weeklyts_yearlyts - smooth_yearday
abs_diff <- abs(diff)
barplot(diff[order(-abs_diff)], main = 'Error (seasonal data removed)', ylab = 'Error')
boxplot(diff, main = 'Error (seasonal data removed)', ylab = 'Error')

# Autocorrelation and partial correlation on data corrected for seasonality
par(mfrow = c(2, 2))
acf(df$price_weeklyts_yearlyts, 90, main = 'Autocorrelation (seasonal data removed)')
pacf(df$price_weeklyts_yearlyts, 90, main = 'Partial autocorrelation (seasonal data removed)')
acf(df$price_weeklyts_yearlyts, 1800, main = 'Autocorrelation (seasonal data removed)')
pacf(df$price_weeklyts_yearlyts, 1800, main = 'Partial autocorrelation (seasonal data removed)')

# Use Arima function to create forecast model
forecast_model <- Arima(ts, order = c(2, 1, 2), list(order = c(1, 1, 1), period = 7))

# Calculate forecast error using test data set
fcstlmpts <- ts
fcstlmpmodel <- forecast_model
fcsterrs <- c()
fcstpred <- c()
fcstperc <- c()
for (i in 1:nrow(df_test)) {
  p <- as.numeric(predict(fcstlmpmodel, newdata = fcstlmpts, n.ahead = 1)$pred)
  fcstpred <- c(fcstpred, p)
  fcsterrs <- c(fcsterrs, p - df_test$price[i])
  fcstperc <- c(fcstperc, (p - df_test$price[i]) / df_test$price[i])
  fcstlmpts <- ts(c(fcstlmpts, df_test$price[i]), frequency = 7)
  fcstlmpmodel <- Arima(fcstlmpts, model = fcstlmpmodel)
}

# Plot forecaset error
par(mfrow = c(1, 1))
plot(fcsterrs, type = 'l', main = 'Forecast Error')

# Plot actuals versus forecast
plot(fcstpred, type = 'l', main = 'Actuals vs. Forecast', col = 'red')
lines(df_test$price)
legend('topright', c('Actuals', 'Forecast'), lty = 1, col = c('black', 'red'))

# Calculate mean error
fcstabserr <- mean(abs(fcsterrs))
fcstpercerr <- mean(abs(fcstperc)) * 100
model_results <- data_frame(method = "Auto-Regressive Integrated Moving Averages (ARIMA)", AbsoluteError = fcstabserr,
                            PercentError = fcstpercerr)
model_results %>% knitr::kable()

# Forecast model plot
plot(forecast(Arima(tail(ts, 100), model = forecast_model)), main = 'Forecast Model - 7 days')