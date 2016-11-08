# library(TTR)
# library(tseries)
library(forecast)
# library(ucminf)
# library(lpSolve)
n_prediction <- 3
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
trackingSignal = function(z, y) {
  ifelse(cumsum(abs(z-y))==0, 0, 1:length(z)*cumsum(z - y)/cumsum(abs(z-y)))
}

# Load data
raw_data <- c(50800, 50300, 50200, 48700, 48500, 48100, 50100, 48700, 49200, 51100, 50800, 52800, 53000, 51800, 53600, 53100, 51600, 50800, 50600, 49700, 49700, 50300, 49900, 51800)  

# Transform to time-series data structure
df <- data.frame(raw_data)
dfts <- ts(df)

###### PART A #####
# model.WMA <- WMA(dfts, n)
# forecast.WMA <- forecast(model.WMA, n, w)
# accuracy(forecast.WMA)


# ###### PART B #####
# 
# model.ES <- HoltWinters(dfts, gamma=FALSE, l.start = 49000, b.start = 100)
# plot(model.ES)
# accuracy(model.ES)
# forecast.ES <- forecast(model.ES, n_prediction)
# accuracy(forecast.ES)
# plot(forecast.ES)

# using "ets"
# # Automated forecasting using an exponential model
# model.ES <- ets(dfts, opt.crit = "mse", model = "AZN")
# accuracy(model.ES)
# forecast.ES <- forecast(model.ES, n_prediction)
# plot(forecast.ES)
# 
# # using "ets"
# # Automated forecasting using an exponential model
# model.ES <- ets(dfts, opt.crit = "mse")
# accuracy(model.ES)
# forecast.ES <- forecast(model.ES, n_prediction)
# plot(forecast.ES)

###### PART C #####

# Automated forecasting using an ARIMA model
model.ARIMA <- auto.arima(dfts)
accuracy(model.ARIMA)
forecast.ARIMA <- forecast(model.ARIMA, n_prediction)
plot(forecast.ARIMA)
lines(forecast.ARIMA$fitted, col="green")
trackingSignal.ARIMA <- trackingSignal(forecast.ARIMA$fitted, dfts)
plot(trackingSignal.ARIMA)
