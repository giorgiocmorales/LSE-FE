#FM437 (WT) LECTURE 5

library(tidyverse)
library(PerformanceAnalytics)

#Daily Exchange Rates: ARMA(0, 0)
set.seed(123)  # For reproducibility
exchange_rates <- arima.sim(model = list(order = c(0, 0, 0)), n = 1000)
plot(exchange_rates, type = "l", main = "Simulated Daily Exchange Rates (ARMA(0, 0))", ylab = "Value")


#SDaily Stock Returns: ARMA(2, 5)
set.seed(123)
stock_returns <- arima.sim(model = list(order = c(2, 0, 5), 
                                        ar = c(0.5, -0.3),  # Example AR coefficients
                                        ma = c(0.4, -0.2, 0.1, 0.05, -0.01)), # Example MA coefficients
                           n = 1000)
plot(stock_returns, type = "l", main = "Simulated Daily Stock Returns (ARMA(2, 5))", ylab = "Value")

# Daily Interest Rates: ARMA(3, 4)
set.seed(123)
interest_rates <- arima.sim(model = list(order = c(3, 0, 4), 
                                         ar = c(0.6, -0.4, 0.2),  # Example AR coefficients
                                         ma = c(0.3, -0.25, 0.15, -0.05)), # Example MA coefficients
                            n = 1000)
plot(interest_rates, type = "l", main = "Simulated Daily Interest Rates (ARMA(3, 4))", ylab = "Value")

#Fit models
# Fit ARMA models for exchange rates

library(forecast)

fit_exchange <- Arima(exchange_rates, order = c(0, 0, 0))
AIC(fit_exchange)
BIC(fit_exchange)

fit_stock <- Arima(stock_returns, order = c(2, 0, 5))
AIC(fit_stock)
BIC(fit_stock)

fit_interest <- Arima(interest_rates, order = c(3, 0, 4))
AIC(fit_interest)
BIC(fit_interest)

#Residuals

# Generate residuals
residuals_exchange <- residuals(fit_exchange)
residuals_stock <- residuals(fit_stock)
residuals_interest <- residuals(fit_interest)

# Compute squared errors
squared_errors_exchange <- residuals_exchange^2
squared_errors_stock <- residuals_stock^2
squared_errors_interest <- residuals_interest^2

# Plot ACFs
acf(squared_errors_exchange, main = "ACF of Squared Errors (Exchange Rates)")
acf(squared_errors_stock, main = "ACF of Squared Errors (Stock Returns)")
acf(squared_errors_interest, main = "ACF of Squared Errors (Interest Rates)")

# Get numerical ACF values
acf_values <- acf(squared_errors_exchange, plot = FALSE)
acf_values$acf  # Numerical autocorrelations

#







