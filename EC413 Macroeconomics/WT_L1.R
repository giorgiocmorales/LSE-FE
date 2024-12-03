# FM 413 Lecture 1

set.seed(123)
n <- 200
time <- 1:n
trend_linear <- 2 + 0.05 * time # Linear trend
trend_quadratic <- 2 + 0.05 * time - 0.0002 * time^2 # Quadratic trend
cycle <- sin(2 * pi * time / 50) + rnorm(n, sd = 0.2) # Cyclical component
y_linear <- trend_linear + cycle
y_quadratic <- trend_quadratic + cycle

# Linear

# Fit a linear model for the trend
fit_linear <- lm(y_linear ~ time)
trend_estimated_linear <- predict(fit_linear)
cyclical_component_linear <- y_linear - trend_estimated_linear

# Plot results
plot(time, y_linear, type = "l", col = "black", main = "Linear Trend Decomposition")
lines(time, trend_estimated_linear, col = "blue", lwd = 2, lty = 2) # Trend
lines(time, cyclical_component_linear, col = "red", lwd = 1) # Cyclical component
legend("bottomright", legend = c("Original", "Trend", "Cyclical"), col = c("black", "blue", "red"), lty = c(1, 2, 1))

# Quadratic

# Fit a quadratic model for the trend
fit_quadratic <- lm(y_quadratic ~ time + I(time^2))
trend_estimated_quadratic <- predict(fit_quadratic)
cyclical_component_quadratic <- y_quadratic - trend_estimated_quadratic

#Comparison

# Plot results
plot(time, y_quadratic, type = "l", col = "black", main = "Quadratic Trend Decomposition")
lines(time, trend_estimated_quadratic, col = "blue", lwd = 2, lty = 2) # Trend
lines(time, cyclical_component_quadratic, col = "red", lwd = 1) # Cyclical component
legend("bottomright", legend = c("Original", "Trend", "Cyclical"), col = c("black", "blue", "red"), lty = c(1, 2, 1))

plot(time, y_linear, type = "l", col = "black", main = "Linear vs. Quadratic Trend")
lines(time, trend_estimated_linear, col = "blue", lwd = 2, lty = 2) # Linear trend
lines(time, trend_estimated_quadratic, col = "green", lwd = 2, lty = 2) # Quadratic trend
legend("bottomright", legend = c("Original", "Linear Trend", "Quadratic Trend"), col = c("black", "blue", "green"), lty = c(1, 2, 2))

# Hodrick Prescott Filter

# Load the required package
library(mFilter)

# Generate a synthetic time series with trend and cyclical components
set.seed(123)
n <- 200
time <- 1:n
trend <- 2 + 0.05 * time # Underlying trend
cycle <- sin(2 * pi * time / 50) + rnorm(n, sd = 0.2) # Cyclical component
y <- trend + cycle # Observed series

# Apply Hodrick-Prescott filter
hp_result <- hpfilter(y, freq = 1600) # 'freq = 1600' is common for quarterly data

# Extract trend and cyclical components
trend_hp <- hp_result$trend
cycle_hp <- hp_result$cycle

# Plot the results
plot(time, y, type = "l", col = "black", lwd = 1.5, 
     main = "HP Filter: Trend and Cyclical Components",
     ylab = "Value", xlab = "Time")
lines(time, trend_hp, col = "blue", lwd = 2, lty = 2) # Trend
lines(time, cycle_hp, col = "red", lwd = 1, lty = 1) # Cyclical component
legend("topright", legend = c("Original", "Trend", "Cyclical"),
       col = c("black", "blue", "red"), lty = c(1, 2, 1), lwd = c(1.5, 2, 1))