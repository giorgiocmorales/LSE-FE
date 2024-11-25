#FM437 (WT) LECTURE 1

# Conventional Brownian Motion ------

simulate_brownian_motion <- function(n, Time, mu = 0, sigma = 1) {
  dt <- Time / n  # Time step size
  t <- seq(0, Time, by = dt)  # Time grid
  
  # Generate Brownian motion
  increments <- rnorm(n, mean = mu * dt, sd = sqrt(dt) * sigma)  # Normal increments
  W <- cumsum(c(0, increments))  # Start at 0 and sum increments
  
  return(list(time = t, motion = W))
}

# Parameters
n <- 1000          # Number of time steps
Time <- 1          # Total time
mu <- 0            # Drift parameter (set to zero)
sigma <- 1         # Volatility

# Simulate
set.seed(123)
bm_result <- simulate_brownian_motion(n, Time, mu, sigma)

# Plot
plot(bm_result$time, bm_result$motion, type = "l", col = "blue", lwd = 2, 
     xlab = "Time", ylab = "W(t)", main = "Conventional Brownian Motion")


# Geometric Brownian Motion ------

simulate_geometric_brownian_motion <- function(n, Time, P0, mu, sigma) {
  dt <- Time / n  # Time step size
  t <- seq(0, Time, by = dt)  # Time grid
  
  # Generate Brownian motion
  increments <- rnorm(n, mean = 0, sd = sqrt(dt))  # Standard normal increments
  W <- cumsum(c(0, increments))  # Start at 0 and sum increments
  
  # Transform to GBM
  P <- P0 * exp((mu - 0.5 * sigma^2) * t + sigma * W)
  
  return(list(time = t, prices = P))
}

# Simulate without drift

# Parameters
P0 <- 100          # Initial value
mu <- 0            # Drift parameter (set to zero)
sigma <- 0.2       # Volatility
n <- 1000          # Number of time steps
Time <- 1          # Total time

# Simulate with drift
gbm_result <- simulate_geometric_brownian_motion(n, Time, P0, mu, sigma)

# Plot
plot(gbm_result$time, gbm_result$prices, type = "l", col = "red", lwd = 2, 
     xlab = "Time", ylab = "P(t)", main = "Geometric Brownian Motion")

# Simulate without drift

# Parameters
P0 <- 100          # Initial value
mu <- 0.08         # Drift parameter (set to zero)
sigma <- 0.21       # Volatility
n <- 1000          # Number of time steps
Time <- 1          # Total time

# Simulate
gbm_result_2 <- simulate_geometric_brownian_motion(n, Time, P0, mu, sigma)

# Plot
plot(gbm_result_2$time, gbm_result_2$prices, type = "l", col = "orange", lwd = 2, 
     xlab = "Time", ylab = "P(t)", main = "Geometric Brownian Motion (Drift)")

# Custom function to calculate the CJ ratio -----
cj_ratio <- function(series) {
  # Calculate the differences between consecutive elements
  diffs <- diff(series)
  
  # Determine the signs of the differences
  signs <- sign(diffs)
  
  # Identify sequences (consecutive identical signs)
  sequences <- sum(head(signs, -1) == tail(signs, -1))
  
  # Identify reversals (consecutive differing signs)
  reversals <- length(signs) - 1 - sequences
  
  # Calculate the CJ ratio
  cj_ratio_value <- sequences / reversals
  
  return(list(CJ_Ratio = cj_ratio_value, Sequences = sequences, Reversals = reversals))
}

cj_ratio(bm_result$motion)
cj_ratio(gbm_result$prices)
cj_ratio(gbm_result_2$prices)


#CJ (1937)

mu <- 0.08
sigma <- 0.21

pi <- pnorm(q = -mu/sigma, lower.tail = FALSE)
pi

#CJ statistic
(pi^2+(1-pi)^2)/(2*pi*(1-pi))


# The Effect of Drift

n <- 99

pi_hat <- 0.6399
pi_s_hat <- pi_hat^2 + (1 - pi_hat)^2
pi_s_hat

mean <- pi_s_hat/(1 - pi_s_hat)
mean

var <- (pi_s_hat*(1-pi_s_hat) + 2*(pi_hat^3 + (1 - pi_hat)^3 - pi_s_hat^2))/(n*(1 - pi_s_hat)^4)
var
sqrt(var)


cj_ratio <- pi_s_hat/(1 - pi_s_hat)
cj_ratio

#Test

cj_0 <- 1

z <- (cj_ratio - cj_0)/sqrt(var)
z

2*pnorm(abs(z), lower.tail = FALSE)