# FM 413 Lecture 2

# Intro

# Parameters
set.seed(123)
T <- 100 # time periods
C_bar <- 2 # baseline consumption
I_bar <- 3 # baseline investment
c <- 0.6 # propensity to consume
i <- 0.3 # sensitivity of investment to income growth

# Initialize variables
Y <- numeric(T)
C <- numeric(T)
I <- numeric(T)
Y[1:2] <- c(10, 11) # initial values

# Model simulation
for (t in 3:T) {
  C[t] <- C_bar + c * Y[t-1]
  I[t] <- I_bar + i * (Y[t-1] - Y[t-2])
  Y[t] <- C[t] + I[t]
}

# Plot results
plot(1:T, Y, type = "l", col = "blue", main = "Samuelson Model", ylab = "Output (Y)", xlab = "Time")
lines(1:T, C, col = "green", lty = 2)
lines(1:T, I, col = "red", lty = 3)
legend("topright", legend = c("Output (Y)", "Consumption (C)", "Investment (I)"),
       col = c("blue", "green", "red"), lty = c(1, 2, 3))

# Supply-Side Samuelson Model

# Parameters
A <- 1  # productivity
alpha <- 0.3
delta <- 0.05 # depreciation
N <- 1 # fixed labor supply
K <- numeric(T)
K[1] <- 5 # initial capital stock

# Extend the Samuelson framework
for (t in 3:T) {
  K[t] <- (1 - delta) * K[t-1] + I[t]
  Y[t] <- A * K[t]^alpha * N^(1 - alpha)
}

# Plot capital stock and output
par(mfrow = c(1, 2))
plot(1:T, Y, type = "l", col = "blue", main = "Output (Y)", ylab = "Output", xlab = "Time")
plot(1:T, K, type = "l", col = "purple", main = "Capital Stock (K)", ylab = "Capital Stock", xlab = "Time")

# 3. Real Business Cycle (RBC) Model (Deterministic)

# Parameters
beta <- 0.96 # discount factor
utility <- function(C, L) log(C) + log(1 - L) # utility function

# Decision variables and initialization
C <- numeric(T)
L <- rep(0.3, T) # fixed labor for simplicity
A_t <- A # productivity (constant here)

# RBC simulation
for (t in 3:T) {
  C[t] <- A_t * K[t]^alpha * L[t]^(1 - alpha) - K[t+1] + (1 - delta) * K[t]
  K[t+1] <- beta * (A_t * alpha * K[t]^(alpha-1) * L[t]^(1 - alpha) + 1 - delta)
}

# Plot consumption, labor, and capital
par(mfrow = c(1, 3))
plot(1:T, C, type = "l", col = "green", main = "Consumption (C)", ylab = "Consumption", xlab = "Time")
plot(1:T, L, type = "l", col = "orange", main = "Labor (L)", ylab = "Labor", xlab = "Time")
plot(1:T, K, type = "l", col = "purple", main = "Capital (K)", ylab = "Capital", xlab = "Time")


# 4. Stochastic RBC Model

# Stochastic parameters
rho <- 0.9
sigma <- 0.02
z <- numeric(T)
z[1] <- A # initial productivity
epsilon <- rnorm(T, mean = 0, sd = sigma)

# Generate stochastic productivity
for (t in 2:T) {
  z[t] <- rho * z[t-1] + epsilon[t]
}

# Stochastic RBC simulation
for (t in 3:T) {
  C[t] <- exp(z[t]) * K[t]^alpha * L[t]^(1 - alpha) - K[t+1] + (1 - delta) * K[t]
  K[t+1] <- beta * (exp(z[t]) * alpha * K[t]^(alpha-1) * L[t]^(1 - alpha) + 1 - delta)
}

# Plot productivity and output
par(mfrow = c(1, 2))
plot(1:T, z, type = "l", col = "red", main = "Productivity (z)", ylab = "z", xlab = "Time")
plot(1:T, Y, type = "l", col = "blue", main = "Output (Y)", ylab = "Output", xlab = "Time")


