# FM 413 Lecture 2

# Load required libraries
library(tidyverse)

# 1. Samuelson Model

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

# Combine data for plotting
samuelson_data <- data.frame(Time = 1:T, Output = Y, Consumption = C, Investment = I) %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "Value")

# Plot results with ggplot2
ggplot(samuelson_data, aes(x = Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Samuelson Model", x = "Time", y = "Value") +
  theme_minimal()

# 2. Supply-Side Samuelson Model

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

# Combine data for plotting
supply_data <- data.frame(Time = 1:T, Output = Y, Capital = K) %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "Value")

# Plot results with ggplot2
ggplot(supply_data, aes(x = Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Supply-Side Samuelson Model", x = "Time", y = "Value") +
  theme_minimal()

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

# Truncate K to length T
K <- K[1:T]

# Combine data for plotting
rbc_deterministic_data <- data.frame(Time = 1:T, Consumption = C, Labor = L, Capital = K) %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "Value")

# Plot results with ggplot2
ggplot(rbc_deterministic_data, aes(x = Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Deterministic RBC Model", x = "Time", y = "Value") +
  theme_minimal()


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

# Combine data for plotting
stochastic_rbc_data <- data.frame(Time = 1:T, Productivity = z, Output = Y) %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "Value")

# Plot results with ggplot2
ggplot(stochastic_rbc_data, aes(x = Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(title = "Stochastic RBC Model", x = "Time", y = "Value") +
  theme_minimal()

#Impulse response functions

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Parameters
T <- 20              # Time periods for IRFs
beta <- 0.96         # Discount factor
alpha <- 0.33        # Capital share in production
rho <- 0.9           # Persistence of productivity shock
sigma <- 0.02        # Standard deviation of productivity shock
nu <- 1              # CRRA coefficient for consumption
eta <- 1.5           # Weight on leisure
shock_time <- 5      # Time of the productivity shock

# Simulate RBC model with one-time productivity shock
simulate_irf <- function(depreciation_rate) {
  # Initialize variables
  K <- rep(5, T + 1)    # Capital stock (initial value)
  C <- rep(0, T)        # Consumption
  I <- rep(0, T)        # Investment
  H <- rep(0.3, T)      # Hours worked
  Y <- rep(0, T)        # Output
  Z <- rep(0, T)        # Productivity (log-level)
  
  # Introduce a one-time shock at `shock_time`
  Z[shock_time] <- sigma
  
  # Solve the model
  for (t in 1:T) {
    # Marginal utility of consumption and labor-leisure trade-off
    U_C <- function(C) C^(-nu)
    U_L <- function(H) eta * (1 - H)^(-nu)
    
    # Marginal product of labor and capital
    MPL <- function(K, H, Z) (1 - alpha) * exp(Z) * K^alpha * H^(-alpha)
    MPK <- function(K, H, Z) alpha * exp(Z) * K^(alpha - 1) * H^(1 - alpha)
    
    # Labor supply condition
    H[t] <- optimize(
      function(h) abs(U_L(h) - MPL(K[t], h, Z[t]) / U_C((1 - depreciation_rate) * K[t] + MPK(K[t], h, Z[t]))),
      interval = c(0.01, 0.99)
    )$minimum
    
    # Output
    Y[t] <- exp(Z[t]) * K[t]^alpha * H[t]^(1 - alpha)
    
    # Investment and consumption
    I[t] <- Y[t] - C[t]
    C[t] <- Y[t] - depreciation_rate * K[t]
    
    # Capital accumulation
    if (t < T) {
      K[t + 1] <- beta * MPK(K[t], H[t], Z[t]) * K[t] + (1 - depreciation_rate) * K[t]
    }
  }
  
  # Combine results into a data frame
  data.frame(
    Time = 1:T,
    Capital = K[1:T],
    Investment = I,
    Consumption = C,
    Output = Y,
    HoursWorked = H,
    Productivity = exp(Z)  # Convert log productivity to levels
  )
}

# Simulate for 100% depreciation
irf_full_depreciation <- simulate_irf(depreciation_rate = 1)

# Scale results to percentage deviations from pre-shock levels
calculate_deviations <- function(data, shock_time) {
  pre_shock <- data[shock_time - 1, ]
  data %>%
    mutate(across(-Time, ~ 100 * (. - pre_shock[.col]) / pre_shock[.col]))
}

# Scale and format
scaled_irf_full <- calculate_deviations(irf_full_depreciation, shock_time)

# Pivot for ggplot
scaled_irf_full_long <- scaled_irf_full %>%
  pivot_longer(cols = -Time, names_to = "Variable", values_to = "Value")

# Plot IRFs
ggplot(scaled_irf_full_long, aes(x = Time, y = Value, color = Variable)) +
  geom_line(size = 1) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "IRFs for nu = 1, 100% Depreciation",
    x = "Time",
    y = "Percentage Deviation",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

