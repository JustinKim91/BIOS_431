# BIOS 431 Emerging Infectious Diseases Lab 2 ----
# Justin Kim
# 09/13/2024


## Simulating a model in R ----

# Define Growth Model

exponential_growth = function (t, y, parameters){
  N = y[1];
  with(
    as.list(parameters),
    {
      dNdt = (b-d)*N
      res = c(dNdt)
      list(res)
    }
  )
}

# Load package
library(deSolve)

# Parameters

initials = c(N=10)
params = c(b = 0.3, d = 0.1)
t = 0:100

# Simulation

exp_grow_sim = lsoda(y = initials, times = t, parms = params, func = exponential_growth)

exp_grow_sim
plot(exp_grow_sim)
# abline(h = 2000000000)

# Model Check

N0 = 10
b = 0.3
d = 0.1
curve(N0*exp((b-d)*x), from = 0, to = 100, col = 'red', add = T)

## Question 1 ----

# Population that grows indefinitely
initials = c(N=10)
params = c(b = 0.3, d = 0.1)
t = 0:100

plot(exp_grow_sim)


# Population that crashes
initials = c(N=10)
params_crash = c(b = 0.3, d = 0.4)
t = 0:100

crash_sim = lsoda(y = initials, times = t, parms = params_crash, func = exponential_growth)
plot(crash_sim)
