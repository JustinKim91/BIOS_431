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

# To simulate the indefinite growth, the birth rate parameter should exceed the
# ... death rate parameter, and vice versa for for the crashing
crash_sim = lsoda(y = initials, times = t, parms = params_crash, func = exponential_growth)
plot(crash_sim)

## Question 2 ----

### a ----

# Change the dNdt parameter to reflect competition by introducing a*N and c*N
# ... which represent competition to simulate max birth/death rate by decreasing
# ... and increasing birth and death rate, respectively, as N increases.
logistic_growth = function (t, y, parameters){
  N = y[1];
  with(
    as.list(parameters),
    {
      dNdt = ((b-a*N)-(d+c*N))*N
      res = c(dNdt)
      list(res)
    }
  )
}

# Population that reaches stable positive value
initials_log_grow = c(N=1000)
params_logistic = c(a = 0.000001, b = 0.5, c = 0.0000001, d = 0.4)
t_log = 0:200

log_grow_sim = lsoda(y = initials_log_grow, times = t_log, parms = params_logistic, func = logistic_growth)
plot(log_grow_sim)

# Population that crashes to 0
initials_log_crash = c(N=100)
params_logistic_crash = c(a = 0.0001, b = 0.5, c = 0.001, d = 0.4)
t = 0:200

log_crash_sim = lsoda(y = initials_log_crash, times = t, parms = params_logistic_crash, func = logistic_growth)
plot(log_crash_sim)

### b ----

# N* = 90909.09, which we will round to 90910.
plot(log_grow_sim)
abline(h = 90910,col = 'red')

# The population's trajectory is limited by the equilibrium value, which is the
# ... carrying capacity. This carrying capacity is the function of the simulated
# ... competition. The equilibrium looks stable and would work for any other
# ... initial populations, given that the equilibrium depends only upon the
# ... max birth/death rates and the competition parameters (a and c).

### c ----

# If I varied c, while keeping the max birth and death rates constant, N* would
# ... be inversely proportional to c. This is because, N*, the carrying capacity
# ... , is the population at which the change in population becomes 0. In other
# ... words, (b-a*N)-(d+c*N) = 0. This means that N* = (b-d)/(c+a). Thus, if c
# ... increases, N* would decrease. If c decreases, N* would increase.
