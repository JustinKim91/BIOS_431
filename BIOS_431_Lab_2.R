# BIOS 431 Emerging Infectious Diseases Lab 2 ----
# Justin Kim
# 09/13/2024

library(deSolve)

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