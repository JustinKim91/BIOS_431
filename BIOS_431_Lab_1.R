# BIOS 431 Emerging Infectious Diseases Lab 1 ----
# Justin Kim
# 08/20/2024

## Basic Computations and Commands ----

5+15
100 - 25/5^2
5*10
100^0.5

## Functions ----



exp(1)^2 # Can also input as exp(2)
sqrt(5)
log(50)
cos(0.5)


## Function Names ----

# What Function performs a t-test in R?
# t.test()

# What function performs a linear model in R?
# lm()

## deSolve ----

# Commented out the install.packages so that the command isn't executed every 
# ... time the document is Run in full

# install.packages("deSolve")
library(deSolve)

## How to use a known function ----

?mean
??mean

## Functions (cont.) ----
# Some functions require more than one argument

seq(from = 1, to = 10, by = 1)


## Assigning Variables ----

d <- 1:12; d

## Matrices ----


## Random Number Generation (1 Point) ----

x <- rnorm(1000, mean = 0, sd = 1)
x

## Visualizing, analyzing, and exporting data (4 points) ----

# The histogram of x displays a relatively uniform bell-shaped curve
hist(x)
