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

### 1 ----
# The histogram of x displays a relatively uniform bell-shaped curve
hist(x)

### 2 ----

y_true <- (2*x) + 10

### 3 ----

y_obs <- rnorm(1000, mean = y_true, sd = 2)

### 4 ----
# library(ggplot2)

severity <- data.frame(x, y_obs)

severity_plot <- ggplot(severity, aes(x = x, y = y_obs)) + 
  geom_point() +
  xlab("Trait") +
  ylab("Disease Severity")

severity_plot

### 5 ----

severity_plot_lr <- severity_plot + geom_smooth(method = "lm", formula = y ~ x)
model_1 <- lm(y_obs ~ x)
summary(model_1)

# The summary yields an estimate for the relationship as y_obs = 2.036x + 9.951
# ... , which is pretty similar to the perfect model of 2x + 10.Furthermore, the
# ... standard error for these coefficients are around 0.06, while the p-values
# ... are highly significant. Thus, this regression correctly estimates the
# ... relationship between the trait and the disease severity.

### 6 ----

# I previously already completed this step, in order to create a scatter plot
# ... using ggplot2, which I named "severity"
disease.data <- data.frame(x, y_obs)




