# BIOS 431 Emerging Infectious Diseases Lab 4 ----
# Justin Kim
# 10/04/2024

# Epidemiology ----

lab4 <- read.csv("../431_Lab_4.csv")
lab4
library(ggplot2)

### Part 1: Disease spread without immunity -----------------------------------

# Generate graphs

#### a ----

disease <- ggplot(lab4, aes(x = t)) +
                    geom_line(aes(y = It1), color = 'darkred') + 
                    geom_line(aes(y = St1), colour = 'dodgerblue') +
                    xlab("Time") +
                    ylab("Number of Individuals")
  
disease                  

#### b ----

diseas_prev <- ggplot(lab4, aes(x = t, y = It1/Nt1)) +
  geom_line() +
  xlab("Time")

diseas_prev


## Part 2: Disease Spread with immunity ---------------------------------------

# a ----

dis_imm <- ggplot(lab4, aes(x = t)) +
  geom_line(aes(y = St2/Nt2), color = 'dodgerblue') + 
  geom_line(aes(y = It2/Nt2), color = 'darkred') +
  geom_line(aes(y = Rt2/Nt2), colour = 'purple') +
  xlab("Time") +
  ylab("Number of Individuals")

dis_imm

## Part 3: Building an epidemiological model ----------------------------------

# B*St1*It1 = It1(t+1) - It1(t)

### Beta for first three steps of disease spread ------------------------------

B1 = (3-1)/(1*49)
# 0.04081633

B2 = (8-3)/(3*43)
# 0.03875969

B3 = (16-8)/(8*37)
# 0.02702703

Bavg = (B1 +B2 +B3)/3
# 0.03553435

### a ----

# Selecting the first three steps is not arbitrary given that it only models
# ... the early stages of the disease spread. The benefit of selecting another
# ... set of time periods is that it would give a more encompassing average
# ... for Beta that is represented of the whole transmission period. However,
# ... from only looking at average Beta, we would not be able to tell whether
# ... the value is skewed more towards the data from a specific period.


### b ----

# Beta for Part 1
Bavg
# 0.03553435


# R0 = St1(t = 1)(B/rr)
# In part 1, because there is no recovery, we cannot estimate an  R0, as R0
# ... depends on the recovery rate.


# Beta for Part 2
b1 = (2-1)/(1*49)
# 0.02040816
b2 = (10-5)/(5*37)
# 0.02702703
b3 = (14-10)/(10*30)
# 0.01333333

bavg = (b1 + b2 + b3)/3
# 0.02025617

# recovery rate = 1
# R0 for Recovering Population
R0 <- (49)*(bavg)/1
mean(R0)
# 0.9925526

### c ----

# As the size of the population increases, tracking the transmissions by
# ... individuals becomes very difficult. Thus, if there is an established
# ... Beta and a known recovery rate, it is much easier to calculate R0
# ... in this way.

### Question 2 ----

library(deSolve)

dS = function (t, state, parameters){
  with(
    as.list(c(state, parameters)),
    {
      dSdt = (-B)*S*I
      dIdt = B*S*I - r*I
      dRdt = r*I
      res = c(dSdt, dIdt, dRdt)
      list(res)
    }
  )
}

#Parameters for No recovery
initials = c(S = 49, I = 1, R = 0)
params = c(B = 0.03553435, r = 0)
t = 0:11

ds_no_rec = lsoda(y = initials, times = t, parms = params, func = dS)

#Parameters for with Recovery
initials = c(S = 49, I = 1, R = 0)
params2 = c(B = 0.02025617, r = 1)
t = 0:11

ds_rec = lsoda(y = initials, times = t, parms = params2, func = dS)

# Graphs

# Disease Dynamics for No Recovery
dis_dyn_1 <- ggplot(ds_no_rec, aes(x = t)) +
  geom_line(aes(y = S), color = 'dodgerblue') + 
  geom_line(aes(y = I), color = 'darkred') +
  geom_line(aes(y = R), colour = 'purple') +
  xlab("Time") +
  ylab("Number of Individuals") +
  ggtitle("Disease Dynamic for No Recovery")

dis_dyn_1

# Disease Dynamics for With Recovery
dis_dyn_2 <- ggplot(ds_rec, aes(x = t)) +
  geom_line(aes(y = S), color = 'dodgerblue') + 
  geom_line(aes(y = I), color = 'darkred') +
  geom_line(aes(y = R), colour = 'purple') +
  xlab("Time") +
  ylab("Number of Individuals") +
  ggtitle("Disease Dynamic With Recovery")

dis_dyn_2

#### Did the model predict the course of the epidemic? ----
# The model predicted the first case relatively well, but seemed to fail pretty
# ... significantly in predicting the disease dynamic for transmission with
# ... recovery. This is likely because in the practice of spreading the disease
# ... in Part 2, I spread the infection whenever it was possible, which may
# ... have artificially simulated transmission, while the calculated R0 was
# ... less than 0, so the model would predict that there would be no
# ... significant transmission.


#### Two reasons why the prediction is only approximate ----
# One reason that the prediction is only approximate is because we assume that
# ... values such as transmission rates or recovery rates are constant
# ... throughout the duration of our modeling. Additionally, our model does not
# ... account for several variables such as incubation time, co-infections, etc.
# ... Thus, the predictions are approximates based on the assumption of constant
# ... values and oversimplification of transmission dynamics.

