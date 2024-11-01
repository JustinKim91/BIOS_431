# BIOS 431 Emerging Infectious Diseases Lab 7 ----
# Justin Kim
# 11/01/2024

## Question 1 -----------------------------------------------------------------

library(deSolve)

MultiSI<-function (t, y, parameters){
  S1 = y[1]; I1 = y[2]; S2 = y[3]; I2 = y[4];
  with(
    as.list(parameters),
    {
      # We just expand our simple old density-dependent SI model to include another host
      # So that means we need two groups of susceptible and infected for host 1 and host 2
      dS1dt = b*(S1+I1)*(1-(S1+I1)/K)-beta11*S1*I1-beta12*S1*I2-d*S1
      dI1dt = beta11*S1*I1+beta12*S1*I2-(d+v)*I1
      dS2dt = b*(S2+I2)*(1-(S2+I2)/K)-beta22*S2*I2-beta21*S2*I1-d*S2
      dI2dt = beta22*S2*I2+beta21*S2*I1-(d+v)*I2
      res = c(dS1dt,dI1dt,dS2dt,dI2dt)
      list(res)
    }
  )
}

# The function above models a multi-host SI-model; the state variables of the model are 
# ... identified as S1, S2, I1, and I2.The S variables describes the susceptible population
# ... in either host; the I variables describe the infected population of the hosts.
# ... For the parameters, b and d represent the birth and death rates, respectively.
# ... K is the carrying capacity. The beta parameters quantify the transmission rates
# ... between hosts; beta11 describes the transmission of the infection from an
# ... individual from host 1 to another in host 1. beta12 describes the transmission
# ... rate from host 1 to host 2... and so on. Our model makes some assumptions, such
# ... as the assumption that infected and susceptible individuals reproduce at the same
# ... rate, or that the birth and death rates are the same between the two hosts.


## Question 2 -----------------------------------------------------------------
library(ggplot2)

params = c(b = 0.2, d = 0.05, K= 150, beta11 = 0.0001, beta22 = 0.001, beta12=0.000,
           beta21=0.000, v = 0.001)
initial = c(S1=100, I1=0.1, S2=100, I2=0.1)
timespan=500 #duration of the simulation
t = seq(0,timespan,0.1) #created a sequence of time steps
steps=length(t) #we keep track of time steps in case we want to change the duration

mh_SI <- data.frame(lsoda(y = initial, times = t, parms = params, func = MultiSI))
mh_SI


Q2_plot <- ggplot(mh_SI, aes(x = time)) +
  geom_line(aes(y = S1, colour = 'Species 1 Susceptible')) +
  geom_line(aes(y = I1, colour = 'Species 1 Infected')) +
  geom_line(aes(y = S2, colour = 'Species 2 Susceptible')) +
  geom_line(aes(y = I2, colour = 'Species 2 Infected')) +
  xlab("Time") +
  ylab("Population") +
  ggtitle("Multi-Host SI-Model") +
  scale_colour_manual(name = "Legend",
                      values = c('Species 1 Susceptible' = 'dodgerblue', 
                                 'Species 1 Infected' = 'red', 
                                 'Species 2 Susceptible' = 'darkgreen', 
                                 'Species 2 Infected' = 'purple')) +
  theme(legend.position = c(0.85, 0.75))

Q2_plot


### a -----

# Based on the results of the simulation, I would predict that species 2 poses
# ... a greater risk of spillover than species 1. This is because, in the 
# ... species 1 population, there is a very low prevalence of the infected
# ... population, whereas the infected population grows to a very large proportion
# ... of the species 2 population. This indicates that the population of species
# ... sustains a greater level of infected population, making it more likely for
# ... a cross-species infection to go from species 2 to species 1.

### b ----

# The conditions in which the transmission from species 1 to 2 is nonzero
params_12 = c(b = 0.2, d = 0.05, K= 150, beta11 = 0.0001, beta22 = 0.001, beta12=0.001,
            beta21=0.000, v = 0.001)

test_12 = data.frame(lsoda(y = initial, times = t, parms = params_12, func = MultiSI))


# The conditions in which the transmission from species 2 to 1 is nonzero
params_21 = c(b = 0.2, d = 0.05, K= 150, beta11 = 0.0001, beta22 = 0.001, beta12=0.000,
              beta21=0.001, v = 0.001)

test_21 = data.frame(lsoda(y = initial, times = t, parms = params_21, func = MultiSI))

# Plot for when the transmission from species 1 to 2 is nonzero

test_plot1 <- ggplot(test_12, aes(x = time)) +
  geom_line(aes(y = S1, colour = 'Species 1 Susceptible')) +
  geom_line(aes(y = I1, colour = 'Species 1 Infected')) +
  geom_line(aes(y = S2, colour = 'Species 2 Susceptible')) +
  geom_line(aes(y = I2, colour = 'Species 2 Infected')) +
  xlab("Time") +
  ylab("Population") +
  ggtitle("Multi-Host SI-Model") +
  scale_colour_manual(name = "Legend",
                      values = c('Species 1 Susceptible' = 'dodgerblue', 
                                 'Species 1 Infected' = 'red', 
                                 'Species 2 Susceptible' = 'darkgreen', 
                                 'Species 2 Infected' = 'purple')) +
  theme(legend.position = c(0.85, 0.75))

test_plot1

# Plot for when the transmission from species 2 to 1 is nonzero

test_plot2 <- ggplot(test_21, aes(x = time)) +
  geom_line(aes(y = S1, colour = 'Species 1 Susceptible')) +
  geom_line(aes(y = I1, colour = 'Species 1 Infected')) +
  geom_line(aes(y = S2, colour = 'Species 2 Susceptible')) +
  geom_line(aes(y = I2, colour = 'Species 2 Infected')) +
  xlab("Time") +
  ylab("Population") +
  ggtitle("Multi-Host SI-Model") +
  scale_colour_manual(name = "Legend",
                      values = c('Species 1 Susceptible' = 'dodgerblue', 
                                 'Species 1 Infected' = 'red', 
                                 'Species 2 Susceptible' = 'darkgreen', 
                                 'Species 2 Infected' = 'purple')) +
  theme(legend.position = c(0.85, 0.75))

test_plot2



