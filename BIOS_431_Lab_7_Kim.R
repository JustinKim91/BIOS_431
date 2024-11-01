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
# ... rate from host 2 to host 1... and so on. the v variable describes the
# ... recovery rate of the infected populations. Our model makes some assumptions, such
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
params_21 = c(b = 0.2, d = 0.05, K= 150, beta11 = 0.0001, beta22 = 0.001, beta12=0.000,
            beta21=0.001, v = 0.001)

test_21 = data.frame(lsoda(y = initial, times = t, parms = params_21, func = MultiSI))


# The conditions in which the transmission from species 2 to 1 is nonzero
params_12 = c(b = 0.2, d = 0.05, K= 150, beta11 = 0.0001, beta22 = 0.001, beta12=0.001,
              beta21=0.000, v = 0.001)

test_12 = data.frame(lsoda(y = initial, times = t, parms = params_12, func = MultiSI))

# Plot for when the transmission from species 1 to 2 is nonzero

test_plot1 <- ggplot(test_21, aes(x = time)) +
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

test_plot2 <- ggplot(test_12, aes(x = time)) +
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

# The results of the new simulations and graphs do match my prediction; as beta12
# ... was increased to 0.001, there was an increase in the Species 1 infected
# ... population.


## Question 3 -----------------------------------------------------------------

### a ----

# The intraspecific transmission in Species 2 directly affects the prevalence
# ... of infection in Host species 2, as seen in the formula:

# dI2dt = beta22*S2*I2+beta21*S2*I1-(d+v)*I2

# With time, the I2 value will increase or decrease depending on beta22. The
# ... infected population of host species 2 in turn is a direct factor in the
# ... modeling of the infected population of species 1, as seen in the formula:

# dI1dt = beta11*S1*I1+beta12*S1*I2-(d+v)*I1

# Here, we can see that I2 is a variable within the modeling of species 1
# ... infected population. Thus, I would reason that, increasing beta22 would
# ... eventually cause an increase in I1.


### b ----

#initialize vectors to keep track of state variables (final number of infects and susceptibles)
F_S1=numeric()
F_S2=numeric()
F_I1=numeric()
F_I2=numeric()

betaX_vector = seq(from=0, to=0.005, by=0.0001)
betaY_vector = seq(from=0, to=0.005, by=0.0001)

new_betaX_vector = rep(betaX_vector, each=51) # Vector for different interspecific transmission
new_betaY_vector = rep(betaY_vector, times=51) # Vector for different intraspecific transmission

for (i in 1:2601){
  params["beta12"] = new_betaX_vector[i]
  params["beta22"] = new_betaY_vector[i]
  output = lsoda(y = initial, times = t, func = MultiSI, parms = params)
  F_S1[i]=output[steps, "S1"]
  F_I1[i]=output[steps, "I1"]
  F_S2[i]=output[steps, "S2"]
  F_I2[i]=output[steps, "I2"]
}

PropInf_S1=100*F_I1/(F_S1+F_I1)
output = matrix(PropInf_S1, nrow=51, ncol=51, byrow = T) # Turn data into matrix for plotting
mylevels = seq(0,100, 10) # For shading the plot

#plot data
filled.contour(x=betaX_vector, y=betaY_vector, z=output, levels = mylevels, xlab="Intraspecific
transmission (beta22)", ylab="Interspecific transmission (beta12)")

# As I predicted, the intraspecific transmission within host species 2 is also
# ... important for understanding the spillover to host species 1. As we have
# ... already shown, when the interspecific transmission from species 2 to
# ... species 1, beta12, is increased, we see observe an increase in the
# ... infection prevalence in species 1. Above a certain beta12 threshold,
# ... which I will estimate to be around beta12 = 0.0006, we can see that, as
# ... the intraspecific transmission within host species 2, beta22, increases,
# ... the proportion of the infected species 1 population increases.Thus, the
# ... condition for spillover to occur is that beta12 is greater than 0.0006,
# ... and increasing beta22 maximizes the spillover. All in all, the results
# ... support my prediction that increased beta22 will increase I1.






