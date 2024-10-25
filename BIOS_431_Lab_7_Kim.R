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

params = c(b = 0.2, d = 0.05, K= 150, beta11 = 0.0001, beta22 = 0.001, beta12=0.000,
           beta21=0.000, v = 0.001)
initial = c(S1=100, I1=0.1, S2=100, I2=0.1)
timespan=500 #duration of the simulation
t = seq(0,timespan,0.1) #created a sequence of time steps
steps=length(t) #we keep track of time steps in case we want to change the duration

mh_SI <- lsoda(y = initial, times = t, parms = params, func = MultiSI)
mh_SI

