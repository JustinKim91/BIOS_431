

# install.packages("ggthemes")

library(deSolve)
library(ggplot2)
library(ggthemes)




SIR = function (t, state, parameters){
  with(
    as.list(c(state, parameters)),
    {
      # Total population
      N = S + I + R
      
      # Adjusted recovery rate with fragmentation
      r_adj <- r * (1 - f)
      
      # Birth rate that depends on current population
      birth_term = b * (1 - N / K)
      
      dSdt = birth_term * N - (d + k + r_adj)*S - B*S*I/N + w*R
      dIdt = B*S*I/N - (d + v + k)*I
      dRdt = r_adj*S - (w + d + k)*R
      
      res = c(dSdt, dIdt, dRdt)
      list(res)
    }
  )
}



# Parameters
initials = c(S = 8000, I = 2000, R = 0)
params1 = c(b = 0.5, d = 0.02, K = 10000, c = 0.0001, B = 0.3, 
            w = 0.01, v = 0.05, k = 0, r = 0.5, f = 0)
t = 0:100

# Sequencing a fragmentation vector from o to 1 in increments of 0.01

frag_vec <- seq(0,1,0.01)

# Creating empty vectors for S, R, and I values for the end of simulation

Susceptible = numeric()
Infected = numeric()
Recovered = numeric()

# For loop iterating across f
for (i in 1:length(frag_vec)){
  params1["f"] = frag_vec[i]
  output = lsoda(y = initials, times = t, 
                 func = SIR, parms = params1)
  
  Susceptible[i] = output[101, 2]
  Infected[i] = output[101, 3]
  Recovered[i] = output[101, 4]
  
}

# SIR data
data1 = lsoda(y = initials, times = t, 
              func = SIR, parms = params1)


# Creating a data frame to store the densities and frag values
results = data.frame(frag_vec, Susceptible, Infected, Recovered)
results


amph_plot <- ggplot(results, aes(x = frag_vec)) +
  geom_line(aes(y = Susceptible, color = "Susceptible"), linewidth = 2) +
  geom_line(aes(y = Infected, color = "Infected"), linewidth = 2) +
  geom_line(aes(y = Recovered, color = "Recovered"), linewidth = 2) +
  ggtitle("SIR Population vs Fragmentation") +
  xlab("Degree of Fragmentation") +
  ylab("Individuals") +
  scale_colour_manual(name = "Legend",
                      values = c('Susceptible' = 'green', 
                                 'Infected' = 'red', 
                                 'Recovered' = 'purple')) +
  theme(plot.title = element_text(size = 18, face = "bold"),  
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position.inside = c(0.85, 0.75)
      )

amph_plot  

  
  

# Just SIR, varying fragmentation


initials = c(S = 8000, I = 2000, R = 0)
t = 0:100
  
# Two Sets of Parameters
params1 = c(b = 0.5, d = 0.02, K = 10000, c = 0.0001, B = 0.3, 
            w = 0.01, v = 0.05, k = 0, r = 0.5, f = 0)
  


params2 = c(b = 0.5, d = 0.02, K = 10000, c = 0.0001, B = 0.3, 
            w = 0.01, v = 0.05, k = 0, r = 0.5, f = 1)


# SIR Data

data1 = lsoda(y = initials, times = t, 
              func = SIR, parms = params1)

data2 = lsoda(y = initials, times = t, 
                      func = SIR, parms = params2)

# Plots

d1_plot <- ggplot(data1, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 2) +
  geom_line(aes(y = I, color = "Infected"), linewidth = 2) +
  geom_line(aes(y = R, color = 'Recovered'), linewidth = 2) +
  ggtitle("SIR Population with No Fragmentation") +
  xlab("Time") +
  ylab("Individuals") +
  scale_colour_manual(name = "Legend",
                      values = c('Susceptible' = 'green', 
                                 'Infected' = 'red', 
                                 'Recovered' = 'purple')) +
  theme(plot.title = element_text(size = 18, face = "bold"),  
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position.inside = c(0.85, 0.75)
  )

d1_plot

d2_plot <- ggplot(data2, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible"), linewidth = 2) +
  geom_line(aes(y = I, color = "Infected"), linewidth = 2) +
  geom_line(aes(y = R, color = 'Recovered'), linewidth = 2) +
  ggtitle("SIR Population with Fragmentation") +
  xlab("Time") +
  ylab("Individuals") +
  scale_colour_manual(name = "Legend",
                      values = c('Susceptible' = 'green', 
                                 'Infected' = 'red', 
                                 'Recovered' = 'purple')) +
  theme(plot.title = element_text(size = 18, face = "bold"),  
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position.inside = c(0.85, 0.75)
  )

d2_plot
  

