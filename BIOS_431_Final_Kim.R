



library(deSolve)
library(ggplot2)




SIR = function (t, state, parameters){
  with(
    as.list(c(state, parameters)),
    {
      r_adj <- r*(1-f)
      
      dSdt = (b*(1 - c*(S + I + R))) * (S + I + R) - (d + k + r)*S - B*S*I + w*R
      dIdt = B*S*I - (d + v + k)*I
      dRdt = r_adj*S - (w + d + k)*R
      res = c(dSdt, dIdt, dRdt)
      list(res)
    }
  )
}


# Parameters
initials = c(S = 99, I = 1, R = 0)
params1 = c(b = 0.5, d = 0.05, c = 0.01, B = 0.01, 
            w = 0.05, v = 0.1, k = 0, r = 0.5, f = 0)
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
  geom_line(aes(y = Susceptible), color = "darkgreen") +
  geom_line(aes(y = Infected), color = "red") +
  geom_line(aes(y = Recovered), color = "purple") +
  ggtitle("SIR Population vs Fragmentation") +
  xlab("Degree of Fragmentation") +
  ylab("Individuals")

amph_plot  
  
  
  
  
  
  
  

