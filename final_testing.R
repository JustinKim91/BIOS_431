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


params1 = c(
  b = 0.5,        # Increased birth rate
  d = 0.02,       # Low death rate
  K = 10000,      # Carrying capacity set to initial population
  c = 0.0001,     # Very small carrying capacity effect
  B = 0.1,     # Much lower transmission rate
  w = 0.01,       # Low rate of losing immunity
  v = 0.05,       # Recovery rate
  k = 0,      # No additional removal rate
  r = 0.5,       # Moderate recovery/vaccination rate
  f = 0           # Fragmentation starts at 0
)


initials = c(S = 8000, I = 2000, R = 0)

frag_vec <- seq(0,1,0.01)

# Storage for results
results <- data.frame(
  Fragmentation = frag_vec,
  Susceptible = numeric(length(frag_vec)),
  Infected = numeric(length(frag_vec)),
  Recovered = numeric(length(frag_vec)),
  TotalPopulation = numeric(length(frag_vec))
)

# Simulation loop
for (i in 1:length(frag_vec)){
  params1["f"] = frag_vec[i]
  
  output = lsoda(
    y = initials, 
    times = seq(0, 100, by = 1), 
    func = SIR, 
    parms = params1
  )
  
  
  # Store final time point values
  results$Susceptible[i] = output[nrow(output), 2]
  results$Infected[i] = output[nrow(output), 3]
  results$Recovered[i] = output[nrow(output), 4]
  results$TotalPopulation[i] = sum(output[nrow(output), 2:4])
}

# Plotting
par(mfrow=c(2,1))
plot(results$Fragmentation, results$Susceptible, type = 'l', col = 'blue', 
     main = "Population Compartments vs Fragmentation",
     xlab = "Fragmentation", ylab = "Population")
lines(results$Fragmentation, results$Infected, col = 'red')
lines(results$Fragmentation, results$Recovered, col = 'green')
legend("topright", 
       legend = c("Susceptible", "Infected", "Recovered"),
       col = c("blue", "red", "green"),
       lty = 1)

plot(results$Fragmentation, results$TotalPopulation, type = 'l',
     main = "Total Population vs Fragmentation",
     xlab = "Fragmentation", ylab = "Total Population")


# Prettier Plots

pretty_plot <- ggplot(results, aes(x = Fragmentation)) +
  geom_line(aes(y = Susceptible), color = 'green') +
  geom_line(aes(y = Infected), color = 'red') +
  geom_line(aes(y = Recovered), color = 'purple') +
  xlab("Fragmentation") +
  ylab("Individuals")

pretty_plot





# More Testing

initials = c(S = 8000, I = 2000, R = 0)

params1 = c(
  b = 0.5,        # Increased birth rate
  d = 0.02,       # Low death rate
  K = 10000,      # Carrying capacity set to initial population
  c = 0.0001,     # Very small carrying capacity effect
  B = 0.1,     # Much lower transmission rate
  w = 0.01,       # Low rate of losing immunity
  v = 0.05,       # Recovery rate
  k = 0,      # No additional removal rate
  r = 0.5,       # Moderate recovery/vaccination rate
  f = 0           # Fragmentation starts at 0
)

test = lsoda(y = initials, times = seq(0, 100, by = 1), parms = params1, func = SIR)

test1_plot <- ggplot(test, aes(x = time)) +
  geom_line(aes(y = S), color = 'dodgerblue') + 
  geom_line(aes(y = I), color = 'darkred') +
  geom_line(aes(y = R), colour = 'purple') +
  xlab("Time") +
  ylab("Number of Individuals") +
  ggtitle("Disease Dynamic for No Fragmentation")

test1_plot





params2 = c(
  b = 0.5,        # Increased birth rate
  d = 0.01,       # Low death rate
  K = 10000,      # Carrying capacity set to initial population
  c = 0.0001,     # Very small carrying capacity effect
  B = 0.1,     # Much lower transmission rate
  w = 0.01,       # Low rate of losing immunity
  v = 0.05,       # Recovery rate
  k = 0,      # Very low additional removal rate
  r = 0.5,       # Moderate recovery/vaccination rate
  f = 1           # Fragmentation starts at 0
)


test2 = lsoda(y = initials, times = seq(0, 100, by = 1), parms = params2, func = SIR)

test2_plot <- ggplot(test2, aes(x = time)) +
  geom_line(aes(y = S), color = 'dodgerblue') + 
  geom_line(aes(y = I), color = 'darkred') +
  geom_line(aes(y = R), colour = 'purple') +
  xlab("Time") +
  ylab("Number of Individuals") +
  ggtitle("Disease Dynamic for Fragmentation")


test2_plot



