# BIOS 431 Emerging Infectious Diseases Lab 5 ----
# Justin Kim
# 10/11/2024

## Question 1 -----------------------------------------------------------------

vaccination_and_culling = function (t, state, parameters){
  with(
    as.list(c(state, parameters)),
    {
      dSdt = (b*(1 - c*(S + I + R))) * (S + I + R) - (d + k + r)*S - B*S*I + w*R
      dIdt = B*S*I - (d + v + k)*I
      dRdt = r*S - (w + d + k)*R
      res = c(dSdt, dIdt, dRdt)
      list(res)
    }
  )
}

## Question 2 -----------------------------------------------------------------

library(deSolve)

# Parameters
initials = c(S = 99, I = 1, R = 0)
params1 = c(b = 0.5, d = 0.05, c = 0.01, B = 0.01, 
            w = 0.05, v = 0.15, k = 0, r =0)
t = 0:100

# Sequencing a gamma vector from 0 to 1 by increments of 0.01
gamma_vec <- seq(0,1,0.01)

# Creating empty vectors for (S + R) and I values for the end of simulation

S_R_1 = numeric()
Infected_1 = numeric()

# For loop iterating across r
for (i in 1:length(gamma_vec)){
  params1["r"] = gamma_vec[i]
  output = lsoda(y = initials, times = t, 
                 func = vaccination_and_culling, parms = params1)
  
  S_R_1[i] = output[101, 2] + output[101, 4]
  Infected_1[i] = output[101, 3]
  
}

# Creating a data frame to store the densities and gamma values
results = data.frame(gamma_vec, S_R_1, Infected_1)
results

# Plotting the densities against vaccination rate

library(ggplot2)

density_plot <- ggplot(results, aes(x = gamma_vec)) +
  geom_line(aes(y = S_R_1), color = "darkgreen") +
  geom_line(aes(y = Infected_1), color = "red") +
  ggtitle("SIR Population vs Vaccination Rate") +
  xlab("Vacination Rate") +
  ylab("Individuals")

density_plot

# The level of vaccination, r, required to eliminate the parasite would be when
# ... the Infected_1 < 1. This is of course, assuming that we are dealing with
# ... integer values in real life. The first time which the Infected_1 value
# ... drops below 1 occurs at r = 0.38.

## Question 3 -----------------------------------------------------------------

# Parameters
initials = c(S = 99, I = 1, R = 0)
params1 = c(b = 0.5, d = 0.05, c = 0.01, B = 0.01, 
            w = 0.05, v = 0.15, k = 0, r =0)
t = 0:100

# Sequencing a k vector from 0 to 1 by increments of 0.01
k_vec <- seq(0,1,0.01)

# Creating empty vectors for S and I values for the end of simulation

S_2 = numeric()
Infected_2 = numeric()

# For loop iterating across k
for (i in 1:length(k_vec)){
  params1["k"] = k_vec[i]
  output = lsoda(y = initials, times = t, 
                 func = vaccination_and_culling, parms = params1)
  
  S_2[i] = output[101, 2]
  Infected_2[i] = output[101, 3]
  
}

# Creating a data frame to store the densities and k values
results2 = data.frame(k_vec, S_2, Infected_2)

culling_plot <- ggplot(results, aes(x = k_vec)) +
  geom_line(aes(y = S_2), color = "darkgreen") +
  geom_line(aes(y = Infected_2), color = "red") +
  ggtitle("SI Population vs Culling Rate") +
  xlab("Culling Rate") +
  ylab("Individuals")

culling_plot

# According to the culling_plot and results2 data frame, in a model where there
# ... is no vaccination, at t = 100, there is no need for culling as there are
# ... no infected individuals in the population, as most have presumably died.
# ... Thus, the value of k necessary to eliminate the parasite would be k = 0.
# ... I would choose the vaccination strategy since the culling model cripples
# ... the population; because there is no recovery via vaccinations, each time
# ... an individual is infected, that individual eventually dies. As observed
# ... in the plots for vaccination and culling, with vaccination, at t = 100,
# ... there may be up to 90 susceptible or recovered individuals, while in the 
# ... culling model, there is a projected maximum of 12 susceptible individuals.


## Question 4 -----------------------------------------------------------------

# Creating new gamma and k vectors to loop over all 10,201 combinations of k and r
new_gamma_vec <- rep(gamma_vec, times = 101)
new_k_vec <- rep(k_vec, times = 101)

# Defining an empty matrix to house the values of S + R for every k and r combo
S_R_2 = matrix(0, nrow = length(k_vec), ncol = length(gamma_vec))

# For loop iterating across k
for (i in 1:length(k_vec)){
  for(j in 1:length(gamma_vec)) {
  params1["k"] = new_k_vec[i]
  params1["r"] = new_gamma_vec[j]
  output = lsoda(y = initials, times = t, 
                 func = vaccination_and_culling, parms = params1)
# Assigning S + R values to each cell in the matrix based on k and r  
  S_R_2[i, j] = output[101, "S"] + output[101, "R"]

  }  
}


mylevels = seq(0, 100, 10)

filled.contour(x = k_vec, y = gamma_vec, z = S_R_2, levels = mylevels)

# The contour shows the density of uninfected hosts is most strongly influenced
# ... by the vaccination rates. In fact, as the culling rate increases, the
# ... density of uninfected hosts actually seem to decrease. Thus, I would opt
# ... for a purely vaccination based approach.

