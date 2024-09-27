# BIOS 431 Emerging Infectious Diseases Lab 3 ----
# Justin Kim
# 09/27/2024

# Transmission Dynamics of Robo Pox ----

## Question 1 -----------------------------------------------------------------

# The major assumption about host-host contact process that distinguishes
# ... density-dependent and frequency-dependent transmission is that the
# ... contact rate increases proportionally with the population density in
# ... density-dependent transmission, while it remains constant in frequency-
# ... dependent transmission.

## Question 2 -----------------------------------------------------------------

# Our design for the Robo Pox experiment was to run 3 trials of a given
# ... population density on a tray. To test for density-dependent transmission,
# ... we conducted three trials on population densities of 2, 3, 5, 7, and 10.
# ... (Note: We only had time to conduct one trial the population density of 10)
# ... For the densities of 7 and 10, we split the tray into four quadrants and
# ... each member of the group counted the instances of contact in their
# ... respective quadrants.

## Question 3 -----------------------------------------------------------------

robo <- read.csv("../robo_data.csv")
robo

# Note: We were unable to conduct 20 trials that were at least 30 seconds long,
# ... due to running out of time.

## Question 4 -----------------------------------------------------------------
library(ggplot2)

# Create Plot for Data

robo_plot <- ggplot(robo, aes(x = number.of.individuals, y = total.contacts)) +
  geom_point()+
  xlab("Number of Individuals") +
  ylab("Total Contact (per 20 seconds)")
  
robo_plot


# Fit linear regression

robo_lr <- robo_plot + geom_smooth(method = "lm", formula = y ~ x)
robo_lr

# During our experiment, we recorded the total number of contacts within 20 
# ... second periods. According to the linear regression model, as the number of  
# ... individuals present on the tray increased, the contact rate increased 
# ... proportionally. 

## Question 5 -----------------------------------------------------------------

# I would like to test the hypothesis that the infectiousness of the Robo pox
# ... affects the transmission rate of the disease. To represent the 
# ... infectiousness, i, we would vary the number of contacts necessary for an
# ... individual to become infected. The experimental set up would call for a 
# ... fixed population density while varying the number of contact required. 
# ... We would run three trials for each i and estimate the rate of infection 
# ... by either dividing the total contact by i or, by keeping track of how many 
# ... times an individual makes contact. I predict that, as i increases, the 
# ... infectiousness of Robo Pox will decline. The only materials necessary to
# ... conduct this experiment are the tray and the HexBugs.




