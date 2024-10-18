# BIOS 431 Emerging Infectious Diseases Lab 6 ----
# Justin Kim
# 10/18/2024

## Question 1 -----------------------------------------------------------------
library(network)

# Loading badgers.csv data

badger <- read.csv("../badgers.csv")

badger_matrix <- as.matrix(badger[,2:ncol(badger)])

badger_net <- network(
  badger_matrix,
  matrix.type = "adjacency",
  directed = FALSE,
  weighted = FALSE
)

plot(badger_net)

## Question 2 -----------------------------------------------------------------

# In an outbreak that spreads through a network, the transmission would be
# ... heterogeneous due to the the transmission patterns following the
# ... connectivity of the network. On the other hand, an outbreak in a
# ... homogeneous population would observe a uniform transmission pattern as
# ... each infected individual can infect anybody else equally.The degree of
# ... patient would significantly influence the scale of outbreak; in a network
# ... transmission system, the number of contacts that patient zero has greatly
# ... affects how likely the disease is to spread within the population. If
# ... patient zero has a high degree, they can infect a large amount of people
# ... very quickly, leading to a faster spread of the disease before immunities
# ... or treatments are developed. This can lead to a greater maximum number of
# ... infected individuals.

## Question 3 -----------------------------------------------------------------
library(ggplot2)

degrees_badgers <- function(edgelist) {
    # Finds number of nodes
    N <- attributes(edgelist)$n
    degrees <- numeric()
    # For each node, it checks how many times it is listed in an edge
    for (n in 1:N) {
        degrees[n] <- sum(edgelist == n)
    }
    data.frame("Node ID" = 1:N, "Degree" = degrees)
}

degree <- degrees_badgers(as.edgelist(badger_net))

badger_hist <- ggplot(degree, aes(x = Degree)) +
  geom_histogram(bins = 9) +
  xlab("Degree") +
  ylab("Frequency") +
  ggtitle("Degree Distribution")

badger_hist

# The distribution is relatively normal, but maybe slightly right-skewed given
# ... that there are more intermediate frequencies below degrees of 10 than
# ... above.

## Question 4 -----------------------------------------------------------------
library(epinet)

summarize_SIR <- function(edgelist, beta) {
    result <- SEIR.simulator(
        M = edgelist,
        N = attributes(edgelist)$n,
        beta,
        ki = 1,
        thetai = 2,
        latencydist = "fixed",
        latencyperiod = 0
    )
    ID_list <- degrees_badgers(edgelist)
    data.frame(
        "NodeID" = c(as.numeric(result[1, "Node ID"])),
        "Degree" = c(
            ID_list[ID_list$Node.ID == as.numeric(result[1, "Node ID"]),]$Degree
        ),
        "Proportion" = c(
            as.numeric(1 + length(
                na.omit(result[,"Parent"])
            )) / length(result[,"Parent"])
        )
    )
}


sim_SIR_network <- function(edgelist, beta, num_simulations) {
  result <- data.frame()
  for (i in 1:num_simulations) {
    output <- summarize_SIR(edgelist, beta)
    result <- rbind(result, output)
  }
  return(result)
}

sim_SIR_network(as.edgelist(badger_net), beta = 0.5, num_simulations = 10000)



