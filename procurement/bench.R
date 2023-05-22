# Set the parameters
n <- 3L  # number of options
v <- c(2., 3., 4.)  # pre-paid reservation costs
w <- c(5., 7., 6.)  # execution costs
x <- c(10L, 20L, 15L)  # maximum capacities
z <- 30L  # order size

q1 <- c(0:min(z,x[1L]))
q2 <- c(0:min(z,x[2L]))
q3 <- c(0:min(z,x[3L]))

combos <- as.matrix(unname(expand.grid(q1, q2, q3)))
combos <- combos[rowSums(combos)==z,]

Ct <- rowSums(sweep(combos, 2L, w, '*'))
combos[which.min(Ct),]


# Define the parameters of the problem
Ts <- 5L # Number of periods
nt <- 2L # Number of supply sources
v1 <- 40.0; w1 <-  0.0 # Long-term contract representation
v2 <-  8.0; w2 <- 40.0 # Option contract representation
p  <- 100.0 # price (constant for every period)
mu_D <-  1.0; sd_D <-  0.2 # Demand parameters 
mu_S <- 80.0; sd_S <- 20.0 # Spot market parameters 
h <- 5.0 # Holding cost

for (t in seq(Ts)) {
  d.t <- rnorm(1L, mu_D, sd_D)
  s.t <- rnorm(1L, mu_S, sd_S)
}


