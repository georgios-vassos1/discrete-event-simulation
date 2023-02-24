
# Adaptive multi-stage sampling
AMS <- function(env, ...) {
  AMS      <- new.env(hash = FALSE, parent = emptyenv())
  args     <- list(...)
  AMS$mdp  <- env
  AMS$H    <- args[["H"]]
  AMS$X    <- args[["X"]]
  AMS$A    <- args[["A"]]
  AMS$cX   <- length(args[["X"]]) # Cardinality of state space
  AMS$cA   <- length(args[["A"]]) # Cardinality of action space
  AMS$V    <- NULL
  class(AMS) <- "Adaptive multi-stage sampler"
  return(AMS)
}

init.ams <- function(ams, ...) {
  ams$V <- matrix(NA,  nrow = (ams$H+1L), ncol = length(ams$X))
}

ams_search <- function(ams, x, i, Ni, gamma=1.0, ...) {
  stopifnot(Ni>=ams$cA)
  # Terminal condition
  if (i==ams$H) {
    ams$V[i+1L,x+1L] <- 0.0
    return(0.0)
  }
  # Initialization (fix this to run only once)
  rx <- rep(NA, ams$cA)
  EV <- rep(NA, ams$cA)
  Nx <- rep(NA, ams$cA)
  for (j in seq_along(ams$A)) {
    # Compute reward
    rx[j] <- reward(ams$mdp, x, ams$A[j])
    # Sample next state
    y      <- rNextState(ams$mdp, x, ams$A[j])
    # Compute expected future value
    EV[j]  <- ams_search(ams, y, i+1L, Ni, gamma)
    # Update counter
    Nx[j]  <- 1L
  }
  # Main loop
  n <- length(ams$A)
  while (n <= Ni) {
    Qx <- rx + gamma * (EV/Nx) - sqrt(2*log(n)/Nx)
    # Optimal action
    j_star <- which.min(Qx)
    a_star <- ams$A[j_star]
    # Sample next state
    y      <- rNextState(ams$mdp, x, a_star)
    # Compute  the future value of the state y
    EV[j_star] <- EV[j_star] + ams_search(ams, y, i+1L, Ni, gamma)
    # Update data containers
    Nx[j_star] <- Nx[j_star] + 1L
    n <- n + 1L
  }
  v <- sum((Nx/Ni)*(rx+gamma*EV/Nx))
  # v <- min(rx+gamma*EV/Nx)
  ams$V[i+1L,x+1L] <- v
  return(v)
}

test_ams <- function() {
  source("~/solutions/rl/misc/inventory/mdp.R")
  env <- test_mdp_env()
  ams <- AMS(env, H=3L, X=seq(0L,20L), A=seq(0L,20L))
  init.ams(ams)
  ams_search(ams, x = 4L, i = 0L, Ni = 25L, gamma = .7)
}
