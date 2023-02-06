rm(list=ls())
setwd("~/solutions/rl/misc/inventory")

Inventory <- function(...) {
  # H : length of horizon
  # M : inventory capacity
  # x0: initial state
  # rDemand: random demand sampling
  # h : holding cost
  # p : shortage cost
  # K : set-up cost
  args  <- list(...)
  inv   <- new.env(hash = FALSE, parent = emptyenv())
  inv$H       <- ifelse(!is.null(args[["H"]]) ,      args[["H"]],       3L   )
  inv$M       <- ifelse(!is.null(args[["M"]]) ,      args[["M"]],       20L  )
  inv$x0      <- ifelse(!is.null(args[["x0"]]),      args[["x0"]],       5L  )
  inv$rDemand <- ifelse(!is.null(args[["rDemand"]]), args[["rDemand"]], function(n=1L) sample(seq(0L,9L), n, TRUE))
  inv$h       <- ifelse(!is.null(args[["h"]]) ,      args[["h"]],       function(x) 1.0*x)
  inv$p       <- ifelse(!is.null(args[["p"]]) ,      args[["p"]],       function(x) 1.0*x)
  inv$K       <- ifelse(!is.null(args[["K"]]) ,      args[["K"]],       5.0  )
  inv$X  <- NULL # State space
  inv$A  <- NULL # Action space
  inv$D  <- NULL # Demand space
  inv$EC <- NULL # Expected cost
  inv$V  <- NULL # Value function
  class(inv) <- "Inventory"
  return(inv)
}

init.inv <- function(env, X, A, D, ...) {
  # Store the state space X, action space A, and demand space D
  env$X <- X
  env$A <- A
  env$D <- D
}

expected_cost <- function(env, ...) {
  # Computes the expected cost for (X,A,D) with lost demand, capacity limit M, and fixed setup cost K
  env$EC <- matrix(NA, nrow = env$M+1L, ncol = env$M+1L)
  for (x in env$X) {
    z <- outer(x+env$A, env$D, '-') # -t(z) = outer(env$D, x+env$A, '-')
    env$EC[x+1L,] <- rowSums((1/length(env$D))*(
      env$h(pmin(pmax(z, 0.0), env$M)) + t(env$p(pmax(-t(z), 0.0))))) + env$K*(env$A > 0.0)
  }
}

value_function <- function(env, gamma=1.0, ...) {
  # Value function computation for D~DU(0,9)
  if (is.null(env$EC)) {
    env$EC <- expected_cost(env)
  }
  env$V <- matrix(NA, nrow = env$H+1L, ncol = env$M+1L)
  env$V[env$H+1L,] <- 0.0
  dim.D <- length(env$D)
  for (i in seq(env$H,1L)) {
    for (x in env$X) {
      next_x <- pmin(pmax(outer(x+env$A, env$D, '-'), 0.0), env$M) + 1L
      EV     <- c(matrix(env$V[i+1L,next_x], ncol=dim.D) %*% rep(1./dim.D,dim.D))
      env$V[i,x+1L] <- min(env$EC[x+1L,] + gamma*EV)
    }
  }
}

env <- Inventory()
init.inv(env, seq(0L,env$M), seq(0L,env$M), seq(0L,9L))
expected_cost(env)
value_function(env)
env$V

# AMS object
AMS <- function(...) {
  AMS      <- new.env(hash = FALSE, parent = emptyenv())
  args     <- list(...)
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
  ams$V   <- matrix(NA,  nrow = (ams$H+1L), ncol = length(ams$X))
}

reward <- function(x, a, D=seq(0L,9L), h=1.0, p=1.0, K=5.0, M=20L, ...) {
  (1.0/length(D)) * sum(h*pmin(pmax(x+a-D,0.0),M) + p*pmax(D-x-a, 0.0)) + K*(a>0)
}

rNextState <- function(x, a, D=seq(0L,9L), M=20L, ...) sample(pmin(pmax(x+a-D, 0L), M), 1L)

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
    rx[j] <- reward(x, ams$A[j])
    # Sample next state
    y      <- rNextState(x, ams$A[j])
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
    y      <- rNextState(x, a_star)
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

ams <- AMS(H=3L, X=seq(0L,20L), A=seq(0L,20L))
init.ams(ams)
ams_search(ams, x = 0L, i = 0L, Ni = 35L, gamma = 0.7)

## OBSOLETE
if (FALSE) {
  # Slow implementation for testing
  ECb <- matrix(NA, nrow = env$M+1L, ncol = env$M+1L)
  for (x in seq(0L,env$M)) {
    for (a in seq(0L,env$M)) {
      tmp <- 0.
      for (d in seq(0L,9L)) {
        tmp <- tmp + (.1)*(1.*min(max(x+a-d,0.0),env$M)+1.*(max(d-x-a,0.)))
      }
      ECb[x+1L,a+1L] <- tmp + env$K*(a>0L)
    }
  }
  # Backward dynamic programming
  V <- matrix(NA, nrow = env$H+1L, ncol = env$M+1L)
  V[4L,] <- 0.0
  # gamma  <- 0.593005
  gamma <- 1.0
  for (i in seq(env$H,1L)) {
    for (x in seq(0L,env$M)) {
      EV <- rep(0.0, env$M+1L)
      for (a in seq(0L,env$M)) {
        next_x <- pmin(pmax(x+a-seq(0L,9L), 0.0), env$M) + 1L
        EV[a+1L] <- c(rep(0.1,10L) %*% V[i+1L,next_x])
      }
      V[i,x+1L] <- min(ECb[x+1L,] + gamma*EV)
    }
  }
  V
}
