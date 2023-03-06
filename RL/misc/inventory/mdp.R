
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
  inv$H       <- ifelse(!is.null(args[["H"]]) ,      args[["H"]],        3L  )
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

## Utilities for Monte Carlo planning
# Reward function
reward <- function(env, x, a, ...) {
  (1.0/length(env$D)) * sum(env$h(pmin(pmax(x+a-env$D,0.0),env$M)) + env$p(pmax(env$D-x-a,0.0))) + env$K*(a>0)
}

# State-transition function
rNextState <- function(env, x, a, ...) sample(pmin(pmax(x+a-env$D,0L), env$M),1L)

# Return all possible actions from a given state
get_actions <- function(env, x, ...) {
  unique(pmax(seq(0L,env$M)-x,0L))
}

# Possible next states conditional on x, a
get_transitions <- function(env, x, a, ...) {
  pmin(pmax(x+a-env$D, 0L), env$M)
}

test_mdp_env <- function(...) {
  env <- Inventory()
  init.inv(env, seq(0L,env$M), seq(0L,env$M), seq(0L,9L))
  expected_cost(env)
  value_function(env)
  env
}
