rm(list=ls())
source("~/solutions/rl/misc/inventory/mdp.R")

## Monte Carlo Tree Search
Node <- function(env, state, action=0L, reward=NULL, parent=NULL, ...) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$mdp    <- env
  node$parent <- parent
  node$state  <- state
  node$action <- action
  node$reward <- reward
  node$children <- list()
  class(node)   <- "Monte Carlo Tree Node"
  return(node)
}

MCTS <- function(Cp=1.0, ...) {
  MCTS          <- new.env(hash = FALSE, parent = emptyenv())
  MCTS$Qx       <- list()
  MCTS$Nx       <- list()
  MCTS$Px       <- list()
  MCTS$N        <- list()
  MCTS$Cp       <- Cp
  class(MCTS)   <- "Monte Carlo Tree Search"
  return(MCTS)
}

is_fully_expanded <- function(node) {
  actions <- get_actions(node$mdp, node$state)
  if (length(actions) == length(node$children)) {
    return(TRUE)
  }
  FALSE
}

uct <- function(mcts, x) {
  idx <- as.character(x)
  action <- which.min(unlist(mcts$Qx[[idx]]) + 2.*mcts$Cp*sqrt(2.*log(mcts$N[[idx]]) / unlist(mcts$Nx[[idx]])))
  as.numeric(names(action))
}

get_child <- function(node, a) {
  ida    <- as.character(a)
  r      <- reward(node$mdp, node$state, a)
  next_x <- rNextState(node$mdp, node$state, a)
  children <- unname(sapply(node$children, function(x) x$state))
  cdx <- which(children == next_x)
  if (length(cdx) == 1L) {
    return(node$children[[cdx]])
  }
  node$children[[ida]] <- Node(node$mdp, state = next_x, action = a, reward = r, parent = node)
  return(node$children[[ida]])
}

expand <- function(node, mcts, ...) {
  if (is_fully_expanded(node)) {
    return(node)
  }
  idx     <- as.character(node$state)
  actions <- setdiff(as.character(get_actions(node$mdp, node$state)), names(node$children))
  ida     <- sample(actions, 1L)
  action  <- as.numeric(ida)
  node$children[[ida]] <- get_child(node, action)
  if (is.null(mcts$N[[idx]])) {
    mcts$N[[idx]]  <- 0L
  }
  if (is.null(mcts$Nx[[idx]][[ida]])) {
    mcts$Nx[[idx]][[ida]] <- 0L
    mcts$Qx[[idx]][[ida]] <- 0.0
  }
  return(get_child(node, action))
}

select <- function(node, mcts) {
  if (!is_fully_expanded(node)) {
    return(node)
  } else {
    actions <- names(node$children)
    action  <- uct(mcts, node$state)
    return(get_child(node, action))
  }
}

back_propagate <- function(node, mcts, r, child) {
  idx <- as.character(node$state)
  ida <- as.character(child$action)
  mcts$N[[idx]] <- mcts$N[[idx]] + 1L
  mcts$Nx[[idx]][[ida]] <- mcts$Nx[[idx]][[ida]] + 1L
  mcts$Qx[[idx]][[ida]] <- (mcts$Nx[[idx]][[ida]] * mcts$Qx[[idx]][[ida]] + r) / (mcts$Nx[[idx]][[ida]] + 1.0)
  if (!is.null(node$parent)) {
    back_propagate(node$parent, mcts, node$reward+r, node)
  }
}

simulate <- function(node, mcts, gamma=0.99, ...) {
  x   <- node$state
  idx <- as.character(x)
  # Initialization
  total_reward <- 0.0
  depth  <- 0L
  visits <- NULL
  visits[[idx]] <- 1L
  # Simulation
  while (rbinom(1L, 1L, 1./visits[[idx]])) {
    # Choose an action to execute
    action <- sample(get_actions(node$mdp, x), 1L)
    # Execute the action
    r <- reward(node$mdp, x, action) # Observe reward
    next_x <- rNextState(node$mdp, x, action) # Transit to next state
    # Discount the reward
    total_reward <- total_reward + (gamma)^(depth) * r
    depth <- depth + 1L
    # Update number of visits
    idx   <- as.character(next_x)
    if (is.null(visits[[idx]])) {
      visits[[idx]] <- 1L
    } else {
      visits[[idx]] <- visits[[idx]] + 1L
    }
  }
  total_reward
}

rollout <- function(mcts, root, ...) {
  node <- select(root, mcts)
  child <- expand(node, mcts)
  r     <- simulate(child, mcts)
  back_propagate(node, mcts, r, child)
}

root <- Node(env, 5L)
mcts <- MCTS()
for (i in seq(500L)) {
  rollout(mcts, root)
}
min(unlist(mcts$Qx[["5"]]))


## OBSOLETE
if (FALSE) {
  # reward <- function(x, a, D=seq(0L,9L), h=1.0, p=1.0, K=5.0, M=20L, ...) {
  #   (1.0/length(D)) * sum(h*pmin(pmax(x+a-D,0.0),M) + p*pmax(D-x-a, 0.0)) + K*(a>0)
  # }
  
  # rNextState <- function(x, a, D=seq(0L,9L), M=20L, ...) sample(pmin(pmax(x+a-D, 0L), M), 1L)
  
  # get_actions <- function(x, M=20L, ...) {
  #   unique(pmax(seq(0L,M)-x,0L))
  # }
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
