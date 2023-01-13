rm(list=ls())
setwd("~/solutions/rl")

# String representation for 3x3 tictactoe board in terms of 0,1,2
#  0 : empty, 1 : player 1, 2: player 2
str2num <- function(x) as.integer(unlist(strsplit(x, "")))

# Use aliases {0 :0, 1: 1, 2: -1}
str2mat <- function(x, zc=c(0,1,-1), n=3L, ...) {
  matrix(zc[str2num(x)+1], ncol = n)
}

mat2str <- function(m) {
  paste(c((3L+m)%%3L), collapse = "")
}

zc3d <- function() c(0,1,-1)
# zinv <- function(x) (3L+c(x))%%3L

# It may generate false board configurations but that is not a problem
generateAllBoards <- function(n=3L) {
  x <- seq(0,n-1L)
  for (i in seq(2L,(n*n))) {
    x <- c(outer(seq(0,n-1L), x, FUN = paste0))
  }
  x
}

storeAllBoards <- function(n=3L) {
  X <- generateAllBoards(n) # All possible boards
  L <- list()
  for (x in X) {
    temp <- str2mat(x)
    # Check if board configuration is valid
    if ((sum(temp) > 1L) || (sum(temp) < -1L)) next
    L[[x]] <- temp
  }
  L
}

getBoardState <- function(env, boards) {
  # Generate a list of { board key: game status }
  # The game status comes from `gameEnded` and takes the values
  #  0    : the game has not ended
  #  1    : player 1 win
  #  2    : player 2 win
  #  1e-4 : draw
  Es <- vector(mode = "list", length = length(boards))
  names(Es) <- names(boards)
  for (s in names(Es)) {
    # `pieces` and `empty` are needed for gameEnded
    env$pieces <- boards[[s]]
    env$empty  <- (c(env$pieces)==0L)
    # Store the value
    if (is.null(Es[[s]])) {
      Es[[s]] <- gameEnded(env, 1L)
    }
  }
  Es
}

## Test
source("~/solutions/rl/tictactoe/logic.R")
source("~/solutions/rl/tictactoe/game.R")

env <- globalenv()
initializGame(env, 3L)
initializeBoard(env)

boards <- storeAllBoards()
# x <- boards[["002010201"]]
# c(x)
# zinv(c(x))

Es <- getBoardState(env, boards)
Es

MCTS <- function(cpuct=1.0, numMCTSSims=10L) {
  MCTS        <- new.env(hash = FALSE, parent = emptyenv())
  MCTS$Qsa    <- list()
  MCTS$Nsa    <- list()
  MCTS$Ns     <- list()
  MCTS$Ps     <- list()
  MCTS$Es     <- list()
  MCTS$Vs     <- list()
  MCTS$cpuct  <- cpuct
  MCTS$numMCTSSims <- numMCTSSims
  class(MCTS) <- "Monte Carlo search tree"
  return(MCTS)
}

mcts <- MCTS()
mcts$Es <- Es
mcts$Qsa <- vector(mode = "list", length = length(Es))
mcts$Nsa <- vector(mode = "list", length = length(Es))
mcts$Ns  <- vector(mode = "list", length = length(Es))
for (i in seq(length(Es))) {
  mcts$Ns[[i]]  <- 0L
  mcts$Qsa[[i]] <- rep(0.0, 9L)
  mcts$Nsa[[i]] <- rep(0.0, 9L)
}
names(mcts$Qsa) <- names(mcts$Es)
names(mcts$Nsa) <- names(mcts$Es)

mcts_search <- function(mcts, board, player) {

  s <- mat2str(board * player)

  if (mcts$Es[[s]] != 0L) {
    return(-mcts$Es[[s]])
  }

  if (is.null(mcts$Ps[[s]])) {
    tmp <- c(0.6707,0.6248,0.8812,0.0621,0.3629,0.5467,0.205,0.2302,0.7884) 
    tmp <- tmp / sum(tmp)
    mcts$Ps[[s]] <- tmp   # Replace with Neural Network policy output
    v            <- 0.242 # Replace with Neural Network value approximation
    valids <- (c(board) != 0L)
    mcts$Ps[[s]] <- mcts$Ps[[s]] * valids
    sum_Ps_s     <- sum(mcts$Ps[[s]])
    if (sum_Ps_s > 0.0) {
      mcts$Ps[[s]] <- mcts$Ps[[s]] / sum_Ps_s
    } else {
      print("All valid moves were masked, doing a workaround.")
      mcts$Ps[[s]] <- mcts$Ps[[s]] + valids
      mcts$Ps[[s]] <- mcts$Ps[[s]] / sum(mcts$Ps[[s]])
    }
    mcts$Vs[[s]] <- valids
    mcts$Ns[[s]] <- 0L
    return(-v)
  }

  valids <- mcts$Vs[[s]]
  u <- rep(-Inf, 9L)
  for (a in seq(9L)[valids]) {
    u[a] <- mcts$Qsa[[s]][[a]] + mcts$cpuct * mcts$Ps[[s]][[a]] * sqrt(mcts$Ns[[s]]) / (1 + mcts$Nsa[[s]][[a]])
  }
  a.opt <- which.max(u)
  u.opt <- u[a.opt]

  next_s <- board
  next_s[a.opt] <- player

  v <- mcts_search(mcts, next_s, -player)

  mcts$Qsa[[s]][[a]] <- (mcts$Nsa[[s]][[a]] * mcts$Qsa[[s]][[a]] + v) / (mcts$Nsa[[s]][[a]] + 1.0)
  mcts$Nsa[[s]][[a]] <- mcts$Nsa[[s]][[a]] + 1L
  mcts$Ns[[s]] <- mcts$Nsa[[s]] + 1L
  return(-v)
}

mcts$Ps <- mcts$Vs <- list()
mcts_search(mcts, boards[[sample(length(boards),1)]], 1L)
mcts$Ps

getActionProb <- function(mcts, board, player, temp=1.0) {
  # This function performs numMCTSSims simulations of MCTS starting from
  # canonicalBoard.
  # Returns:
  #   probs: a policy vector where the probability of the ith action is
  #          proportional to Nsa[(s,a)]**(1./temp)
  for (i in seq(numMCTSSims)) {
    mcts_search(mcts, board, player)
  }
  s <- mat2str(board * player)
  counts <- mcts$Nsa[[s]]

  if (temp == 0.0) {
    actions <- which.max(counts)
    a.opt   <- sample(actions, 1L)
    probs   <- rep(0.0, length(counts))
    probs[a.opt] <- 1.0
    return(probs)
  }

  counts <- counts^(1.0/temp)
  probs  <- counts / sum(counts)
  return(probs)
}

######################################################################
