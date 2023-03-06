rm(list=ls())
setwd("~/solutions/rl")

## Test
source("~/solutions/rl/tictactoe/logic.R")
source("~/solutions/rl/tictactoe/game.R")
source("~/solutions/rl/utils.R")

MCTS <- function(game = NULL, nnet = NULL, ...) {
  MCTS        <- new.env(hash = FALSE, parent = emptyenv())
  args        <- list(...)
  MCTS$game   <- game
  MCTS$nnet   <- nnet
  MCTS$Qsa    <- list()
  MCTS$Nsa    <- list()
  MCTS$Ns     <- list()
  MCTS$Ps     <- list()
  MCTS$Es     <- list()
  MCTS$Vs     <- list()
  MCTS$cpuct  <- args[["cpuct"]]
  MCTS$numMCTSSims <- args[["numMCTSSims"]]
  class(MCTS) <- "Monte Carlo search tree"
  return(MCTS)
}

mcts_search <- function(mcts, board, player) {

  s <- mat2str(board * player)

  if (mcts$Es[[s]] != 0L) {
    return(-mcts$Es[[s]])
  }

  if (is.null(mcts$Ps[[s]])) {
    # Neural network
    tmp <- c(0.6707,0.6248,0.8812,0.0621,0.3629,0.5467,0.205,0.2302,0.7884) 
    tmp <- tmp / sum(tmp)
    mcts$Ps[[s]] <- tmp   # Replace with Neural Network policy output
    v            <- 0.242 # Replace with Neural Network value approximation
    # End of neural network
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

mcts_test <- function(env, ...) {

  initializGame(env, 3L)
  initializeBoard(env)
  
  boards <- storeAllBoards()
  # x <- boards[["002010201"]]
  # c(x)
  # zinv(c(x))
  
  Es <- getBoardState(env, boards)
  Es
  
  mcts <- MCTS(cpuct=1.0, numMCTSSims=10L)
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
  
  
  # mcts$Ps <- mcts$Vs <- list()
  mcts_search(mcts, boards[[sample(length(boards),1)]], 1L)
  mcts$Ps
}
