# rm(list=ls())
# setwd("~/solutions/rl/tictactoe")

initializGame <- function(game.env, n) {
  game.env$n <- n
}

getInitialBoard <- function(game.env) {
  n <- game.env$n
  initializeBoard(game.env, n)
  game.env$pieces
}

getBoardSize <- function(game.env) {
  rep(game.env$n, 2L)
}

getActionSize <- function(game.env) {
  (game.env$n^2) + 1L
}

getValidMoves <- function(game.env, ...) {
  valids     <- rep(0L, getActionSize(game.env))
  legalMoves <- getLegalMoves(game.env)
  if (length(legalMoves) == 0L) {
    valids[length(valids)] <- 1L
    return(valids)
  }
  valids[legalMoves] <- 1L
  valids
}

isValidMove <- function(game.env, action) {
  game.env$empty[action]
}

getNextState <- function(game.env, player, action, ...) {
  execute_move(env, action, player)
  -player
}

gameEnded <- function(game.env, player) {
  # return 0 if not ended, 1 if player 1 won, -1 if player 1 lost
  # player = 1
  if (isWin(game.env, player)) {
    return(player)
  }
  if (isWin(game.env, -player)) {
    return(-player)
  }
  if (hasLegalMoves(game.env)) {
    return(0L)
  }
  # draw has a very little value
  return(1e-4)
}

