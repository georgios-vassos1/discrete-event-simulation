# rm(list=ls())
# setwd("~/solutions/rl/tictactoe")

RandomPlayer <- function(game, ...) {
  player      <- new.env(hash = FALSE, parent = emptyenv())
  args        <- list(...)
  player$game <- game
  class(player) <- "Random Player"
  return(player)
}

play <- function(player, board) {
  Na <- getActionSize(player$game)
  valids <- getValidMoves(player$game, board)
  action <- sample.int(Na, 1L)
  while (valids[action] != 1L) {
    action <- sample.int(Na, 1L)
  }
  action
}
