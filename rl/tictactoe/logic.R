# rm(list=ls())
# setwd("~/solutions/rl/tictactoe")

initializeBoard <- function(board.env, n=3L) {
  # Create the empty board array.
  board.env$pieces <- diag(0L, n)
  board.env$n      <- n
  board.env$moves  <- seq(n^2) # stores the legal moves
  board.env$empty  <- rep(TRUE, n^2)
}

pos2d <- function(pos) {
  c(((pos-1L) %%  n) + 1L, ((pos-1L) %/% n) + 1L)
}

getLegalMoves <- function(board.env) {
  board.env$moves[board.env$empty]
}

hasLegalMoves <- function(board.env) {
  any(board.env$empty)
}

isWin <- function(board.env, color) {
  n   <- board.env$n
  win <- color * n
  # Check whether the given player has collected a triplet in any direction; 
  # @param color (1=white,-1=black)
  # check x-strips
  xrows <- apply(board.env$pieces, 1, sum)
  # check y-strips
  ycols <- apply(board.env$pieces, 2, sum)
  # check two diagonal strips
  diags <- c(sum(diag(board.env$pieces)), sum(diag(board.env$pieces[,seq(n,1L)])))
  # return result
  any(c(xrows, ycols, diags) ==  win)
}

execute_move <- function(board.env, move1d, color) {
  # Perform the given move on the board;
  # the move is a value between 1 and n, that is, in serial format
  # color gives the color pf the piece to play (1=white,-1=black)
  stopifnot(board.env$empty[move1d])
  board.env$empty[move1d]       <- FALSE
  xy                      <- pos2d(move1d)
  board.env$pieces[xy[1],xy[2]] <- color
}

display.board <- function(board.env) {
  board.env$pieces
}
