# rm(list=ls())
# setwd("~/solutions/rl/tictactoe")

# Improved version using environment objects

Board <- function(n = 3L, ...) {
  board        <- new.env(hash = FALSE, parent = emptyenv())
  args         <- list(...)
  board$n      <- n
  if (is.null(args[["pieces"]])) {
    board$pieces <- rep(0L, n^2L)   # 1d board representation
  } else {
    board$pieces <- args[["pieces"]]
  }
  class(board) <- "Tic Tac Toe Board"
  return(board)
}

CloneBoard <- function(board) {
  Board(board$n, pieces = board$pieces)
}

DisplayBoard <- function(board) {
  matrix(board$pieces, ncol = board$n)
}

getitem <- function(board, i) {
  # Index $i$ must be for 1d vector representation of the board
  # That is, $i\in\{1,\dots,n\}$.
  return(board$pieces[i])
}

get_legal_moves <- function(board, ...) {
  return(which(board$pieces == 0L))
}

has_legal_moves <- function(board) {
  any(board$pieces == 0L)
}

is_win <- function(board, player) {
  n   <- board$n
  win <- player * n
  # Check whether the given player has collected a triplet in any direction; 
  # @param color (1=white,-1=black)
  pieces2d <- matrix(board$pieces, ncol = n)
  # check x-strips
  xrows <- apply(pieces2d, 1, sum)
  # check y-strips
  ycols <- apply(pieces2d, 2, sum)
  # check two diagonal strips
  diags <- c(sum(diag(pieces2d)), sum(diag(pieces2d[,seq(n,1L)])))
  # return result
  any(c(xrows, ycols, diags) ==  win)
}

execute_move <- function(board, move1d, player) {
  stopifnot(board$pieces[move1d] == 0L)
  board$pieces[move1d] <- player
}


if (FALSE) {
  ## OBSOLETE version
  
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

}
