# rm(list=ls())
# setwd("~/solutions/rl/tictactoe")

flip <- function(M, margin) {
  if (margin == 1L) {
    return(M[seq(nrow(M),1L),,drop = FALSE])
  }
  M[,seq(ncol(M),1L),drop = FALSE]
}

rot90 <- function(M, k, axes=c(1L,2L)) {
  k <- k %% 4L
  if (k == 0L) {
    return(M)
  }
  if (k == 2L) {
    return(flip(flip(M, 1L), 2L))
  }
  if (k == 1L) {
    return(t(flip(M, 2L)))
  } else {
    return(flip(t(M), 2L))
  }
}

# Improved version using environment objects

TicTacToeGame <- function(n = 3L, ...) {
  game <- new.env(hash = FALSE, parent = emptyenv())
  args   <- list(...)
  game$n <- n
  class(game) <- "Tic Tac Toe Game"
  return(game)
}

getInitBoard <- function(game) {
  Board(game$n)
}

getBoardSize <- function(game) {
  rep(game$n, 2L)
}

getActionSize <- function(game) {
  (game$n^2L) + 1L
}

getNextState <- function(game, pieces, player, move1d) {
  newBoard <- Board(game$n, pieces = pieces)
  auxmove  <- getActionSize(game)
  if (move1d != auxmove) {
    execute_move(newBoard, move1d, player)
  }
  list(board = newBoard$pieces, player = -player)
}

getValidMoves <- function(game, board, ...) {
  valids <- rep(0L, getActionSize(game))
  legalMoves <- get_legal_moves(board)
  if (length(legalMoves) == 0L) {
    valids[length(valids)] <- 1L
    return(valids)
  }
  valids[legalMoves] <- 1L
  valids
}

getGameEnded <- function(game, board, player) {
  # return 0 if not ended, 1 if player 1 won, -1 if player 1 lost
  # player = 1
  if (is_win(board, player)) {
    return(1L)
  }
  if (is_win(board, -player)) {
    return(-1L)
  }
  if (has_legal_moves(board)) {
    return(0L)
  }
  # draw has a very little value
  return(1e-4)
}

getCanonicalForm <- function(game, board, player) {
  player * board$pieces
}

getSymmetries <- function(game, pieces, pi1d) {
  stopifnot(length(pi1d) == getActionSize(game))
  pieces2d <- matrix(pieces, ncol = game$n)
  pi2d     <- matrix(pi1d[seq(game$n^2L)], ncol = game$n)
  sym      <- list()
  k        <- 1L
  for (i in seq(ceiling(game$n/2L))) {
    for (j in c(TRUE,FALSE)) {
      newB  <- rot90(pieces2d, i)
      newPi <- rot90(pi2d, i)
      if (j) {
        newB  <- flip(newB, 2L)
        newPi <- flip(newPi, 2L)
      }
      sym[[k]] <- list(board=c(newB), pi=c(newPi, pi1d[length(pi1d)]))
      k <- k + 1L
    }
  }
  sym
}

stringRepresentation <- function(pieces) {
  paste(c((3L+pieces)%%3L), collapse = "")
}

if (FALSE) {
  # OBSOLETE version
  initializeGame <- function(game.env, n) {
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
}
