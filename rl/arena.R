rm(list=ls())
setwd("~/solutions/rl")

source("~/solutions/rl/tictactoe/logic.R")
source("~/solutions/rl/tictactoe/game.R")
source("~/solutions/rl/tictactoe/players.R")

Arena <- function(game, players, ...) {
  stopifnot(length(players) == 2L)
  arena         <- new.env(hash = FALSE, parent = emptyenv())
  args          <- list(...)
  arena$game    <- game
  arena$players <- players
  class(arena)  <- "Arena"
  return(arena)
}

playGame <- function(arena, i = 1L, ...) {
  # Play one game; start = 1 or -1 sets the starting player
  curPlayer <- i
  board     <- getInitBoard(arena$game)
  print(DisplayBoard(board))
  while (getGameEnded(arena$game, board, curPlayer) == 0L) {
    action    <- play(players[[curPlayer]], board)
    valids    <- getValidMoves(
      arena$game, Board(board$n, pieces = getCanonicalForm(arena$game, board, curPlayer)), 1L)
    stopifnot(valids[action] == 1L)
    outcome   <- getNextState(arena$game, board$pieces, curPlayer, action)
    board     <- Board(board$n, pieces = outcome[["board"]])
    curPlayer <- outcome[["player"]]
    print(DisplayBoard(board))
  }
  curPlayer * getGameEnded(arena$game, board, curPlayer)
}

playGames <- function(arena, num, ...) {
  cnt <- rep(0L, 3L) # (player 1 wins, player 2 wins, draws)
  # Games with start = 1L
  for (k in seq(num)) {
    w   <- playGame(arena, 1L)
    idx <- round(((w+2L)%%3L)+1L, 0L)
    cnt[idx] <- cnt[idx] + 1L
  }
  cnt
}

# An Arena class where any 2 agents can be pit against each other
game    <- TicTacToeGame()
players <- list(RandomPlayer(game), RandomPlayer(game))
arena   <- Arena(game, players)
playGame(arena)
playGames(arena, 100)


if (FALSE) {
  # OBSOLETE version modules
  
  playGame <- function(env, start=1L) {
    # Play one game; start = 1 or -1 sets the starting player
    getInitialBoard(env)
    curPlayer <- start
    while ((winner <- gameEnded(env, curPlayer)) == 0L) {
      action    <- players[[curPlayer]](env)
      stopifnot(isValidMove(env, action))
      curPlayer <- getNextState(env, curPlayer, action)
    }
    winner
  }
  
  playGames <- function(env, num, ...) {
    num <- num %/% 2L
    cnt <- rep(0L, 3L) # (player 1 wins, player 2 wins, draws)
    # Games with start = 1L
    for (j in c(1L,-1L)) {
      for (i in seq(num)) {
        w   <- playGame(env, j)
        idx <- as.integer(((w+2L)%%3L)+1L)
        cnt[idx] <- cnt[idx] + 1L
      }
    }
    cnt
  }

  env <- globalenv()
  initializGame(env, 3L)
  
  players <- list(
    randomPlayer,
    randomPlayer
  )
  
  playGames(env, 300L)
  display.board(env)
  
}
