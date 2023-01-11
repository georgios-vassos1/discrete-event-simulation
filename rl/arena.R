rm(list=ls())
setwd("~/solutions/rl")

source("~/solutions/rl/tictactoe/logic.R")
source("~/solutions/rl/tictactoe/game.R")
source("~/solutions/rl/tictactoe/players.R")

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

# An Arena class where any 2 agents can be pit against each other.
env <- globalenv()
initializGame(env, 3L)

players <- list(
  randomPlayer,
  randomPlayer
)

playGames(env, 300L)
display.board(env)
