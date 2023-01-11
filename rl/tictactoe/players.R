# rm(list=ls())
# setwd("~/solutions/rl/tictactoe")

randomPlayer <- function(game.env) {
  Na <- getActionSize(game.env) - 1L # Last action is auxiliary; not valid action
  repeat{
    action <- sample.int(Na, 1L)
    if(isValidMove(game.env, action)){
      break
    }
  }
  action
}
