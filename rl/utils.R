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
