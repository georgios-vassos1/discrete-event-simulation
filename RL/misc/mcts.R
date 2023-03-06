## Minimal implementation of Monte Carlo tree search (MCTS)
newBoard <- function(pieces=matrix(rep(0L,9L), nrow = 3L), turn=TRUE, winner=0L, terminal=FALSE) {
  board          <- new.env(hash = FALSE, parent = emptyenv())
  board$pieces   <- pieces
  board$turn     <- turn
  board$winner   <- winner
  board$terminal <- terminal
  class(board)   <- "TicTacToeBoard"
  return(board)
}

mat2str <- function(m) {
  paste(c((3L+m)%%3L), collapse = "")
}

isWin <- function(pieces, player, n=3L) {
  win <- player * n
  # Check whether the given player has collected a triplet in any direction; 
  # @param color (1=white,-1=black)
  # check x-strips
  xrows <- apply(pieces, 1, sum)
  # check y-strips
  ycols <- apply(pieces, 2, sum)
  # check two diagonal strips
  diags <- c(sum(diag(pieces)), sum(diag(pieces[,seq(n,1L)])))
  # return result
  any(c(xrows, ycols, diags) ==  win)
}

find_winner <- function(pieces) {
  # return 0 if not ended, 1 if player 1 won, -1 if player 1 lost
  # player = 1
  if (isWin(pieces, 1L)) {
    return(1L)
  }
  if (isWin(pieces, -1L)) {
    return(-1L)
  }
  if (all(pieces != 0L)) {
    return(0L)
  }
  return(-Inf)
}

make_move <- function(board, idx) {
  pieces      <- board$pieces
  pieces[idx] <- 1L * board$turn + (-1L) * (1L-board$turn)
  turn        <- ! board$turn
  winner      <- find_winner(pieces)
  terminal    <- (winner > -Inf) || (!any(pieces == 0L))
  return(newBoard(pieces, turn, winner, terminal))
}

find_children <- function(board, n=3L) {
  if (board$terminal) {
    return(NULL)
  }
  children <- list()
  for (i in seq(n^2)) {
    if (c(board$pieces)[i] != 0) {
      next
    }
    child <- make_move(board, i)
    children[[mat2str(child$pieces)]] <- child
  }
  children
}

find_random_child <- function(board) {
  if (board$terminal) {
    return(NULL)
  }
  valids <- which(board$pieces == 0L)
  return(make_move(board, sample(valids, 1L)))
}

reward <- function(board) {
  stopifnot(board$terminal)
  player <- (1L * board$turn + (-1L) * (1L-board$turn))
  stopifnot(board$winner != player)
  if (board$winner != player) {
    return(0.0) # Your opponent has just won. Bad.
  }
  if (board$winner == 0L) {
    return(0.5) # Is a tie
  }
}

MCTS <- function(exploration_weight=1.0) {
  MCTS                    <- new.env(hash = FALSE, parent = emptyenv())
  MCTS$Q                  <- list()
  MCTS$N                  <- list()
  MCTS$children           <- list()
  MCTS$exploration_weight <- exploration_weight
  class(MCTS)             <- "Monte Carlo search tree"
  return(MCTS)
}

expand <- function(mcts, node) {
  # node: an instance of the class `TicTacToeBoard`
  idx <- mat2str(node$pieces)
  if (!is.null(mcts$children[[idx]])) {
    return(NULL)
  }
  mcts$children[[idx]] <- find_children(node)
  mcts$N[[idx]] <- 0L
  mcts$Q[[idx]] <- 0.0
}

backpropagate <- function(mcts, path, reward) {
  # path: list of node keys
  for (node in rev(path)) {
    idx <- mat2str(node$pieces)
    mcts$N[[idx]] <- mcts$N[[idx]] + 1L
    mcts$Q[[idx]] <- mcts$Q[[idx]] + reward
    reward        <- 1 - reward  # 1 for me is 0 for my enemy, and vice versa
  }
}

simulate <- function(mcts, node) {
  invert_reward <- TRUE
  while (1) {
    if (node$terminal) {
      rwd <- reward(node)
      return(invert_reward*(1-rwd) + (1-invert_reward)*rwd)
    }
    node          <- find_random_child(node)
    invert_reward <- ! invert_reward
  }
}

uct <- function(mcts, nx, logN) {
  # ndx: node key string
  (mcts$Q[[nx]] / mcts$N[[nx]]) + mcts$exploration_weight * sqrt(logN / mcts$N[[nx]])
}

uct_select <- function(mcts, node) {
  idx <- mat2str(node$pieces)
  for (nx in mcts$children[[idx]]) {
    stopifnot(!is.null(mcts$children[[mat2str(nx$pieces)]]))
  }
  logN <- log(mcts$N[[idx]])
  v    <- c()
  for (child in mcts$children[[idx]]) {
    v <- c(uct(mcts, mat2str(child$pieces), logN), v)
  }
  j <- names(mcts$children[[idx]])[which.max(v)]
  mcts$children[[idx]][[j]]
}

score <- function(mcts, idx) {
  if (is.null(mcts$N[[idx]])) {
    return(-Inf)  # avoid unseen moves
  }
  mcts$Q[[idx]] / mcts$N[[idx]]
}

choose <- function(mcts, node) {
  stopifnot(!node$terminal)
  idx <- mat2str(node$pieces)
  if (is.null(mcts$children[[idx]])) {
    return(find_random_child(node))
  }
  v <- vector(mode = "numeric", length = length(mcts$children[[idx]]))
  for (i in seq_along(mcts$children[[idx]])) {
    v[i] <- score(mcts, names(mcts$children[[idx]][i]))
  }
  mcts$children[[idx]][[which.max(v)]]
}

select <- function(mcts, node) {
  path <- list()
  while (1L) {
    idx <- mat2str(node$pieces)
    path[[idx]] <- node
    if (is.null(mcts$children[[idx]])) {
      return(path)
    }
    unexplored <- setdiff(names(mcts$children[[idx]]), names(mcts$children))
    if (length(unexplored) > 0L) {
      jdx <- tail(unexplored, 1L)
      path[[jdx]] <- mcts$children[[idx]][[jdx]]
      return(path)
    }
    node <- uct_select(mcts, node)
  }
}

rollout <- function(mcts, node) {
  path <- select(mcts, node)
  leaf <- tail(path, 1L)[[1L]]
  expand(mcts, leaf)
  rwd  <- simulate(mcts, leaf)
  backpropagate(mcts, path, rwd)
}

## Run some games
tree  <- MCTS()
board <- newBoard()
while (1L) {
  idx <- sample(seq(9L)[board$pieces == 0L], 1L)
  board <- make_move(board, idx)
  print(board$pieces)
  if (board$terminal) {
    break
  }
  for (i in seq(50L)) {
    rollout(tree, board)
  }
  board <- choose(tree, board)
  print(board$pieces)
  if (board$terminal) {
    break
  }
}
