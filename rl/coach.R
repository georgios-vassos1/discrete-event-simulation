
coach <- function(game = NULL, nnet = NULL, ...) {
  coach        <- new.env(hash = FALSE, parent = emptyenv())
  coach$game   <- game
  coach$nnet   <- nnet
  coach$pnet   <- NULL  # the competitor network
  coach$args   <- list(...)
  coach$mcts   <- MCTS(game, nnet, cpuct = args[["cpuct"]], numMCTSSims = args[["numMCTSSims"]])
  coach$tempThreshold        <- args[["tempThreshold"]]
  coach$trainExamplesHistory <- list()  # history of examples from args.numItersForTrainExamplesHistory latest iterations
  coach$skipFirstSelfPlay    <- FALSE   # can be overriden in loadTrainExamples()
  class(coach) <- "Coach instance"
  return(coach)
}

execute_episode <- function(coach) {
  # This function executes one episode of self-play, starting with player 1.
  # As the game is played, each turn is added as a training example to
  # trainExamples. The game is played till the game ends. After the game
  # ends, the outcome of the game is used to assign values to each example
  # in trainExamples.
  # 
  # It uses a temp=1 if episodeStep < tempThreshold, and thereafter
  # uses temp=0.
  # 
  # Returns:
  #   trainExamples: a list of examples of the form (canonicalBoard, currPlayer, pi,v)
  #                  pi is the MCTS informed policy vector, v is +1 if
  #                  the player eventually won the game, else -1.
  trainExamples   <- list()
  board           <- getInitBoard(coach$game)
  coach$curPlayer <- 1
  episodeStep     <- 0
  while (1L) {
    episodeStep    <- episodeStep + 1L
    canonicalBoard <- getCanonicalForm(coach$game, board, coach$curPlayer)
    temp           <- as.integer(episodeStep < coach$tempThreshold)
  
    pi  <- getActionProb(coach$mcts, canonicalBoard, temp = temp)
    sym <- getSymmetries(coach$game, canonicalBoard, pi)
    for (i in seq_along(sym)) {
      trainExamples[[i]] <- list(
        sym[[i]]$b, coach$curPlayer, sym[[i]]$p, NA
      )
    }

    action <- sample(seq(pi), 1L, prob = pi)
    output <- getNextState(coach$game, board, coach$curPlayer, action)
    board  <- output[["board"]]
    coach$curPlayer <- output[["curPlayer"]]

    r = getGameEnded(coach$game, board, coach$curPlayer)

    if (r != 0.0) {
      result <- vector(mode = "list", length = length(trainExamples))
      i <- 1L
      for (x in trainExamples) {
        result[[i]] <- list(
          x[[1]], x[[3]], r * ((-1) ^ (x[[2]] != coach$curPlayer))
        )
        i <- i + 1L
      }
      return(result)
    }
  }
}
