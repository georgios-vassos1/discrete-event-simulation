
reticulate::source_python("~/solutions/bandits/exutils.py")

index <- list.files("~/pcmabinf/OpenML-CC18")

ImputeMedian <- function(X) {
  X.dim <- dim(X)
  n <- X.dim[1]
  p <- X.dim[2]
  na.rows <- (which(is.na(X)) %%  n)
  na.cols <- (which(is.na(X)) %/% n) + 1
  for (j in unique(na.cols)) {
    mask <- is.na(X[na.rows,j])
    X[na.rows[mask],j] <- median(X[,j], na.rm = TRUE)
  }
  X
}

DataUnit <- function(filex, reward_variance=1.0, ...) {
  args    <- list(...)
  # Read data index from dump
  dump    <- read_pickle(filex)
  context <- dump[[1L]]
  arms    <- dump[[2L]] + 1L
  # Simple imputation of missing values
  if (sum(is.na(context)) > 0L) {
    print("Imputing column median to missing values!")
    context <- ImputeMedian(context)
  }
  # Number of context observations
  cxdim   <- dim(context)
  nobs    <- cxdim[1L]
  # Permutation indexing
  shuffle <- sample(seq(nobs), nobs, FALSE)
  # New data object
  data <- new.env(hash = FALSE, parent = emptyenv())
  data$task_id           <- filex
  data$arm_count         <- length(unique(arms))
  data$observation_count <- nobs
  data$feature_count     <- cxdim[2L]
  data$context           <- context[shuffle,]
  data$arms              <- arms[shuffle]
  data$reward_variance   <- reward_variance
  class(data) <- "Data unit"
  return(data)
}

DataUnitTest <- function(...) {
  world <- DataUnit(index[1L])
}

train.idx <- function(k, nfolds) {
  c(k:(k+nfolds-2L))%%nfolds+1L
}

block.idx <- function(k, size) {
  (k-1L)*size+seq(size)
}
