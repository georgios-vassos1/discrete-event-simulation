
reticulate::source_python("~/solutions/rl/bandits/exutils.py")

index <- list.files("~/pcmabinf/OpenML-CC18")

ImputeMedian <- function(X) {
  X.dim <- dim(X)
  n <- X.dim[1]
  p <- X.dim[2]
  na.r <- (seq_along(X)[is.na(X)] %% n)
  na.c <- (seq_along(X)[is.na(X)] %/% n) + 1
  for (j in unique(na.c)) {
    jdx <- na.r[na.c==j]
    X[jdx,na.c] <- median(X[,na.c], na.rm = TRUE)
  }
  X
}

DataUnit <- function(filex, ...) {
  data    <- new.env(hash = FALSE, parent = emptyenv())
  args    <- list(...)
  dump    <- read_pickle(filex)
  context <- dump[[1]]
  arms    <- dump[[2]] + 1
  if (sum(is.na(context)) > 0) {
    print("Imputing column median to missing values!")
    context <- ImputeMedian(context)
  }
  nobs    <- nrow(context)
  shuffle <- sample(1:nobs, nobs, FALSE)
  data$nobs    <- nobs
  data$context <- context[shuffle,]
  data$arms    <- arms[shuffle]
  class(data)  <- "Data unit"
  return(data)
}

