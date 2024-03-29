
LoggingPolicy <- function(
  batch_count,
  batch_size,
  outcome_model,
  epsmult = 1.0, ...) 
{
  args    <- list(...)
  loggpi  <- new.env(hash = FALSE, parent = emptyenv())
  loggpi$batch_count   <- batch_count
  loggpi$batch_size    <- batch_size
  loggpi$outcome_model <- outcome_model
  loggpi$epsmult       <- epsmult
  loggpi$X       <- NULL
  loggpi$A       <- matrix(NA, nrow = batch_count*batch_size, ncol = 1)
  loggpi$Y       <- matrix(NA, nrow = batch_count*batch_size, ncol = 1)
  loggpi$P       <- matrix(NA, nrow = batch_count*batch_size, ncol = 1)
  loggpi$prevP   <- matrix(NA, nrow = sum(seq(batch_count)*batch_size), ncol = 1)
  loggpi$bigblok <- c(0L, cumsum(seq(batch_count)*batch_size))
  loggpi$epsilon <- vector(mode = "numeric", length = batch_count*batch_size)
  loggpi$regret  <- vector(mode = "numeric", length = batch_count*batch_size)
  class(loggpi)  <- "Logging policy"
  return(loggpi)
}

logging_policy_run <- function(loggpi, data, ...) {
  # Read input arguments
  batch_count <- loggpi$batch_count
  batch_size  <- loggpi$batch_size
  bigblok     <- loggpi$bigblok
  epsmult     <- loggpi$epsmult
  narms       <- data$arm_count
  n           <- nrow(data$context)
  p           <- ncol(data$context)
  # Preallocation
  loggpi$X <- matrix(NA, nrow = batch_count*batch_size, ncol = p)
  # Execution
  for (batch in seq(batch_count)) {
    outcome_models <- vector(mode = "list", length = 0L)
    k <- 1
    # Get a batch
    idx    <- sample(seq(n), batch_size, TRUE)
    X_test <- data$context[idx,]
    atab   <- table(loggpi$A)
    unique_arms   <- as.numeric(names(atab))
    count_per_arm <- rep(0L, narms)
    if (length(atab) > 0L) {
      count_per_arm <- as.numeric(atab)
    }
    if ((length(unique_arms) == narms) && (min(count_per_arm) > 0L)) {
      Y_hat_test <- matrix(NA, nrow = batch_size, ncol = narms)
      for (a in seq(narms)) {
        ida <- which(loggpi$A == a)
        Xa  <- loggpi$X[ida,]
        Ya  <- loggpi$Y[ida,1L]
        ma  <- loggpi$outcome_model(Ya ~ ., data = data.frame(Ya=Ya, Xa))
        outcome_models[[k]] <- ma
        k <- k + 1L
        Y_hat_test[,a] <- unname(predict(ma, data.frame(X_test)))
      }
      # Compute epsilon = (number of observations + 1)^(-1/3)
      eps    <- epsmult * ((batch-1L)*batch_size+1L)^(-1.0/3.0)
      choice <- runif(batch_size) > eps
      A_opt  <- apply(Y_hat_test, 1L, which.max)
      A_test <- A_opt * choice + sample(seq(narms), batch_size, TRUE) * (1L-choice)
      P_test <- (1.0-eps+eps/narms) * as.numeric(A_test==A_opt) + (eps/narms) * as.numeric(A_test!=A_opt)
    } else {
      eps    <- 1.0
      A_test <- sample(seq(narms), batch_size, TRUE)
      P_test <- rep(1/narms, batch_size)
    }
    Y_test <- rnorm(batch_size, as.numeric(data$arms[idx]==A_test), data$reward_variance)
    block  <- (batch-1)*batch_size + seq(batch_size)
    loggpi$X[block,] <- X_test
    loggpi$A[block,] <- A_test
    loggpi$Y[block,] <- Y_test
    loggpi$P[block,] <- P_test
    loggpi$epsilon[block] <- rep(eps, batch_size)
    loggpi$regret[block]  <- as.numeric(data$arms[idx] != A_test)
    if (!length(outcome_models)) {
      loggpi$prevP[(bigblok[batch]+1):bigblok[batch+1]] <- rep(1.0 / narms, bigblok[batch+1])
    } else {
      fdx   <- tail(block, 1)
      Y_hat <- matrix(NA, nrow = fdx, ncol = narms)
      for (a in seq_along(outcome_models)) {
        ma        <- outcome_models[[a]]
        Y_hat[,a] <- unname(predict(ma, data.frame(loggpi$X[1:fdx,])))
      }
      A_best <- apply(Y_hat, 1, which.max)
      loggpi$prevP[(bigblok[batch]+1):bigblok[batch+1]] <- (loggpi$A[1:fdx] == A_best) * (1-eps+eps/narms) + 
        (loggpi$A[1:fdx] != A_best) * eps / narms
    }
  }
}

logging_policy_test <- function(idx = index[1L], ...) {
  args <- list(...)
  if (is.null(args[["batch_count"]])) {
    batch_count <- 100L
  }
  if (is.null(args[["batch_size"]])) {
    batch_size  <- 100L
  }
  if (is.null(args[["outcome_model"]])) {
    outcome_model <- rpart::rpart
  }
  if (is.null(args[["epsmult"]])) {
    epsmult <- 1.0
  }
  bandit <- LoggingPolicy(batch_count, batch_size, outcome_model, epsmult)
  data   <- DataUnit(idx)
  logging_policy_run(bandit, data)
  bandit
}
