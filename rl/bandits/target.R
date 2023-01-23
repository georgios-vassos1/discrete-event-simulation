
TargetPolicy <- function(...)  {
  args     <- list(...)
  targetpi <- new.env(hash = FALSE, parent = emptyenv())
  targetpi$arm_count         <- args[["arm_count"]]
  targetpi$nx_assignment     <- NULL
  targetpi$cx_outcome_models <- NULL
  targetpi$contextual        <- NULL
  class(targetpi) <- "Target policy"
  return(targetpi)
}

nx_const <- function(targetpi, ...) {
  args <- list(...)
  arm  <- args[["arm"]]
  targetpi$nx_assignment       <- rep(0L, targetpi$arm_count)
  targetpi$nx_assignment[arm] <- 1L
  targetpi$contextual <- FALSE
}

nx_runif <- function(targetpi, ...) {
  narms <- targetpi$arm_count
  targetpi$nx_assignment <- rep(1.0, narms) / narms
  targetpi$contextual <- FALSE
}

nx_greedy  <- function(targetpi, ...) { # Argument added for later convenience
  args <- list(...)
  arm_counts    <- table(args[["arms"]])
  count_per_arm <- as.numeric(arm_counts)
  targetpi$nx_assignment <- rep(0L, targetpi$arm_count)
  targetpi$nx_assignment[which.max(count_per_arm)] <- 1L
  targetpi$contextual <- FALSE
}

nx_stoch  <- function(targetpi, ...) { # Argument added for later convenience
  args <- list(...)
  arm_counts    <- table(args[["arms"]])
  count_per_arm <- as.numeric(arm_counts)
  targetpi$nx_assignment <- rep(0L, targetpi$arm_count)
  targetpi$nx_assignment <- count_per_arm / sum(count_per_arm)
  targetpi$contextual <- FALSE
}

contextual <- function(targetpi, ...) {
  args     <- list(...)
  ntrain   <- nrow(args[["batch"]]$context)
  narms    <- targetpi$arm_count
  arms_    <- sample(seq(narms), ntrain, TRUE)
  rewards  <- rnorm(ntrain, as.numeric(args[["batch"]]$arms==arms_), 1.0)
  targetpi$cx_outcome_models <- list()
  j <- 1
  for (a in seq(narms)) {
    ida <- which(arms_ == a)
    Xa  <- args[["batch"]]$context[ida,]
    Ya  <- rewards[ida]
    targetpi$cx_outcome_models[[j]] <- args[["outcome_model"]](Ya ~ ., data.frame(Ya=Ya, Xa))
    j   <- j + 1
  }
  targetpi$contextual <- TRUE
}

policy <- function(targetpi, X_new, ...) {
  args  <- list(...)
  if (is.vector(X_new)) X_new <- cbind(X_new)
  n     <- nrow(X_new)
  narms <- targetpi$arm_count
  if (!targetpi$contextual) {
    return(matrix(rep(tp$nx_assignment, each=n), ncol=narms))
  } else {
    Y_hat <- matrix(NA, nrow = n, ncol = narms)
    A_hat <- matrix(NA, nrow = n, ncol = narms)
    for (a in seq(narms)) {
      Y_hat[,a] <- predict(targetpi$cx_outcome_models[[a]], data.frame(X_new))
    }
    opt_arm <- apply(Y_hat, 1, which.max)
    for (a in seq(narms)) {
      A_hat[,a] <- as.numeric(opt_arm == a)
    }
    return(A_hat)
  }
}

if (FALSE) {
  world <- DataUnit(index[1L])
  tp <- TargetPolicy(arm_count = length(unique(c(world$arms))))
  nx_const(tp, arm=2L)
  tp$nx_assignment
  nx_runif(tp)
  tp$nx_assignment
  nx_greedy(tp, arms=world$arms)
  tp$nx_assignment
  nx_stoch(tp, arms=world$arms)
  tp$nx_assignment
  policy(tp, world$context)
  # Test contextual policy
  shuffle <- sample(seq(nrow(world$context)), nrow(world$context), FALSE)
  world$context <- world$context[shuffle,]
  world$arms    <- world$arms[shuffle]
  contextual(tp, batch = world, outcome_model=lm)
  tp$cx_outcome_models
  policy(tp, world$context)
}
