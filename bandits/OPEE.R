
OPEEstimator <- function(...)  {
  args <- list(...)
  opee <- new.env(hash = FALSE, parent = emptyenv())
  opee$arm_count <- args[["arm_count"]]
  opee$bandit    <- args[["bandit"]]
  opee$truth  <- NULL
  opee$dm     <- NULL
  opee$ips    <- NULL
  opee$dr     <- NULL
  opee$adr    <- NULL
  opee$cadr   <- NULL
  opee$mrdr   <- NULL
  opee$camrdr <- NULL
  # Train the outcome models per arm and store the predictions
  opee$Q        <- NULL
  opee$Q_MRDR   <- NULL
  opee$Q_CAMRDR <- NULL
  opee$target   <- TargetPolicy(arm_count=args[["arm_count"]])
  class(opee) <- "OPE estimator"
  return(opee)
}

# (X, A, Y, P, prevP, epsilon, Y.0) are the output of the logging policy
CrossFitting <- function(opee, outcome_model=lm, nfolds=5L, ...) {
  args   <- list(...)
  n      <- nrow(opee$bandit$X)
  narms  <- opee$arm_count
  # Train the outcome models per arm and store the predictions
  Q <- Q_MRDR <- Q_CAMRDR <- matrix(NA, nrow = n, ncol = narms)
  ## Cross-fitting loop
  fold_size <- n %/% nfolds
  for (k in seq(nfolds)) {
    train.folds <- train.idx(k, nfolds)
    train <- c(vapply(train.folds, block.idx, FUN.VALUE = rep(0.0, fold_size), size = fold_size))
    valid <- block.idx(k, fold_size)
    X_train <- opee$bandit$X[train,]
    A_train <- opee$bandit$A[train,]
    Y_train <- opee$bandit$Y[train,]
    P_train <- opee$bandit$P[train,]
    X_valid <- opee$bandit$X[valid,]
    for (a in seq(narms)) {
      ida <- as.logical(A_train == a)
      if (!sum(ida)) next
      Xa <- X_train[ida,]
      Ya <- Y_train[ida]
      # Train unweighted outcome model.
      ma         <- outcome_model(Ya ~ ., data.frame(Ya=Ya, Xa))
      Q[valid,a] <- unname(predict(ma, data.frame(X_valid)))
      # Train MRDR-weighted outcome model.
      # Loss function: \sum_{i=1}^{N} w_i (y_i - \hat{Q}_{a_i}(x_i))^2, 
      #                w_i = g^*(a_i | x_i) * (1 - g_t(a_i | x_i)) / g_t(a_i | x_i)^2
      g_a      <- P_train[ida]
      g_star_a <- policy(opee$target, Xa)[,a]
      w_a <- NA
      if (all(g_star_a==0.0)) {
        w_a <- rep(1.0, length(Ya))
      } else {
        w_a <- g_star_a * (1-g_a) / g_a^2
      }
      ma <- outcome_model(Ya ~ ., data = data.frame(Ya=Ya, Xa), weights=w_a)
      Q_MRDR[valid,a] <- unname(predict(ma, data.frame(X_valid)))
      if (all(g_star_a==0.0)) {
        w_a <- rep(1.0, length(Ya))
      } else {
        w_a <- g_star_a / g_a
      }
      ma <- outcome_model(Ya ~ ., data = data.frame(Ya=Ya, Xa), weights=w_a)
      Q_CAMRDR[valid,a] <- unname(predict(ma, data.frame(X_valid)))
    }
  }
  g_star <- policy(opee$target, opee$bandit$X)
  opee$Q             <- Q
  opee$Q_MRDR        <- Q_MRDR
  opee$Q_CAMRDR      <- Q_CAMRDR
  opee$g_star        <- g_star
  opee$Q_star        <- rowSums(g_star*Q)
  opee$Q_MRDR_star   <- rowSums(g_star*Q_MRDR)
  opee$Q_CAMRDR_star <- rowSums(g_star*Q_CAMRDR)
  opee$Q0_star       <- rowSums(sweep(g_star, 1, opee$bandit$Y, '*'))
}

mean_and_var <- function(D, w=NULL) {
  if (is.null(w)) w <- rep(1.0, length(D))
  mu <- as.numeric(D %*% (w / sum(w)))
  c(mu, as.numeric((((D-mu)^2) %*% (w^2)) / (sum(w)^2)))
}

# V^* \frac{1}{N} \sum_{t=1}^N \sum_{a} g^*(a | x_i) E[Y | A=a, X=x_t]
compute_truth <- function(opee, ...) {
  D   <- opee$Q0_star
  res <- mean_and_var(D)
  opee$truth <- c(res[1], 0.0)
}

# V = \frac{1}{N} \sum_{t=1}^N \sum_{a} g^*(a | x_t) \hat{Q}(x_t, a)
compute_dm <- function(opee, ...) {
  opee$dm <- mean_and_var(opee$Q_star)
}

# V = \frac{1}{N} \sum_{t=1}^N \frac{g^*(a_t | x_t)}{g_t(a_t | x_t)} y_t
compute_ips <- function(opee, ...) {
  A.dummy  <- unname(model.matrix(~A - 1, data.frame(A=factor(opee$bandit$A)))[,])
  g_star_a <- rowSums(A.dummy * opee$g_star)
  D <- (g_star_a / opee$bandit$P[,1]) * opee$bandit$Y[,1]
  opee$ips <- mean_and_var(D)
}

# V = \frac{1}{N} \sum_{t=1}^N (\sum_{a} g^*(a | x_t) \hat{Q}(x_t, a) 
#     + \frac{g^*(a_t | x_t)}{g_t(a_t | x_t)} (y_t - hat{Q}(x_t, a_t)))
compute_dr <- function(opee, ...) {
  A.dummy  <- unname(model.matrix(~A - 1, data.frame(A=factor(opee$bandit$A)))[,])
  g_star_a <- rowSums(A.dummy * opee$g_star)
  Q_a      <- rowSums(A.dummy * opee$Q)
  D        <- opee$Q_star + (g_star_a / opee$bandit$P[,1]) * (opee$bandit$Y[,1] - Q_a)
  opee$dr  <- mean_and_var(D)
}

# V = \frac{1}{\sum_{t=1}^N w_t} \sum_{t=1}^N w_t ((\sum_{a} g^*(a | x_t) \hat{Q}(x_t, a) 
#      + \frac{g^*(a_t | x_t)}{g_t(a_t | x_t)} (y_t - hat{Q}(x_t, a_t))))
# w_t = \sqrt(g_t(a_t | x_t))
compute_adr <- function(opee, ...) {
  A.dummy  <- unname(model.matrix(~A - 1, data.frame(A=factor(opee$bandit$A)))[,])
  g_star_a <- rowSums(A.dummy * opee$g_star)
  Q_a <- rowSums(A.dummy * opee$Q)
  D   <- opee$Q_star + (g_star_a / opee$bandit$P[,1]) * (opee$bandit$Y[,1] - Q_a)
  w   <- sqrt(opee$bandit$P[,1])
  opee$adr <- mean_and_var(D, w)
}

# D_t = \sum_{a} g^*(a | x_t) \hat{Q}_a(x_t) 
#     + \frac{g^*(a_t | x_t)}{g_t(a_t | x_t)} (y_t - hat{Q}(x_t, a_t))
# \frac{1}{N} \sum_{t=1}^N w_t D_t
# w_i = \sqrt{\frac{1}{i-1} \sum_{j}^{i-1} \frac{g_t(a_j | x_j)}{g_t(a_i | x_i)} D_t^2 
#             - \left(\frac{1}{i-1} \sum_{j}^{i-1} \frac{g_t(a_j | x_j)}{g_t(a_i | x_i)} D_t\right)^2}
compute_cadr <- function(opee, ...) {
  N  <- nrow(opee$bandit$X)
  bk <- opee$bandit$bigblok
  bs <- opee$bandit$batch_size
  A.dummy  <- unname(model.matrix(~A - 1, data.frame(A=factor(opee$bandit$A)))[,])
  g_star_a <- rowSums(A.dummy * opee$g_star)
  Q_a      <- rowSums(A.dummy * opee$Q)
  D        <- opee$Q_star + (g_star_a / opee$bandit$P[,1]) * (opee$bandit$Y[,1] - Q_a)
  w <- sigma2 <- vector(mode = "numeric", length = N)
  for (i in seq(2L,N)) {
    trail <- seq(i-1L)
    gs_os <- opee$bandit$P[trail]
    gt_os <- opee$bandit$prevP[bk[((i-1L)%/%bs)+1L]+trail]
    idx      <- (opee$bandit$A[trail]-1L)*(i-1L)+trail
    D1_gt_os <- opee$Q_star[trail] + 
      (opee$g_star[idx] / gt_os) * 
      (opee$bandit$Y[trail] - opee$Q[idx])
    psi1t     <- ((gt_os/gs_os)*(D1_gt_os^2.0))
    psi2t     <- ((gt_os/gs_os)*(D1_gt_os))
    sigma2[i] <- mean(psi1t) - mean(psi2t)^2.0
    w[i]      <- ifelse(sigma2[i] <= 0.0, 0.0, 1.0/sqrt(sigma2[i]))
  }
  opee$cadr <- mean_and_var(D, w)
}

# V = \frac{1}{N} \sum_{t=1}^N (\sum_{a} g^*(a | x_t) \hat{Q_MRDR}(x_t, a) 
#     + \frac{g^*(a_t | x_t)}{g_t(a_t | x_t)} (y_t - hat{Q_MRDR}(x_t, a_t)))
compute_mrdr <- function(opee, ...) {
  A.dummy  <- unname(model.matrix(~A - 1, data.frame(A=factor(opee$bandit$A)))[,])
  g_star_a <- rowSums(A.dummy * opee$g_star)
  Q_MRDR_a <- rowSums(A.dummy * opee$Q_MRDR)
  D <- opee$Q_MRDR_star + (g_star_a / opee$bandit$P[,1]) * (opee$bandit$Y[,1] - Q_MRDR_a)
  opee$mrdr <- mean_and_var(D)
}

opee_test <- function(...) {
  world  <- DataUnit(index[1L])
  narms  <- world$arm_count # length(unique(world$arms))
  bandit <- LoggingPolicy(batch_count = 100L, batch_size = 100L, outcome_model = lm, epsmult = 0.01)
  logging_policy_run(bandit, world)
  opee  <- OPEEstimator(arm_count = narms, bandit = bandit)
  contextual(opee$target, batch=get_new_batch(world), outcome_model=lm)
  CrossFitting(opee)
  compute_truth(opee)
}
