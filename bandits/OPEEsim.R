
OPEEsimulator <- function(banditsim, outcome_model, training_model, ntrain, seed=1704, ...) {
  # training_model: training outcome model
  # ntrain: train sample size
  OPEEsim <- new.env(hash = FALSE, parent = emptyenv())
  args    <- list(...)
  OPEEsim$banditsim      <- banditsim
  OPEEsim$outcome_model  <- outcome_model
  OPEEsim$training_model <- training_model
  OPEEsim$ntrain    <- ntrain
  OPEEsim$seed      <- seed
  OPEEsim$sim_count <- args[["sim_count"]]
  OPEEsim$target    <- TargetPolicy(arm_count=args[["arm_count"]])
  OPEEsim$opees     <- list()
  class(OPEEsim) <- "Monte Carlo search tree"
  return(OPEEsim)
}

opee_simulation <- function(opeesim, i, ...) {
  args   <- list(...)
  ntrain <- OPEEsim$ntrain
  # Get bandit logs
  bandit <- opeesim$banditsim$bandits[[i]]
  # Initialize OPEE object
  opee   <- OPEEstimator(bandit = bandit, arm_count = length(unique(bandit$A)))
  # Execute contextual target policy
  contextual(opee$target, batch=args[["batch"]], outcome_model=args[["outcome_model"]])
  # Fit the data
  CrossFitting(opee)
  # Compute estimators
  compute_truth(opee)
  # Store output
  OPEEsim$opees[[i]] <- opee
}
