
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
  # ntrain <- opeesim$ntrain
  # Get bandit logs
  bandit <- opeesim$banditsim$bandits[[i]]
  # Initialize OPEE object
  opee   <- OPEEstimator(bandit = bandit, arm_count = length(unique(bandit$A)))
  # Execute contextual target policy
  contextual(opee$target, batch=args[["batch"]], outcome_model=opeesim$outcome_model)
  # Fit the data
  CrossFitting(opee, outcome_model = opeesim$training_model)
  # Compute estimators
  compute_truth(opee)
  compute_dm(opee)
  compute_ips(opee)
  compute_dr(opee)
  compute_adr(opee)
  compute_mrdr(opee)
  compute_cadr(opee)
  # Store output
  opeesim$opees[[i]] <- opee
}

get_metrics <- function(opee, ...) {
  args <- list(...)
  coverage   <- vector(mode = "numeric", length = length(args[["estimators"]]))
  for (i in seq_along(args[["estimators"]])) {
    e <- args[["estimators"]][i]
    range <- opee[[e]][1L] + qnorm(0.975)*c(-1.,1.)*sqrt(opee[[e]][2L])
    coverage[i] <- as.numeric(dplyr::between(opee$truth[1], range[1L], range[2L]))
  }
  coverage
}

opee_simulator_test <- function(opeesim, i, world, banditsim, ...) {
  stopifnot(length(index)>0L)
  ## Experiment
  args <- list(...)
  # Initialize bandit
  banditsim$bandit <- LoggingPolicy(batch_count = 100L, batch_size = 100L, outcome_model = opeesim$training_model, epsmult = 0.01)
  # Initialize bandit simulation
  # banditsim <- BanditSimulator(bandit)
  # Simulate bandit: execute logging policy
  bandit_simulation(banditsim, i = i, data = world)
  # Initialize OPE simulator
  # opeesim <- OPEEsimulator(banditsim, lm, lm, 100L)
  # Run simulation
  opee_simulation(opeesim, i, batch = get_new_batch(world, size = opeesim$ntrain))
  # Compute coverage
  get_metrics(opeesim$opees[[i]], estimators = args[["estimators"]])
}

sequential_exp <- function(...) {
  i <- 1L   # data set index
  N <- 64L  # number of experiments
  estimators <- c("dm", "ips", "dr", "adr", "mrdr", "cadr")
  coverage <- rep(0L, length(estimators))
  for (j in seq(N)) {
    print(j)
    coverage <- coverage + opee_simulator_test(i, estimators = estimators)
  }
}
