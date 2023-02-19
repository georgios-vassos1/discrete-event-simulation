
BanditSimulator <- function(bandit, seed=1704, ...) {
  banditsim <- new.env(hash = FALSE, parent = emptyenv())
  args      <- list(...)
  banditsim$bandit    <- bandit
  banditsim$seed      <- seed
  banditsim$bandits   <- list()
  banditsim$sim_count <- args[["sim_count"]]
  class(banditsim) <- "Bandit simulator"
  return(banditsim)
}

bandit_simulation <- function(banditsim, i, data) {
  # Copy bandit
  banditsim$bandits[[i]] <- LoggingPolicy(
    batch_count   = banditsim$bandit$batch_count,
    batch_size    = banditsim$bandit$batch_size,
    outcome_model = banditsim$bandit$outcome_model,
    epsmult       = banditsim$bandit$epsmult)
  # Execute logging policy
  logging_policy_run(banditsim$bandits[[i]], data)
}

bandit_simulator_test <- function(...) {
  stopifnot(length(index)>0L)
  # Get data
  world  <- DataUnit(index[1L])
  narms  <- world$arm_count
  # Initialize bandit
  bandit <- LoggingPolicy(batch_count = 100L, batch_size = 100L, outcome_model = lm, epsmult = 0.01)
  # Initialize bandit simulation
  banditsim <- BanditSimulator(bandit)
  # Simulate bandit: execute logging policy
  bandit_simulation(banditsim, i = 1L, data = world)
  # Return bandit logs
  banditsim$bandits[[1L]]
}
