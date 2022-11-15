# Auxiliary 
rDemand     <- function(d.vec, p)     rpois(1, 25.0)
rDemandTime <- function(mean.dem)     1.0
rOrderTime  <- function(lower, upper) 0.0

## Input configuration
input.conf <- function(env) {
  with(env, {
    big_s               <- 57L
    initial_inv_level   <- 57L
    prob_distr_demand   <- NA
    num_policies        <- length(big_s)
    num_events          <- 4L
    num_months          <- 120L
    num_values_demand   <- NA
    small_s             <- 17L
    holding_cost        <- 1.0
    incremental_cost    <- 3.0
    maxlag              <- NA
    mean_interdemand    <- 1.0
    minlag              <- NA
    setup_cost          <- 32.0
    shortage_cost       <- 5.0
    sim_time            <- 0.0
    t4                  <- 1.0-1e-6 # Time of first order
  })
  assertthat::are_equal(length(env$big_s),length(env$small_s))
}
