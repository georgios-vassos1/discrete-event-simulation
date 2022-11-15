# Auxiliary 
rDemand     <- function(d.vec, p)     sample(d.vec, 1, prob=p)
rDemandTime <- function(mean.dem)     rexp(1, 1.0/mean.dem)
rOrderTime  <- function(lower, upper) runif(1, lower, upper)

## Input configuration
input.conf <- function(env) {
  with(env, {
    big_s               <- NA
    order_size          <- c(20L,80L,20L,60L)
    initial_inv_level   <- 60L
    prob_distr_demand   <- c(1/6,1/3,1/3,1/6) # For discrete demand density
    num_policies        <- length(order_size)
    num_events          <- 4L
    num_months          <- 120L
    num_values_demand   <- length(prob_distr_demand)
    small_s             <- c(20L,20L,10L,30L)
    holding_cost        <- 1.0
    incremental_cost    <- 3.0
    maxlag              <- 1.00
    mean_interdemand    <- 0.10
    minlag              <- 0.50
    setup_cost          <- 32.0
    shortage_cost       <- 5.0
    sim_time            <- 0.0
    t4                  <- 0.0
  })
  assertthat::are_equal(length(env$big_s),length(env$small_s))
}
