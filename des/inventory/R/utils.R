## Initialization routine
init.env <- function(env, ...) {
  # State variables
  env$inv_level           <- env$initial_inv_level
  env$time_last_event     <- 0.0
  # Counters
  env$amount              <- NA
  env$next_event_type     <- NA
  env$total_ordering_cost <- 0.0
  env$area_holding        <- rep(0.0,2)
  env$area_shortage       <- rep(0.0,2)
  env$monidx              <- 1L # Current month index
  env$total_month_cost    <- vector(mode = "numeric", length = env$num_months)
  # Simulation clock
  env$sim_time            <- 0.0
  # Event list
  env$next_event          <- c(Inf, env$sim_time+rDemandTime(env$mean_interdemand), env$num_months, env$t4)
}

## Determine next event type and advance clock
timing <- function(env) {
  idx                 <- which.min(env$next_event)
  env$next_event_type <- idx
  env$sim_time        <- env$next_event[idx]
}

## Update area accumulators for time-average statistics
update_stats <- function(env) {
  # Time since last event
  time_since_last_event  <- env$sim_time - env$time_last_event
  env$time_last_event    <- env$sim_time
  # Status of the inventory level during the previous interval
  if (env$inv_level < 0.0) {
    env$area_shortage[1] <- env$area_shortage[2]
    env$area_shortage[2] <- env$area_shortage[2] - env$inv_level * time_since_last_event
  } else if (env$inv_level > 0.0) {
    env$area_holding[1] <- env$area_holding[2]
    env$area_holding[2] <- env$area_holding[2] + env$inv_level * time_since_last_event
  }
}

## Order arrival routine
order_arrival <- function(env) {
  # Increment the inventory level by the amount ordered
  env$inv_level     <- env$inv_level + env$amount
  # Eliminate the order arrival event (since there is no residual order)
  env$next_event[1] <- Inf
}

## Demand routine
demand <- function(env, ...) {
  # Decrement the inventory level by a generated demand size
  env$inv_level     <- env$inv_level - rDemand(seq(env$num_values_demand), env$prob_distr_demand)
  # Schedule the time of the next demand
  env$next_event[2] <- env$sim_time + rDemandTime(env$mean_interdemand)
}

## Inventory evaluation function for (s,S) policy, i.e., (minimum level, maximum level)
evaluate.S <- function(env, ...) {
  order_cost <- 0.0
  if (env$inv_level < env$small_s[i]) {
    # Place an order for the appropriate amount
    env$amount <- env$big_s[i] - env$inv_level
    order_cost <- env$setup_cost + env$incremental_cost * env$amount
    env$total_ordering_cost <- env$total_ordering_cost + order_cost
    # Schedule the arrival of the order
    env$next_event[1] <- env$sim_time + rOrderTime(env$minlag,env$maxlag)
  }
  env$total_month_cost[env$monidx] <- order_cost + 
    diff(env$area_shortage) * env$shortage_cost + 
    diff(env$area_holding)  * env$holding_cost
  env$monidx <- env$monidx + 1L
  # Schedule the next inventory evaluation
  env$next_event[4] <- env$sim_time + 1.0
}

## Inventory evaluation function for (s,d) policy, i.e., (minimum level, order size)
evaluate.d <- function(env, ...) {
  order_cost <- 0.0
  if (env$inv_level < env$small_s[i]) {
    # Place an order for the appropriate amount
    env$amount <- env$order_size[i] + env$small_s[i] - env$inv_level
    order_cost <- env$setup_cost + env$incremental_cost * env$amount
    env$total_ordering_cost <- env$total_ordering_cost + order_cost
    # Schedule the arrival of the order
    env$next_event[1] <- env$sim_time + rOrderTime(env$minlag,env$maxlag)
  }
  env$total_month_cost[env$monidx] <- order_cost + 
    diff(env$area_shortage) * env$shortage_cost + 
    diff(env$area_holding)  * env$holding_cost
  env$monidx <- env$monidx + 1L
  # Schedule the next inventory evaluation
  env$next_event[4] <- env$sim_time + 1.0
}

