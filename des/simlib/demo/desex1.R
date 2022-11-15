rm(list=ls())
setwd("/Users/gva/solutions/des")

Rcpp::sourceCpp("./simlib/src/des.cpp")

## Single-server queuing system
config <- function(env) {
  with(env, {
    EVENT_ARRIVAL   <- 1L
    EVENT_DEPARTURE <- 2L
    LIST_QUEUE      <- 1L
    LIST_SERVER     <- 2L
    SAMPST_DELAYS   <- 1L
  })
}

init.env <- function(env) {
  with(env, {
    num_custs_delayed   <- 0L
    num_delays_required <- 1000L
    mean_interarrival   <- 0.0
    mean_service        <- 0.0
  })
}

init_model <- function(env) {
  env$num_custs_delayed <- 0L
  arrival_at <- env$des$get_simtime() + rexp(1L, rate = 1.0)
  env$des$event_schedule(arrival_at, env$EVENT_ARRIVAL)
}

arrive <- function(env) {
  next_arrival_at <- env$des$get_simtime() + rexp(1L, rate = 1.0)
  env$des$event_schedule(next_arrival_at, env$EVENT_ARRIVAL)
  if (env$des$get_listsize(env$LIST_SERVER) == 1L) {
    env$des$set_transfer(1L, env$des$get_simtime())
    env$des$insert(env$LIST_QUEUE, TRUE, 2L) # Insert last
  } else {
    env$des$sampst(0.0, env$SAMPST_DELAYS)
    env$num_custs_delayed <- env$num_custs_delayed + 1L
    env$des$insert(env$LIST_SERVER, TRUE, 1L) # Insert first
    departure_at <- env$des$get_simtime() + rexp(1L, rate = 2.0)
    env$des$event_schedule(departure_at, env$EVENT_DEPARTURE)
  }
}

depart <- function(env) {
  if (env$des$get_listsize(env$LIST_QUEUE) == 0L) {
    env$des$remove(env$LIST_SERVER, TRUE) # Remove first
  } else {
    env$des$remove(env$LIST_QUEUE, TRUE) # Remove first
    env$des$sampst(env$des$get_simtime() - env$des$get_transfer(1L), SAMPST_DELAYS)
    env$num_custs_delayed <- env$num_custs_delayed + 1L
    departure_at <- env$des$get_simtime() + rexp(1L, rate = 2.0)
    env$des$event_schedule(departure_at, env$EVENT_DEPARTURE)
  }
}

env <- globalenv()
config(env)
init.env(env)
## Initalize discrete-event simulation object
env$des <- new(DES, 10L, 4L)
init_model(env)
while (env$num_custs_delayed < env$num_delays_required) {
  env$des$timing()
  switch (env$des$get_next_event_type(),
    arrive(env),
    depart(env)
  )
}
env$des$sampst(0.0, -1L)
round(plyr::laply(seq(4L), function(x) env$des$get_transfer(x)), 3L)
for (j in seq(2L)) {
  env$des$timest(0.0, -(25L+j))
  print(round(plyr::laply(seq(3L), function(x) env$des$get_transfer(x)), 3L))
}

