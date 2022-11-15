rm(list=ls())
setwd("/Users/gva/solutions/des")

Rcpp::sourceCpp("./simlib/src/des.cpp")

## Job-shop model
config <- function(env) {
    with(env, {
      # Event type for arrival of a job to the system
      EVENT_ARRIVAL         <- 1L
      # Event type for departure of a job from a particular station 
      EVENT_DEPARTURE       <- 2L
      # Event end simulation 
      EVENT_END_SIMULATION  <- 3L
      # Maximum number of stations
      MAX_NUM_STATIONS      <- 5L
      # Maximum number of job types
      MAX_NUM_JOB_TYPES     <- 3L
      # Probability of arrival of each job type
      prob_distrib_job_type <- c(0.3,0.5,0.2)
      # mean interarrival time of jobs equal to 1/4=0.25 hours
      mean_interarrival     <- 0.25
      # Duration of the simulation
      length_simulation     <- 365L
      # Workstations in routing
      route <- list(
        c(3L,1L,2L,5L),
        c(4L,1L,3L),
        c(2L,5L,1L,4L,3L)
      )
      # Mean service times for successive tasks in hours
      mean_service <- list(
        c(0.50, 0.60, 0.85, 0.50),
        c(1.10, 0.80, 0.75),
        c(1.20, 0.25, 0.70, 0.90, 1.00)
      )
      # Job type global variable
      num_tasks <- plyr::laply(route, length)
    })
}

init.env <- function(env) {
    with(env, {
      # Global auxilia
      num_stations  <- MAX_NUM_STATIONS
      num_job_types <- MAX_NUM_JOB_TYPES
      job_type      <- NA
      task          <- NA
      # Machine utilization
      num_machines      <- c(3L,2L,4L,3L,1L)
      num_machines_busy <- rep(0L, MAX_NUM_STATIONS)
    })
}

arrive <- function(env, newjob) {
  # If this is a new arrival to the system, then 
  # generate the time of the next arrival and 
  # determine the job type and task number of the arriving job.
  if (newjob == 1) {
    next_event_at <- env$des$get_simtime() + rexp(1, 1.0/env$mean_interarrival)
    env$des$event_schedule(next_event_at, env$EVENT_ARRIVAL)
    env$job_type <- sample(seq(env$num_job_types), 1L, prob=env$prob_distrib_job_type)
    env$task     <- 1L
    # print(paste0("Schedule next event at ", next_event_at, " with job type ", env$job_type, " and task ", env$task))
  }
  # Determine the station from the route matrix
  with(env, station <- route[[job_type]][task])
  #  Check to see whether all machines in this station are busy
  if (with(env, num_machines_busy[station] == num_machines[station])) {
    # print(paste0("All machines at station ", env$station, " are busy."))
    # All machines in this station are busy, so place the arriving job at the end of the appropriate queue. 
    # Note that the following data are stored in the record for each job:
    #  1. Time of arrival to this station.
    #  2. Job type.
    #  3. Current task number
    with(env, des$set_transfer(1L, des$get_simtime()))
    with(env, des$set_transfer(2L, job_type))
    with(env, des$set_transfer(3L, task))
    with(env, des$insert(station, TRUE, 2L))
  } else {
    # print(paste0("A machine at station ", env$station, " is idle."))
    # A machine in this station is idle, so start service on the arriving job (which has a delay of zero)
    with(env, des$sampst(0.0, station))
    with(env, des$sampst(0.0, num_stations + job_type))
    with(env, num_machines_busy[station] <- num_machines_busy[station] + 1L)
    with(env, des$timest(1.0*num_machines_busy[station], station))
    # Schedule a service completion. Note defining attributes beyond the first two 
    # for the event record before invoking event_schedule
    with(env, des$set_transfer(3L, job_type))
    with(env, des$set_transfer(4L, task))
    next_departure_at <- env$des$get_simtime() + rgamma(1L, shape=2.0, scale=with(env, mean_service[[job_type]][task]/2.0))
    env$des$event_schedule(next_departure_at, env$EVENT_DEPARTURE)
  }
}

# Event function for departure of a job from a particular station
depart <- function(env) {
  # print(paste0("Departure at ", env$des$get_simtime()))
  # Determine the station from which the job is departing
  with(env, job_type <- des$get_transfer(3L))
  with(env, task     <- des$get_transfer(4L))
  with(env, station  <- route[[job_type]][task])
  # Check to see whether the queue for this station is empty
  if (with(env, des$get_listsize(station) == 0)) {
    # print(paste0("The queue at station ", env$station, " is empty."))
    # The queue for this station is empty, so make a machine in this station idle
    with(env, num_machines_busy[station] <- num_machines_busy[station] - 1L)
    with(env, des$timest(1.0*num_machines_busy[station], station))
  } else {
    # print(paste0("XXX The queue at station ", env$station, " is not empty."))
    # The queue is nonempty, so start service on first job in queue
    with(env, des$remove(station, TRUE))
    # print(paste0("XXX First job in queue arrived at ", env$des$get_transfer(1L)))
    # Update global counters
    delay[[station]] <<- c(delay[[station]], des$get_simtime() - des$get_transfer(1L))
    # Tally this delay for this station
    with(env, des$sampst(des$get_simtime() - des$get_transfer(1L), station))
    # Tally this same delay for this job type
    job_type_queue <- env$des$get_transfer(2L)
    task_queue     <- env$des$get_transfer(3L)
    env$des$sampst(env$des$get_simtime() - env$des$get_transfer(1L), env$num_stations + job_type_queue)
    # Schedule end of service for this job at this station. 
    # Note defining attributes beyond the first two for the event record before invoking event_schedule.
    env$des$set_transfer(3L, job_type_queue)
    env$des$set_transfer(4L, task_queue)
    next_departure_at <-
      env$des$get_simtime() + rgamma(1L, shape=2.0, scale=env$mean_service[[job_type_queue]][task_queue]/2.0)
    # print(paste0("XXX Schedule the next departure at ", next_departure_at))
    env$des$event_schedule(next_departure_at, env$EVENT_DEPARTURE)
  }
  # If the current departing job has one or more tasks yet to be done, 
  # send the job to the next station on its route
  if (with(env, task < num_tasks[job_type])) {
    # print(paste0("Current departing job has one or more tasks yet to be done."))
    with(env, task <- task + 1L)
    arrive(env, 2L)
  }
}

## Job-shop discrete-event simulation model
env <- globalenv()
config(env)
# Initialize environment
init.env(env)
# Global counters
delay   <- vector(mode = "list", length = env$num_stations)
## Initalize discrete-event simulation object
env$des <- new(DES, 10L, 4L)
# Schedule the arrival of the first job.
env$des$event_schedule(rexp(1, 1.0/env$mean_interarrival), env$EVENT_ARRIVAL)
# Schedule the end of the simulation.
env$des$event_schedule(8L*env$length_simulation, env$EVENT_END_SIMULATION)
# Run simulation
while (1) {
  env$des$timing()
  switch (env$des$get_next_event_type(),
    arrive(env,1L),
    depart(env),
    break
  )
}
# Output
overall_avg_job_tot_delay <- 0.0
for (i in seq(env$num_job_types)) {
  avg_job_tot_delay <- env$des$sampst(0.0, -(env$num_stations + i)) * env$num_tasks[i]
  overall_avg_job_tot_delay <- overall_avg_job_tot_delay + env$prob_distrib_job_type[i] * avg_job_tot_delay
}
overall_avg_job_tot_delay
for (j in seq(env$num_stations)) {
  print(paste0(
    j, " | ",
    round(env$des$filest(j), 4L), " | ",
    round(env$des$timest(0.0, -j)/env$num_machines[j], 4L), " | ",
    round(env$des$sampst(0.0, -j), 4L)))
}
