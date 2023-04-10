
collapse <- function(point, m) {
  (point[1L]-1L) * m + point[2L]
}

audit <- function(point, m, n) {
  point[1L] <- max(point[1L],1L)
  point[2L] <- max(point[2L],1L)
  point[1L] <- min(point[1L],m)
  point[2L] <- min(point[2L],n)
  point
}

move2d <- function(...) {
  mv2d <- matrix(0L, nrow = 4L, ncol = 2L)
  mv2d[1L,1L] <- -1L # left
  mv2d[2L,2L] <- -1L # down
  mv2d[3L,2L] <-  1L # up
  mv2d[4L,1L] <-  1L # right
  mv2d
}

GridWorld <- function(m, n, blocked=NULL, goal=NULL, ...) {
  gwd <- new.env(hash = FALSE, parent = emptyenv())
  gwd$terrain <- matrix(0L, nrow = m, ncol = n)
  gwd$blocked <- blocked
  if (!is.null(blocked)) {
    gwd$blocked_states <- collapse(unname(unlist(blocked)), m)
    for (item in blocked) {
      gwd$terrain[item$x,item$y] <- -Inf
    }
  }
  if (!is.null(goal)) {
    for (item in goal) {
      gwd$terrain[item$x,item$y] <- item$reward
    }
  }
  gwd$S <- c(1L:(m*n))
  gwd$A <- seq(4L)
  gwd$action_index <- list("left" = 1L, "down" = 2L, "up" = 3L, "right" = 4L, "terminate" = 'x')
  gwd$action_alias <- list("left", "down", "up", "right", "x" = "terminate")
  gwd$action2d <- move2d()
  class(gwd) <- "Grid World"
  return(gwd)
}

step <- function(env, point, a) {
  m <- nrow(env$terrain)
  n <- ncol(env$terrain)
  new_point <- audit(point + env$action2d[a,], m, n)
  if (env$terrain[new_point[1L], new_point[2L]] == -Inf) new_point <- point
  new_state <- collapse(new_point, m)
  is_done   <- (new_state == (m*n)) # Terminal state
  list(
    new_state = new_state,
    new_point = new_point,
    reward    = -1L,
    is_done   = is_done
  )
}

get_transitions <- function(env, point, ...) {
  eps  <- runif(1L,.0,.1)
  prob <- 1. - (2. * eps)
  new_states <- rep(NA, length(env$A))
  for (i in env$A) {
    new_states[i] <- step(env, point, i)$new_state
  }
  list(
    "left"  = setNames(stack(tapply(c(prob, eps,  eps,  0.0),  new_states, sum)), c("prob", "state")),
    "down"  = setNames(stack(tapply(c(eps,  prob, eps,  0.0),  new_states, sum)), c("prob", "state")),
    "up"    = setNames(stack(tapply(c(eps,  0.0,  prob, eps),  new_states, sum)), c("prob", "state")),
    "right" = setNames(stack(tapply(c(eps,  0.0,  eps,  prob), new_states, sum)), c("prob", "state")),
    "terminate" = setNames(c(1.0, Inf), c("prob", "state"))
  )
}

TabularPolicy <- function(Ns, ...) {
  tabpi <- new.env(hash = FALSE, parent = emptyenv())
  tab   <- vector(mode = "integer", length = Ns) # state index to action index
  class(tabpi) <- "Tabular Policy"
  return(tabpi)
}

select_action <- function(tpi, s) {
  tpi$tab[s]
}

update_tpi <- function(tpi, s, a) {
  tpi$tab[s] <- a
}

scenario.0 <- function(...) {
  blocked <- list(
    list(x = 2L, y = 2L)
  )
  goal <- list(
    list(x = 1L, y = 4L, reward =  1.0),
    list(x = 2L, y = 4L, reward = -1.0)
  )

  m <- 3L
  n <- 4L
  env <- GridWorld(m, n, blocked = blocked, goal = goal)
  env
}

env <- scenario.0()
env$terrain


## Standard test
scenario.x <- function(...) {
  blocked <- list(
    list(x = c(6L:10L), y = 4L),
    list(x = c(1L: 7L), y = 8L)
  )

  m <- 10L
  n <- 10L
  env <- GridWorld(m, n, blocked=blocked)
  env
}

env <- scenario.x()
env$terrain

policy = function(Q, s) {
  return(which.max(Q[s,]))
}

learn_q <- function(env, N = 1000L, start = c(1L,1L), gamma = 1.0, alpha = 0.6) {
  m <- nrow(env$terrain)
  Q <- matrix(0L, nrow = length(env$S), ncol = length(env$A))
  for (n in c(1L:N)) {
    point <- start
    s <- collapse(point, m)
    while (TRUE) {
      a <- policy(Q, s)
      outcome <- step(env, point, a)
      q <- outcome$reward + gamma * max(Q[outcome$new_state,])
      Q[s,a] = (1-alpha) * Q[s,a] + alpha * q
      s = outcome$new_state
      point = outcome$new_point
      if (outcome$is_done) break
    }
  }
  Q
}

Q <- learn_q(env)
V <- apply(Q, 1L, max)
dim(V) <- dim(env$terrain)
t(V)
