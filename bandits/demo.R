
env <- new.env()

env$N  <- 500L
env$Na <- 2L
env$X  <- mvtnorm::rmvnorm(env$N, mean = c(0.7, 0.2))
env$A  <- sample(seq(env$Na), env$N, TRUE) # Randomized assignment
env$A  <- matrix(c(model.matrix(~-1+factor(env$A))), ncol=env$Na)

# True parameters
env$beta  <- c(0.0, 0.4)
env$gamma <- c(0.5, 0.2)

# Y(a) = beta a + X^{T}gamma
env$Y <- sweep(sweep(env$A, 2L, env$beta, '*'), 1L, env$X %*% env$gamma, '+')

run_experiment <- function(env, ...) {
  N      <- env$N
  Na     <- env$Na
  beta   <- env$beta
  gamma  <- env$gamma
  # Initialization
  burnin <- 10L
  beta.hat  <- c(.1,.1)
  gamma.hat <- c(.1,.1)
  # Burn-in period
  a.hist <- NULL
  r.hist <- NULL
  for (t in seq(burnin)) {
    a.t    <- sample(seq(Na), 1L)
    r.true <- env$Y[t,a.t]
    r.hist <- c(r.hist, r.true)
    a.hist <- unname(rbind(a.hist, a.t))
  }
  # Learning loop
  regret <- rep(.0, N-burnin)
  ps     <- rep(.0, N-burnin)
  Qvals  <- matrix(.0, nrow = N-burnin, ncol = Na)
  for (t in seq(t+1L,N)) {
    x.t  <- env$X[t,,drop=FALSE]
    r.t  <- beta.hat + c(x.t %*% gamma.hat)
    eps  <- 1./sqrt(t-burnin)
    u    <- as.numeric(runif(1L) > eps)
    a.t  <- which.max(r.t) * u + sample(seq(Na), 1L) * (1.-u)
    r.true <- beta + c(x.t %*% gamma)
    r.hist <- c(r.hist, r.true[a.t])
    a.hist <- unname(rbind(a.hist, a.t))
    H      <- unname(model.matrix(~-1.+factor(a.hist)+env$X[1L:t,,drop=FALSE]))
    ## Update parameters
    param  <- solve(t(H)%*%H) %*% t(H) %*% r.hist
    beta.hat  <- param[1L:2L,]
    gamma.hat <- param[3L:4L,]
    ## Propensity model
    pmodel <- glm(A ~ ., family = binomial, data = data.frame(A=c(0L,1L)[a.hist], X1=env$X[1L:t,1L], X2=env$X[1L:t,2L]))
    ps[t-burnin] <- 1. - unname(predict(pmodel, data.frame(X1 = x.t[,1], X2 = x.t[,2]), type="response"))
    ## Outcome model
    qmodel <- glm(Y ~ A * (X1 + X2), family = gaussian, data = data.frame(Y=r.hist, A=c(0L,1L)[a.hist], X1=env$X[1L:t,1L], X2=env$X[1L:t,2L]))
    Qvals[t-burnin, 1L] <- unname(predict(qmodel, data.frame(A = 0L, X1 = x.t[,1], X2 = x.t[,2])))
    Qvals[t-burnin, 2L] <- unname(predict(qmodel, data.frame(A = 1L, X1 = x.t[,1], X2 = x.t[,2])))
    ## Regret computation
    regret[t-burnin] <- max(r.true)-r.true[a.t]
  }
  ## Marginal estimates
  arm <- 1L
  beta[arm]
  ida <- (a.hist[(burnin+1L):N]==arm)
  sum(ida * r.hist[(burnin+1L):N]) / sum(ida)
  mean((ida * r.hist[(burnin+1L):N] / ps))
  mean(Qvals[,arm]) + mean((ida / ps) * (r.hist[(burnin+1L):N]-Qvals[,arm]))
  # Return values
  cumsum(regret)
}

df <- data.frame(Y=r.hist, A=a.hist, X1=env$X[,1L], X2=env$X[,2L])

regret <- rep(.0, env$N-(env$N%/%10L))
N <- 10L
for (i in seq(N)) {
  resust <- run_experiment(env)
  regret <- regret + resust
}
regret <- regret / N

# Multi-threaded Monte Carlo simulations
library(foreach)
library(doParallel)

# setup parallel backend to use many processors
cores <- detectCores()
cl    <- makeCluster(cores[1L]-4L) # not to overload your computer
registerDoParallel(cl)

N <- 10

joint.res <- foreach (j=seq(N), .combine='c', .multicombine=FALSE) %dopar% {
  thread.res <- run_experiment(env) # / N # calling a function
  thread.res # Equivalent to joint.res = cbind(joint.res, thread.res)
}
# stop cluster
stopCluster(cl)

plot(joint.res, type='l')
