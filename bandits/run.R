
source("~/solutions/bandits/utils.R")
source("~/solutions/bandits/logging.R")
source("~/solutions/bandits/target.R")
source("~/solutions/bandits/banditsim.R")
source("~/solutions/bandits/OPEE.R")
source("~/solutions/bandits/OPEEsim.R")

# Multi-threaded Monte Carlo simulations
library(foreach)
library(doParallel)

# setup parallel backend to use many processors
cores <- detectCores()
cl    <- makeCluster(cores[1L]-4L) # not to overload your computer
registerDoParallel(cl)

i     <- 36L # Index 35L is problematic
world <- DataUnit(index[i]) # Get data
N     <- 10L # number of experiments
estimators <- c("dm", "ips", "dr", "adr", "mrdr", "cadr")
banditsim  <- BanditSimulator(NULL)
opeesim    <- OPEEsimulator(banditsim, rpart::rpart, rpart::rpart, 100L*100L)
# opee_simulator_test(opeesim, index[i], world, banditsim, estimators = estimators)

joint.res <- foreach (j=seq(N), .combine=rbind, .multicombine=FALSE, .errorhandling='pass') %dopar% {
  thread.res <- opee_simulator_test(opeesim, index[i], world, banditsim, estimators = estimators) # calling a function
  thread.res # Equivalent to joint.res = cbind(joint.res, thread.res)
}
# stop cluster
stopCluster(cl)

colMeans(joint.res)
