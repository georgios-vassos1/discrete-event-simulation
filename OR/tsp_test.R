rm(list=ls())

source("~/solutions/OR/tsp.R")

# Travelling salesman problem
n <- 4L
memo <- matrix(-1L, nrow = n+1L, ncol = bitwShiftL(1,n+1L))

# dist[i][j] represents shortest distance to go from i to j
# this matrix can be calculated for any given graph using
# all-pair shortest path algorithms
dist <- matrix(c(
   0.0, 10.0, 15.0, 20.0,
  10.0,  0.0, 25.0, 25.0,
  15.0, 25.0,  0.0, 30.0,
  20.0, 25.0, 30.0,  0.0),
  nrow = n, ncol = n, byrow = TRUE)

ans <- Inf
for (i in seq(n)) {
  ans <- min(ans, dist[i,1L] + tsp(i, (bitwShiftL(1L,n+1L))-1L))
}
ans


