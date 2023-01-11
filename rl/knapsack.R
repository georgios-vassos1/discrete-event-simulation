rm(list=ls())
setwd("~/solutions/rl")

# 0-1 Knapsack Problem using recursion
# www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/

# Time Complexity: O(2N)
# Auxiliary Space: O(N), Stack space required for recursion

knapsack <- function(W, wt, val, n) {
  if ((n == 0) || (W == 0)) {
    return(0L)
  }
  if (wt[n] > W) {
    knapsack(W, wt, val, n-1L)
  } else {
    return(
      max(val[n] + knapsack(W-wt[n], wt, val, n-1L), knapsack(W, wt, val, n-1L))
    )
  }
}

# 3-items instance
val <- c(60, 100, 120) # value of each item
wt  <- c(10, 20, 30)   # weight of each item
W   <- 50              # knapsack weight capacity
n   <- length(val)     # number of items
print(knapsack(W, wt, val, n))

# 0-1 Knapsack Problem using memorization

# Time Complexity: O(N * W)
# Auxiliary Space: O(N * W) + O(N)

x <- matrix(-1L, nrow = n, ncol = W)

knapsack <- function(W, wt, val, n) {
  if ((n == 0L) || (W == 0L)) {
    return(0L)
  }
  if (x[n,W] != -1) {
    return(x[n,W])
  }
  if (wt[n] <= W) {
    x[n,W] <<- max(val[n] + knapsack(W-wt[n], wt, val, n-1L), knapsack(W, wt, val, n-1L))
    return(x[n,W])
  }
  else {
    x[n,W] <<- knapsack(W, wt, val, n-1L)
    return(x[n,W])
  }
}

print(knapsack(W, wt, val, n))

# 0-1 Knapsack Problem using dynamic programming

# Time Complexity: O(N * W). where ‘N’ is the number of elements and ‘W’ is capacity. 
# Auxiliary Space: O(N * W)

knapsack <- function(W, wt, val, n) {
  x <- matrix(0L, nrow = n+1L, ncol = W+1L)

  for (i in seq(n+1L)) {
    for (w in seq(W+1L)) {
      if ((i == 1L) || (w == 1L)) {
        x[i,w] <- 0L
      } else if (wt[i-1L] <= w) {
        x[i,w] <- max(val[i-1L] + x[i-1L,w-wt[i-1L]], x[i-1L,w])
      } else {
        x[i,w] <- x[i-1L,w]
      }
    }
  }
  print(x)
  x[n+1L,W+1L]
}

print(knapsack(W, wt, val, n))

# We know we are always using the  current row or
# the previous row of the array/vector . Thereby we can
# improve it further by using a 2D array but with only
# 2 rows i%2 will be giving the index inside the bounds
# of 2d array K

# Time Complexity: O(N * W)
# Auxiliary Space: O(2 * W)

knapsack <- function(W, wt, val, n) {
  x <- matrix(0L, nrow = 2L, ncol = W+1L)

  for (i in seq(n+1L)) {
    idx <- (i%%2L)+1L
    jdx <- ((i-1L)%%2L)+1L
    for (w in seq(W+1L)) {
      if ((i == 1L) || (w == 1L)) {
        x[idx,w] <- 0L
      } else if (wt[i-1L] <= w) {
        x[idx,w] <- max(val[i-1L] + x[jdx,w-wt[i-1L]], x[jdx,w])
      } else {
        x[idx,w] <- x[jdx,w]
      }
    }
  }
  idx <- ((n+1L)%%2L)+1L
  x[idx,W+1L]
}

print(knapsack(W, wt, val, n))

# 0-1 Knapsack Problem using dynamic programming(Space optimized)

# Time Complexity: O(N * W). As redundant calculations of states are avoided
# Auxiliary Space: O(W)

knapsack <- function(W, wt, val, n) {
  x <- vector(mode = "integer", length = W+1L)

  for (i in seq(n)) {
    for (w in seq(W+1L, 1L)) {
      if (wt[i] <= w) {
        x[w] <- max(val[i]+x[w-wt[i]], x[w])
      }
    }
  }
  x[W+1L]
}

print(knapsack(W, wt, val, n))

