library(lpSolve)

# Set the parameters
n <- 3L  # number of options
v <- c(2., 3., 4.)  # pre-paid reservation costs
w <- c(5., 7., 6.)  # execution costs
x <- c(10L, 20L, 15L)  # maximum capacities
z <- 30L  # order size

# Objective function
obj <- w

# Constraint matrix
con <- t(matrix(c(diag(n), -diag(n), rep(1., n)), nrow = n))
dir <- c(rep("<=", 2*n), "=")
rhs <- c(x, rep(0., n), z)

# Solve the linear program
lp <- lp(direction = "min", objective.in = obj, const.mat = con, const.dir = dir, const.rhs = rhs)

q.alloc <- lp$solution

sum(v*x)+sum(w*q.alloc)

## 
n <- 5L
x <- c(10L, 20L, 15L,  8L, 12L)
v <- c( 5., 10.,  8.,  2.,  7.)
w <- c(20., 15., 10., 25., 18.)
q <- 50L

# Objective function
obj <- w

# Constraint matrix
con <- t(matrix(c(diag(n), -diag(n), rep(1., n)), nrow = n))
dir <- c(rep("<=", 2*n), "=")
rhs <- c(x, rep(0., n), q)

# Solve the linear program
lp <- lp(direction = "min", objective.in = obj, const.mat = con, const.dir = dir, const.rhs = rhs)

q.alloc <- lp$solution

sum(v*x)+sum(w*q.alloc)

