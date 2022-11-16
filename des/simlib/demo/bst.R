rm(list=ls())
setwd("~/solutions/des")

Rcpp::sourceCpp("./simlib/src/bst.cpp")

bt <- new(BST)
X <- c()
for (i in seq(1e2L)) {
  x <- runif(1, 0.1, 365.0)
  bt$insert(x, i)
  if (runif(1) > 0.5) {
    X <- c(X, x)
  }
}
bt$printTree()
bt$search(X[65])

