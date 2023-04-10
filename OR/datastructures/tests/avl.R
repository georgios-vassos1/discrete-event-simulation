rm(list=ls())
source("~/solutions/OR/datastructures/avl.R")

# keys <- c(10L, 20L, 30L, 40L, 50L, 25L)
keys <- c(9L, 5L, 10L, 0L, 6L, 11L, -1L, 1L, 2L)
root <- NULL
for (key in keys) {
  root <- insert.avl(root, key)
}
pre.order(root)
root <- delete.avl(root, 10L)
pre.order(root)
