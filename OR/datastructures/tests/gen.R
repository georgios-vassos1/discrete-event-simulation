
## Some tests
refarg <- function(node, ...) {
  x <- node
  x$key <- 22L
}

refarg_test <- function(...) {
  key0 <- 10L
  node <- Node(key0)
  refarg(node)
  key1 <- node$key
  key0 != key1
}
