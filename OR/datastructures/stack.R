
Stack <- function(...) {
  stack <- new.env(hash = FALSE, parent = emptyenv())
  stack$items  <- NULL
  stack$len    <- 0L
  class(stack) <- "Stack"
  return(stack)
}

push <- function(stack, node) {
  stack$len <- stack$len + 1L
  stack$items[[stack$len]] <- node
}

pop <- function(stack) {
  x <- stack$items[[stack$len]]
  stack$items[[stack$len]] <- NULL
  stack$len <- stack$len - 1L
  x
}
