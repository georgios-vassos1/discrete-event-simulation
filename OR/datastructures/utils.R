
getref <- function(x)  {
  data.table::address(x)
}

Counter <- function(...) {
  args    <- list(...)
  counter <- new.env(hash = FALSE, parent = emptyenv())
  counter$count  <- ifelse(is.null(args[["count"]]), 0L, args[["count"]])
  class(counter) <- "Counter"
  return(counter)
}
