# Clear environment
rm(list=ls())

Node <- function(value=NULL, weight=NULL, ...) {
  node <- new.env(hash = FALSE, parent = emptyenv())
  node$value  <- value
  node$weight <- weight
  class(node) <- "Node"
  return(node)
}

Edge <- function(s=NULL, t=NULL, weight=NULL, ...) {
  edge <- new.env(hash = FALSE, parent = emptyenv())
  edge$s <- s
  edge$t <- t
  edge$weight <- weight
  class(edge) <- "Edge"
  return(edge)
}

Graph <- function(nodes, ...) {
  graph <- new.env(hash = FALSE, parent = emptyenv())
  graph$nodes    <- nodes
  graph$adj_list <- list()
  class(graph)   <- "Graph"
  return(graph)
}

init_graph <- function(g, edges, ...) {
  args <- list(...)
  g$visited <- NULL
  for (e in edges) {
    # g$adj_list[[e$s]] <- c(g$adj_list[[e$s]], Node(e$t, e$weight))
    g$adj_list[[e$s]][[e$t]] <- Node(e$t, e$weight)
  }
  g$Nv <- args[["Nv"]]
}

copy_graph <- function(g) {
  gg <- Graph()
  gg$nodes    <- g$nodes
  gg$adj_list <- g$adj_list
  gg$Nv       <- g$Nv
  gg$visited  <- NULL
  gg
}

print_graph <- function(g) {
  n <- 1L
  while (n <= length(g$adj_list)) {
    for (node in g$adj_list[[n]]) {
      print(paste0("Vertex ", names(g$adj_list[n]), " ==> ", node$value, " (", node$weight, ")"))
    }
    n <- n + 1L
  }
}

## Auxiliary queue data structure
Queue <- function(...) {
  queue        <- new.env(hash = FALSE, parent = emptyenv())
  queue$X      <- NULL
  class(queue) <- "Queue"
  return(queue)
}

insert <- function(q, x, ...) {
  q$X <- c(q$X, x)
}

pop <- function(q) {
  x <- q$X[1L]
  if ((n <- length(q$X)) == 1L) {
    q$X <- NULL
  } else {
    q$X <- q$X[2:n]
  }
  return(x)
}

peek <- function(q) {
  q$X[1L]
}

empty <- function(q) {
  is.null(q$X)
}

