
dfs <- function(g, v, ...) {
  g$visited[[v]] <- TRUE
  print(v)
  for (node in g$adj_list[[v]]) {
    if (is.null(g$visited[[node$value]]) || !g$visited[[node$value]]) {
      dfs(g, node$value)
    }
  }
}

bfs <- function(g, s, t, parent=NULL) {
  visited <- list()
  queue   <- Queue()
  insert(queue, s)
  visited[[s]]      <- TRUE
  parent$keys[[s]]  <- -1L
  # Standard BFS loop
  while (!empty(queue)) {
    u <- pop(queue)
    for (node in g$adj_list[[u]]) {
      v <- node$value
      if ((is.null(visited[[v]]) || !visited[[v]]) && (g$adj_list[[u]][[v]]$weight > 0L)) {
        insert(queue, v)
        visited[[v]] <- TRUE
        parent$keys[[v]]  <- u
        if (v == t) return(TRUE)
      }
    }
  }
  return(FALSE)
}

