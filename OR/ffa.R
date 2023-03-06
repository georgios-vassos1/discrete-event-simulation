
FFA <- function(g, s, t) {
  parent      <- new.env(hash = FALSE, parent = emptyenv())
  parent$keys <- as.list(rep(-1., g$Nv))
  names(parent$keys) <- g$nodes
  max_flow <- 0.0
  while (bfs(g, s, t, parent)) {
    path_flow <- Inf
    x <- t
    while (x != s) {
      path_flow <- min(path_flow, g$adj_list[[parent$keys[[x]]]][[x]]$weight)
      x <- parent$keys[[x]]
    }
    max_flow <- max_flow + path_flow
    v <- t
    while (v != s) {
      u <- parent$keys[[v]]
      g$adj_list[[u]][[v]]$weight <- g$adj_list[[u]][[v]]$weight - path_flow
      if (is.null(g$adj_list[[v]][[u]])) {
        g$adj_list[[v]][[u]] <- Node(u, 0.0)
      }
      g$adj_list[[v]][[u]]$weight <- g$adj_list[[v]][[u]]$weight + path_flow
      v <- parent$keys[[v]]
    }
  }
  max_flow
}


