rm(list=ls())

source("~/solutions/OR/utils.R")
source("~/solutions/OR/elem.R")
source("~/solutions/OR/ffa.R")

# List of nodes
nodes <- c("0", "1", "2", "3", "4", "5")
# List of edges to form the graph
edges <- list(
  Edge("0","1",16L),
  Edge("0","2",13L),
  Edge("1","2",10L),
  Edge("1","3",12L),
  Edge("2","1", 4L),
  Edge("2","4",14L),
  Edge("3","2", 9L),
  Edge("3","5",20L),
  Edge("4","3", 7L),
  Edge("4","5", 4L)
)
# Define Graph object
g <- Graph(nodes)
# Initialize graph
init_graph(g, edges, Nv = 6L)
# Depth-first search
dfs(g, "0")
# Depth-first search
parent <- new.env(hash = FALSE, parent = emptyenv())
bfs(g, "0", "5", parent)
# Ford-Fulkerson Algorithm
FFA(g, "0", "5")
# Print graph
print_graph(g)



