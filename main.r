library(igraph)

## Importing the dependency: igraph
## It is a useful library for graph manipulation and network analysis.
## You can read more about it at: https://en.wikipedia.org/wiki/Igraph
## 
##     decompose, spectrum
## The following object is masked from 'package:base':
## 
##     union

graph <- list(s = c("a", "b"),
              a = c("s", "b", "c", "d"),
              b = c("s", "a", "c", "d"),
              c = c("a", "b", "d", "e", "f"),
              d = c("a", "b", "c", "e", "f"),
              e = c("c", "d", "f", "z"),
              f = c("c", "d", "e", "z"),
              z = c("e", "f"))

weights <- list(s = c(3, 5),
                a = c(3, 1, 10, 11),
                b = c(5, 3, 2, 3),
                c = c(10, 2, 3, 7, 12),
                d = c(15, 7, 2, 11, 2),
                e = c(7, 11, 3, 2),
                f = c(12, 2, 3, 2),
                z = c(2, 2))

# Create edgelist with the weighted graphs in a uniform list
G <- data.frame(stack(graph), weights = stack(weights)[[1]])

set.seed(500)
el <- as.matrix(stack(graph))
g <- graph_from_edgelist(el)

oldpar <- par(mar = c(1, 1, 1, 1))
plot(g, edge.label = stack(weights)[[1]])
par(oldpar)

path_length <- function(path) {
  # If path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # Get all consecutive nodes
  pairs <- cbind(values = path[-length(path)], ind = path[-1])
  # Join with G and sum over weights
  sum(merge(pairs, G)[ , "weights"])
}

find_shortest_path <- function(graph, start, end, path = c()) {
  # If there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # Add next node to path so far
  path <- c(path, start)
  
  # Base case of recursion: if end is reached return path
  if (start == end) return(path)
  
  # Initialize shortest path as NULL
  shortest <- NULL
  # Loop through all nodes linked from the current node (given in start)
  for (node in graph[[start]]) {
    # Proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # Recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- find_shortest_path(graph, node, end, path)
      # If newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(newpath) < path_length(shortest))
        shortest <- newpath
    }
  }
  # Finally, the shortest path between the given initial node and the neighboring node of the graph will be returned.
  shortest
}