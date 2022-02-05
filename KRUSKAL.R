# Kruskal's algorithm - Make sure the edge list is formatted correctly (with leading 0s if necessary)
kruskal <- function(graphedges) {
    edges <- graphedges[order(graphedges[, 3], graphedges[, 1], graphedges[, 2]), ]
    vertices <- new.env(hash = TRUE)
    for (i in 1 : (length(edges) / 3)) { # Collect the vertices
        v1 <- edges[i, 1]
        v2 <- edges[i, 2]
        vertices[[v1]] <- TRUE
        vertices[[v2]] <- TRUE
    }
    nodes <- unlist(ls(vertices))
    
    res <- c()
    bigdsu <- dsu(nodes)
    for (i in 1 : (length(edges) / 3)) {
        v1 <- edges[i, 1]
        v2 <- edges[i, 2]
        if (!bigdsu$same(v1, v2)) {
            bigdsu$unite(v1, v2)
            res <- append(res, c(v1, v2))
            
        }
    }
    return(matrix(res, nrow = length(res) / 2, byrow = TRUE))
}

# Example
list <- c(1, 2, 3, 2, 3, 5, 2, 4, 2, 3, 4, 8, 5, 1, 7, 5, 4, 4)
graph <- c()
for (i in 1 : length(list)) graph <- append(graph, toString(list[i]))
edges <- matrix(graph, nrow = length(graph) / 3, byrow = TRUE)

kruskal(edges) # (2, 4, 2) (1, 2, 3) (5, 4, 4) (2, 3, 5)

