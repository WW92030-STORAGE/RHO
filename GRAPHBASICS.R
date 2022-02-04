# Adjacency list (using hashmap)
# g[[a]][[b]] = c represents the edge from a to b of weight c.
graph <- function() {
    return(new.env(hash = TRUE))
}

# Does environment g contain variable h?
contains <- function(g, h) {
    return(!is.null(g[[h]]))
}

# Adds edge from a to b onto graph g
edge <- function(g, a2, b2) {
    a <- toString(a2)
    b <- toString(b2)
    if (!contains(g, a)) g[[a]] <- new.env(hash = TRUE)
    if (!contains(g, b)) g[[b]] <- new.env(hash = TRUE)
    g[[a]][[b]] <- 1
}

# Adds edge from a to b with weight c onto graph g
wedge <- function(g, a2, b2, c) {
    a <- toString(a2)
    b <- toString(b2)
    if (!contains(g, a)) g[[a]] <- new.env(hash = TRUE)
    if (!contains(g, b)) g[[b]] <- new.env(hash = TRUE)
    g[[a]][[b]] <- c
}

# Adds edge between a and b in both directions
edge2 <- function(g, a2, b2) {
    edge(g, a2, b2)
    edge(g, b2, a2)
}

# Adds edge (a, b) with weight c in both directions
wedge2 <- function(g, a2, b2, c) {
    wedge(g, a2, b2, c)
    wedge(g, b2, a2, c)
}

# Create a list of edges (includes all edges)
# Edge weights are represented as string
# To get an integer use strtoi()
edgelist <- function(g) {
    keys <- unlist(ls(g))
    edges = c()
    
    maxlength <- 1
    for (i in 1 : length(keys)) {
        vertex <- g[[keys[i]]]
        adj <- unlist(ls(vertex))
        for (j in 1 : length(adj)) {
            weight <- toString(vertex[[adj[[j]]]])
            maxlength <- max(c(maxlength, nchar(weight)))
        }
    }
    
    for (i in 1 : length(keys)) {
        vertex <- g[[keys[i]]]
        adj <- unlist(ls(vertex))
        for (j in 1 : length(adj)) {
            weight <- toString(vertex[[adj[j]]])
            weight <- pad(weight, maxlength, "0")
            vec <- c(keys[i], adj[j], weight)
            edges <- append(edges, vec)
        }
    }
    res <- matrix(edges, nrow = length(edges) / 3, byrow = TRUE)
    return(res[order(res[, 3], res[, 1], res[, 2]), ])
}

# Example
network <- graph()

edge2(network, 0, 1)
edge2(network, 1, 2)
edge2(network, 4, 5)
edge2(network, 6, 5)
edge2(network, 8, 3)
edge2(network, 2, 3)

n1 <- network[["2"]] # Adjacency list for vertex "2"
ls(n1) # "1" "3"

edgelist(network) # (0, 1, 1), (1, 0, 1), (1, 2, 1), (2, 1, 1), etc.

