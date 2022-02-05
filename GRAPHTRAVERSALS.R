# Breadth first search for connected components
bfs <- function(g, src) {
    vis <- new.env(hash = TRUE)
    queue <- c(toString(src))
    while (length(queue) > 0) {
        now <- queue[1]
        queue <- queue[-1]
        if (contains(vis, now)) next
        vis[[now]] <- TRUE
        if (!contains(g, now)) next
        adj <- unlist(ls(g[[now]]))
        if (length(adj) <= 0) next
        for (i in 1 : length(adj)) {
            xp <- toString(adj[i])
            if (contains(vis, xp)) next
            queue <- append(queue, xp)
            vis[[next]] <- TRUE
        }
    }
    
    return(unlist(ls(vis)))
}

# Dijkstra's algorithm on graph g (O(n^2))
dijkstra <- function(g, src) {
    vertices <- unlist(ls(g))
    dist <- new.env(hash = TRUE)
    done <- new.env(hash = TRUE)
    n <- length(vertices)
    
    for (i in 1 : n) dist[[vertices[i] ]] = 2147483647
    dist[[toString(src)]] <- 0
    for (count in 0 : n - 2) {
        md <- 2147483647
        mi <- 1
        for (i in 1 : n) { # Minimum distance in unprocessed vertices
            if (!contains(done, vertices[i])) {
                if (dist[[vertices[i]]] <= md) {
                    md <- dist[[vertices[i]]]
                    mi <- i
                }
            }
        }
        u <- vertices[mi]
        done[[u]] <- TRUE
        if (!contains(g, u)) next
        if (length(g[[u]]) <= 0) next
        adj <- unlist(ls(g[[u]]))
        for (i in 1 : length(adj)) {
            v <- adj[i]
            if (contains(done, v)) next
            if (dist[[u]] == 2147483647) next
            original <- dist[[v]] + 0
            candidate <- dist[[u]] + g[[u]][[v]]
            dist[[v]] <- min(c(original, candidate))
        }
    }
    return(dist)
}

# Utility function for topological sorting
tsutil <- function(g, start, vis, src, res) {
    start[[src]] <- TRUE
    vis[[src]] <- TRUE
    if (length(ls(g[[src]])) > 0) {
        adj <- unlist(ls(g[[src]]))
        for (i in 1 : length(adj)) {
            v <- adj[i]
            if (!contains(vis, v)) {
                list <- tsutil(g, start, vis, v, res)
                res <- append(list, res)
            }
        }
    }
    res <- append(src, res)
    rm(list = c(src), envir = start)
    return(res)
}

# Topological sort a directed acyclic graph
topsort <- function(g) {
    vertices <- unlist(ls(g))
    n <- length(vertices)
    started <- new.env(hash = TRUE)
    vis <- new.env(hash = TRUE)
    res <- c()
    
    for (i in 1 : n) {
        if (!contains(vis, vertices[i])) {
            sub <- tsutil(g, started, vis, vertices[i], c())
            res <- append(sub, res)
        }
    }
    return(res)
}

# EXAMPLE - BFS

network <- graph()

edge(network, 0, 1)
edge(network, 1, 2)
edge(network, 4, 5)
edge(network, 6, 5)
edge(network, 8, 3)
edge(network, 2, 3)

bfs(network, 4) # "4" "5"

# EXAMPLE - DIJKSTRA

network <- graph()
wedge(network, 1, 2, 6)
wedge(network, 1, 3, 2)
wedge(network, 3, 2, 3)

res <- dijkstra(network, 1)
res[["2"]] # 5
res[["3"]] # 2

# EXAMPLE - TOPOLOGICAL SORT

network <- graph()
edge(network, 1, 2)
edge(network, 3, 1)
edge(network, 4, 5)

topsort(network) # "4" "5" "3" "1" "2"
