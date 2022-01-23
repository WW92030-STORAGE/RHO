# Generalized Fenwick Tree that operates on any abelian group and its binary operation

# Binary operation
combine <- function(a, b) {
    return(a + b)
}

# Inverse
inv <- function(a) {
    return(-1 * a)
}

# Subtraction (aka. addition of the inverse)
subtract <- function(a, b) {
    return(combine(a, inv(b)))
}

# Sum of the first x values (indices 0 ... x)
psum <- function(tree, x) {
    if (x == 0) return(x)
    sum <- 0
    while (x > 0) {
        sum <- combine(sum, tree[x])
        x <- x - bitwAnd(x, -1 * x)
    }
    return(sum)
}

# Return the sum of indices a ... b
query <- function(tree, a, b) {
    return(subtract(psum(tree, b), psum(tree, a - 1)))
}

# Add x to index k
change <- function(tree, k, x) {
    while (k <= length(tree)) {
        tree[k] <- combine(tree[k], x)
        k <- k + bitwAnd(k, -1 * k)
    }
    return(tree)
}

# Change the value at k to x
update <- function(tree, k, x) {
    prev <- tree[k]
    return(change(tree, k, subtract(x, prev)))
}

# Create Fenwick array from input (arr[n])
fenwick <- function(arr) {
    n <- length(arr)
    tree <- memset(n, 0)
    for (i in 1 : n) tree <- change(tree, i, arr[i])
    return(tree)
}

