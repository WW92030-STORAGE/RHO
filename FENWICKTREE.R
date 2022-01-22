# Array fill method
memset <- function(n, x) {
    if (n <= 0) return(x)
    arr <- c(x)
    for (i in 2 : n) arr <- append(arr, x)
    return(arr)
}

# Sum of the first x values (indices 0 ... x)
psum <- function(tree, x) {
    if (x == 0) return(x)
    sum <- 0
    while (x > 0) {
        sum <- sum + tree[x]
        x <- x - bitwAnd(x, -1 * x)
    }
    return(sum)
}

# Return the sum of indices a ... b
query <- function(tree, a, b) {
    return(psum(tree, b) - psum(tree, a - 1))
}

# Add x to index k
change <- function(tree, k, x) {
    while (k <= length(tree)) {
        tree[k] <- tree[k] + x
        k <- k + bitwAnd(k, -1 * k)
    }
    return(tree)
}


# Change the value at k to x
update <- function(tree, k, x) {
    prev <- tree[k]
    return(change(tree, k, x - prev))
}

# Create Fenwick array from input (arr[n])
fenwick <- function(arr) {
    n <- length(arr)
    tree <- memset(n, 0)
    for (i in 1 : n) tree <- change(tree, i, arr[i])
    return(tree)
}
