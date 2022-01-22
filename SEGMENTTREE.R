# Smallest power of 2 >= n
ceillog <- function(n) {
    count = 0
    pow = 1
    while (pow * 2 <= n) {
        pow = pow * 2
        count = count + 1
    }
    
    if (pow == n) return(pow)
    return(pow * 2)
}

# Array fill method (also serves as an initializer)
memset <- function(n, x) {
    if (n <= 0) return(x)
    arr <- c(x)
    for (i in 2 : n) arr <- append(arr, x)
    return(arr)
}

# Combine function
combine <- function(a, b) {
    return(a + b);
}


# Generate segment tree array from input vector (ray)
segtree <- function(ray) {
    n <- length(ray)
    size <- ceillog(n)
    arr <- memset(size, 0)
    tree <- memset(2 * size, 0)
    for (i in 1 : size) arr[i] <- ray[i]
    for (i in (size) : (2 * size - 1)) tree[i] <- arr[i - size + 1];
    
    for (ii in 1 : (size - 1)) {
        i <- size - ii
        tree[i] = combine(tree[2 * i], tree[2 * i + 1])
    }
    return(tree)
}

# Range query from indices a through b
query <- function(tree, start, end) {
    n <- floor(length(tree) / 2.0)
    a <- start + n # Add n to reach the corresponding tree node
    b <- end + n
    
    res <- 0 # initial value
    
    while (a <= b) { # While the interval has positive size
        if ((a %% 2) == 1) { # If A points to a node of odd index
            res <- res + tree[a] # Add its value to the sum
            a <- a + 1 # Increment the node pointer index.
		 # This moves the pointer 1 node to the right
		 # wrapping around to the next layer if necessary.
        } 
        if ((b %% 2) == 0) { # Same thing for B, except even index
            res <- res + tree[b]
            b <- b - 1
        }
        a <- a / 2 # Move each pointer up a layer
        b <- b / 2 # to correspond to the parent nodes
    }
    return(res)
}
# Add x to the element at index k
delta <- function(tree, k, x) {
    n <- floor(length(tree) / 2.0)
    i <- k + n
    
    tree[i] <- tree[i] + x
    i <- floor(i / 2.0)
    while (i >= 1) {
   # Move up the layers and update the node’s ancestors.
        tree[i] <- combine(tree[2 * i], tree[2 * i + 1])
        i <- floor(i / 2.0)
    }
    
    return(tree)
}
