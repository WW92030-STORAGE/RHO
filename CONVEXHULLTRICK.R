# Intersection of lines (y = a1 * x + b1) and (y = a2 * x + b2)
intersect <- function(a1, b1, a2, b2) {
    if (a1 == a2) return(0)
    res <- (b2 - b1) / (a1 - a2)
    return(res)
}

# Intersection of lines represented by rows r1 and r2 in line matrix mat
intersection <- function(mat, r1, r2) {
    return(intersect(mat[r1, 1], mat[r1, 2], mat[r2, 1], mat[r2, 2]))
}

# Evaluates b1 * x + b2
eval <- function(b1, b2, x) {
    return(b1 * x + b2)
}

# Generate list of lines that contain extremal values
# Any line that is always below at least one other line is excluded
# The row [b1, b2] represents the line y = b1 * x + b2
hull <- function(input) {
    
    lines <- input[order(input[, 1], input[, 2]), ]
    n <- length(lines) / 2
    
    res <- matrix(lines[n, ], nrow = 1)
    for (i in (n - 1) : 1) {
        while (length(res) > 2 * 1) {
            n2 <- length(res) / 2
            i1 <- intersect(lines[i, 1], lines[i, 2], res[n2, 1], res[n2, 2])
            i2 <- intersection(res, n2, n2 - 1)
            if (i1 > i2) res <- res[-n2, ]
            else break
        }
        res <- rbind(res, lines[i, ])
    }
    res <- res[order(res[, 1], res[, 2]), ]
    return(res)
}

# Given the convex hull lines ch of the linear functions
# Determines the maximum function output for the given x value
# The list of lines must be sorted ascending by slope and y-intercept
solve <- function(ch, x) {
    n <- length(ch) / 2
    if (x <= intersection(ch, 1, 2)) return(1)
    if (x >= intersection(ch, n - 1, n)) return(n)
    low <- 2
    high <- n - 1
    while (low + 10 < high) {
        mid <- floor((low + high) * 0.5)
        left <- intersection(ch, mid, mid - 1)
        right <- intersection(ch, mid, mid + 1)
        deficit <- (x < left)
        excess <- (x > right)
        if (!(deficit | excess)) return(mid)
        if (deficit) high <- mid
        else low <- mid
    }
    lb <- max(c(2, low - 10))
    ub <- min(c(n - 1, high + 10))
    for (i in lb : ub) {
        left <- intersection(ch, i, i - 1)
        right <- intersection(ch, i, i + 1)
        if (x >= left & x <= right) return(i)
    }
    return(-1)
}

# Example - https://codeforces.com/blog/entry/63823

vec <- c(-2, 1, -1, 3, 0, 2, 1, -2)
# vec <- append(vec, c(-6, 12))
lines <- matrix(vec, nrow = length(vec) / 2, byrow = TRUE)
ch <- hull(lines)

for (i in -10 : 10) {
    num <- i * 0.5
    res <- solve(ch, num)
    cat(num, " ", res, eval(ch[res, 1], ch[res, 2], num), "\n")
}
