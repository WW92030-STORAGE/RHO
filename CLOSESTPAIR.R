# ALL METHODS EXCEPT solve() RETURN AN ARRAY OF 4 VALUES
# [X1 Y1 X2 Y2]
# NOTE - The length() of a matrix is how many elements there are. 
# If you want the number of rows then you must divide the total length by the number of columns.

# Squared distance between points
rsq <- function(x1, y1, x2, y2) {
    dx <- x2 - x1
    dy <- y2 - y1
    return(dx * dx + dy * dy)
}

# Squared distance between points, represented as a 4-tuple
dsq <- function(x) {
    return(rsq(x[1], x[2], x[3], x[4]))
}

# Clone a 4-tuple
clone <- function(x) {
    return(c(x[1], x[2], x[3], x[4]))
}

# Brute force the closest points on indices a to b
bruteforce <- function(points, a, b) {
    if (b < a) return(bruteforce(points, b, a))
    x1 <- points[a, 1]
    x2 <- points[b, 1]
    y1 <- points[a, 2]
    y2 <- points[b, 2]
    
    res <- rsq(x1, y1, x2, y2)
    
    for (i in a : b) {
        for (j in a : b) {
            if (i >= j) next
            cx1 <- points[i, 1]
            cx2 <- points[j, 1]
            cy1 <- points[i, 2]
            cy2 <- points[j, 2]
            
            dist <- rsq(cx1, cy1, cx2, cy2)
            if (dist < res) {
                res <- dist
                x1 <- cx1
                x2 <- cx2
                y1 <- cy1
                y2 <- cy2
            }
        }
    }
    
    return(c(x1, y1, x2, y2))
}

# Closest pair of points along a vertical strip of radius maxdist
solvestrip <- function(points, maxdist) {
    strip <- points[order(points[, 2], points[, 1]),]
    md <- maxdist
    n <- length(strip) / 2
    
    if (n < 10) return(bruteforce(strip, 1, n))
    
    res <- c(strip[1, 1], strip[1, 2], strip[2, 1], strip[2, 2])
    for (i in 1 : n) {
        for (j in i + 1 : n) {
            if (i >= j) next
            if (i >= n | j >= n) next
            dy <- abs(strip[j, 2] - strip[i, 2])
            if (dy * dy > md) break
            dist <- rsq(strip[j, 1], strip[j, 2], strip[i, 1], strip[i, 2])
            if (dist < md) {
                md <- dist
                res <- c(strip[i, 1], strip[i, 2], strip[j, 1], strip[j, 2])
            }
        }
    }
    
    return(res)
}

# Nearest points in range points[a] ... points[b]
closest <- function(points, a, b) {
    n <- b - a + 1
    if (n < 10) return(bruteforce(points, a, b))
    
    mid <- floor(0.5 * (a + b))
    midx <- points[mid, 1]
    midy <- points[mid, 2]
    
    res <- closest(points, a, mid)
    right <- closest(points, mid, b)
    md <- dsq(res)
    ld <- dsq(right)
    
    if (ld < md) {
        res <- clone(right)
        md <- ld
    }
    
    strip <- matrix(c(midx, midy), byrow = FALSE, nrow = 1)
    for (i in a : b) {
        if (i == mid) next
        dx <- abs(midx - points[i, 1])
        if (dx * dx < md) strip <- rbind(strip, c(points[i, 1], points[i, 2]))
    }
    
    if (length(strip) <= 2) return(res)
    
    sres <- solvestrip(strip, md)
    srsq <- dsq(sres)
    
    if (srsq < md) {
        md <- srsq
        res <- clone(sres)
    }
    
    return(res)
}

# Closest pair of points
solve <- function(points) {
    data <- points[order(points[, 1], points[, 2]),]
    n <- length(data[, 1])
    p2 <- closest(data, 1, n)
    
    # Indices of points
    i1 <- -1; # Last occurrence of point 1
    i2 <- -1; # First occurrence of point 2
    # This ensures i1 != i2 even if the 2 points are same
    for (i in 1 : n) {
        if (points[i, 1] == p2[1] & points[i, 2] == p2[2]) i1 <- i
        if (i2 >= 0) next
        if (points[i, 1] == p2[3] & points[i, 2] == p2[4]) i2 <- i
    }
    return(c(i1, i2))
}

# Generate a matrix of points from x and y arrays
pm <- function(x, y) {
    if (length(x) != length(y)) return(matrix())
    return(matrix(c(x, y), byrow = FALSE, nrow = length(x)))
}

# Example using random points
# You can convert a data frame to a matrix using as.matrix()
x <- floor(runif(500, 0, 4096))
y <- floor(runif(500, 0, 4096))

points <- pm(x, y)

pair <- solve(points) # Indices of the closest pair
points[pair[1],] # First point
points[pair[2],] # Second point
bruteforce(points, 1, length(points) / 2) # Check with brute-force
