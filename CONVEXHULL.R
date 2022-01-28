# Orientation of the path between 3 points
orientation <- function(x1, y1, x2, y2, x3, y3) {
    value1 <- (y2 - y1) * (x3 - x2)
    value2 <- (y3 - y2) * (x2 - x1)
    res <- value1 - value2
    if (res == 0) return(0)
    if (res > 0) return(1)
    return(-1)
}

# Convex hull algorithm
convexhull <- function(points) {
    data <- points[order(points[, 1], points[, 2]),]
    n <- length(data[, 1])
    
    if (n <= 3) return(points)
    
    upper <- matrix(c(data[1, 1], data[1, 2], data[2, 1], data[2, 2]), byrow = TRUE, nrow = 2)
    for (i in 3 : n) {
        size <- length(upper) / 2
        
        while (size >= 2) {
            if (orientation(upper[size - 1, 1], upper[size - 1, 2], upper[size, 1], upper[size, 2], data[i, 1], data[i, 2]) >= 0) break
            upper <- upper[-size, ]
            size <- length(upper) / 2
        }
        upper <- rbind(upper, c(data[i, 1], data[i, 2]))
    }
    
    lower <- matrix(c(data[1, 1], data[1, 2], data[2, 1], data[2, 2]), byrow = TRUE, nrow = 2)
    for (i in 3 : n) {
        size <- length(lower) / 2
        while (size >= 2) {
            if (orientation(lower[size - 1, 1], lower[size - 1, 2], lower[size, 1], lower[size, 2], data[i, 1], data[i, 2]) <= 0) break
            lower <- lower[-size, ]
            size <- length(lower) / 2
        }
        lower <- rbind(lower, c(data[i, 1], data[i, 2]))
    }
    
    # Merge the lists together and remove duplicates
    
    merged <- rbind(upper, lower)
    res <- merged[order(merged[, 1], merged[, 2]), ]
    
    i <- 1
    while (i < length(res) / 2) {
        while (res[i, 1] == res[i + 1, 1] & res[i, 2] == res[i + 1, 2]) {
            res <- res[-(i + 1), ]
            if (i + 1 > length(res) / 2) break
        }
        
        i <- i + 1
    }
    
    return(res)
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

x <- c(2, 2, 3, 4, 4, 6)
y <- c(1, 5, 3, 3, 4, 3)

points <- pm(x, y)
convexhull(points)
