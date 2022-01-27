# Performs the integral on f(x) on x in [a, b]
# You will need to define another function and pass it as a parameter.
# Example: Let g <- function(x) { return(x * x) }
# Calling integrate(g, 0, 10, 65536) returns 333.33333...
integrate <- function(f, a, b, n2) {
    n <- n2 * 2
    if (a > b) return(-1 * integrate(b, a))
    h <- (b - a + 1.0 - 1.0) / n
    res <- f(a) + f(b)
    for (i in 1 : n - 1) {
        x <- a + h * i
        
        coefficient <- 2
        if (i %% 2 == 1) coefficient <- 4
        
        area <- f(x) * coefficient
        res <- res + area
    }
    return(res * h / 3)
}

# Performs the integral on f(x, y) on x in [a, b] and fixed y
integrate2 <- function(f, a, b, n2, y) {
    n <- n2 * 2
    if (a > b) return(-1 * integrate(b, a))
    h <- (b - a + 1.0 - 1.0) / n
    res <- f(a, y) + f(b, y)
    for (i in 1 : n - 1) {
        x <- a + h * i
        
        coefficient <- 2
        if (i %% 2 == 1) coefficient <- 4
        
        area <- f(x) * coefficient
        res <- res + area
    }
    return(res * h / 3)
}
