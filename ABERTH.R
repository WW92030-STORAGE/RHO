# In this paper, complex numbers are represented as 2-element vectors

realcomp <- function(n) {
    return(c(n, 0));
}

add <- function(a, b) {
    return(c(a[1] + b[1], a[2] + b[2]))
}

subtract <- function(a, b) {
    return(c(a[1] - b[1], a[2] - b[2]))
}

times <- function(a, b) {
    return(c(a[1] * b[1] - a[2] * b[2], a[1] * b[2] + a[2] * b[1]))
}

inv <- function(a) {
    if (a[1] == 0 & a[2] == 0) return(a)
    rsq = a[1] * a[1] + a[2] * a[2]
    return(c(a[1] * 1.0 / rsq, -1.0 * a[2] / rsq))
}

divide <- function(a, b) {
    if (b[1] == 0 & b[2] == 0) return(a)
    return(times(a, inv(b)))
}

# Polynomials are represented as a 2-wide array of complex numbers
# This function converts a real polynomial into a complex one.
comppoly <- function(arr) {
    res <- c()
    for (i in 1 : length(arr)) res <- append(res, arr[i])
    for (i in 1 : length(arr)) res <- append(res, 0)
    return(matrix(res, byrow = FALSE, nrow = length(arr)))
}


polyeval <- function(arr, x) {
    n = length(arr) / 2
    res <- realcomp(arr[1, ])
    for (i in 2 : n) {
        res <- times(res, x)
        res <- add(res, realcomp(arr[i, ]))
    }
    return(res)
}

# Polynomial derivative approximation
deriv <- function(arr, x) {
    ep <- 0.0000001
    dx <- c(ep, ep)
    y = add(x, dx)
    fx = polyeval(arr, x)
    fy = polyeval(arr, y)
    dy = subtract(fy, fx)
    res = divide(dy, dx)
    if (res[1] == 0 & res[2] == 0) res = add(res, dx)
    return(res)
}

# Aberth method
solve <- function(arr) {
    n = (length(arr) / 2) - 1
    res = matrix(rep(0, 2 * n), byrow = FALSE, nrow = n)
    offset = matrix(rep(0, 2 * n), byrow = FALSE, nrow = n)
    one = realcomp(1)
    initx = -1.0 * arr[2, 1] / n
    inity = -1.0 * arr[2, 1] / n
    for (i in 1 : n) {
        res[i, 1] = runif(1, initx - 1, initx + 1)
        res[i, 2] = runif(1, inity - 1, inity + 1)
    }
    
    
    for (sksksk in 1 : 1024) {
        for (i in 1 : n) {
            sum = realcomp(0)
            for (j in 1 : n) {
                if (j == i) next;
                sum = add(sum, inv(subtract(res[i, ], res[j, ])))
            }
            val = polyeval(arr, res[i, ])
            dev = deriv(arr, res[i, ])
            quo = divide(val, dev)
            denom = subtract(one, times(quo, sum))
            offset[i, ] = divide(quo, denom)
        }
        for (i in 1 : n) res[i, ] = subtract(res[i, ], offset[i, ])
    }
    
    return(matrix(res, byrow = FALSE, nrow = n))
}

# Example

poly <- comppoly(c(29, 20, -17, 89)) # 29x^3 + 20x^2 - 17x + 89 = 0
res <- solve(poly) # 3 complex roots

polyeval(poly, res[1, ])
polyeval(poly, res[2, ])
polyeval(poly, res[3, ]) # All should be 0 or close to 0
