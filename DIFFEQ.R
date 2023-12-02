deriv <- function(func, x) {
    H = 0.000001
    return( (func(x + H) - func(x)) / H )
}

newton <- function(func, x0) {
    R = 16
    for (i in 1 : R) {
        d = deriv(func, x0)
        if (d == 0) d = d + 0.000001
        x0 = x0 - func(x0) / d
    }
    return(x0)
}

euler <- function(deriv, x, y, h) { # deriv(x, y) takes in two values
    return(y + deriv(x, y) * h)
}

midpt <- function(deriv, x, y, h) {
    h2 = 0.5 * h
    return(y + h * deriv(x + h2, y + h2 * deriv(x, y)))
}

trap <- function(deriv, x, y, h) {
    h2 = 0.5 * h
    return(y + h2 * (deriv(x, y) + deriv(x + h, y + h * deriv(x, y))))
}

rk4 <- function(deriv, x, y, h) {
    h6 = h / 6.0
    h2 = h / 2.0
    s1 = deriv(x, y)
    s2 = deriv(x + h2, y + h2 * s1)
    s3 = deriv(x + h2, y + h2 * s2)
    s4 = deriv(x + h, y + h * s3)
    return(y + h6 * (s1 + s2 + s2 + s3 + s3 + s4))
}

reveuler <- function(deriv, x, y, h) {
    # Y = y + h * F(x + h, Y) => 0 = y + h * F(x + h, Y) - Y
    piece <- function(n) {
        return(y + h * deriv(x + h, n) - n)
    }
    return(newton(piece, y))
}

solve <- function(method, deriv, x, y, h, steps) {
    dx = c(x)
    dy = c(y)
    for (i in 1 : steps) {
        y = method(deriv, x, y, h)
        x = x + h
        dx = append(dx, x)
        dy = append(dy, y)
    }
    return(dy)
}

# FUNCTIONS

func <- function(x, y) {
    return(2 * x)
}

H = 0.01
S = floor(4 / H)

list = solve(euler, func, 0, 0, H, S)
print(list[S])

list = solve(midpt, func, 0, 0, H, S)
print(list[S])

list = solve(trap, func, 0, 0, H, S)
print(list[S])

list = solve(rk4, func, 0, 0, H, S)
print(list[S])

list = solve(reveuler, func, 0, 0, H, S)
print(list[S])
