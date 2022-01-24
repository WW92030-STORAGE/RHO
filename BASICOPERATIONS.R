# BASIC OPERATIONS FOR VARIOUS PURPOSES

# Create an array of n values, each of which is a copy of x
memset <- function(n, x) {
  if (n <= 0) return(x)
  arr <- c(x)
  for (i in 2 : n) arr <- append(arr, x)
  return(arr)
}

# Evaluates b^e for integer exponents e
pow <- function(b, e) {
    if (e == 0) return(1)
    if (e < 0) return(1.0 / pow(b, -1 * e))
    if (e %% 2 == 1) return(b * pow(b, e - 1))
    half <- pow(b, e / 2)
    return(half * half)
}

# Evaluates b^e mod m for nonnegative exponents e
pow <- function(b, e, m) {
    if (e == 0) return(1)
    if (e %% 2 == 1) return((b * pow(b, e - 1, m)) %% m)
    half <- pow(b, e / 2, m)
    return((half * half) %% m)
}

# Greatest common divisor
gcd <- function(a, b) {
    if (a < b) return(gcd(b, a))
    if (b == 0) return(a)
    return(gcd(a %% b, b))
}


# Basic primality test
isPrime <- function(x) {
    if (x <= 1) return(FALSE)
    if (x == 2) return(TRUE)
    if (x != 2 & x %% 2 == 0) return(FALSE)
    
    n <- 3
    while (n * n <= x + 10) {
        if (x %% n == 0) return(FALSE)
        n <- n + 1
    }
    
    return(TRUE)
}
