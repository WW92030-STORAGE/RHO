# Iterative pseudorandom number generator (PRNG)
prng <- function(n, c, m) {
    res <- exp(n, 31, m)
    res <- res + c
    return(res %% m)
}

# Pollard's Rho Algorithm - Attempts to generate nontrivial divisors of N
rho <- function(n, c) {
    if (n == 1) return(1)
    
    # Prepare a list of small primes. This outs 82.89% of all integers
    smallprimes <- c(2, 3, 5, 7, 11, 13, 17, 19)
    for (i in 1 : 8) {
        if (n %% smallprimes[i] == 0) return(smallprimes[i])
    }
    
    x <- 2 
    y <- 2 
    # Each iteration, x is plugged once into the PRNG.
    # However, y is plugged TWICE into the PRNG.
    
    d <- 1 # Initial candidate
    while (d == 1) {
        x <- prng(x, c, n)
        y <- prng(y, c, n)
        y <- prng(y, c, n)
        d <- gcd(abs(x - y), n)
    }
    return(d) # Either a nontrivial divisor of N or N itself
    # WARNING: This method runs INFINITELY if N is prime.
}


# Factors an integer N
factor <- function(n) {
    if (isPrime(n)) return(n) # See the above warning
    res <- n
    while (res == n) {
        c <- floor(runif(1, 1, n))
        res <- rho(n, c)
    }
    return(res)
}
