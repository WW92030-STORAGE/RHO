# Create an array of n values, each of which is a copy of x
memset <- function(n, x) {
  if (n <= 0) return(x)
  arr <- c(x)
  for (i in 2 : n) arr <- append(arr, x)
  return(arr)
}

# Create an array of characters from a string
chararr <- function(s) {
    res <- c()
    n <- nchar(s)
    for (i in 1 : n) res <- append(res, utf8ToInt(substr(s, i, i)))
    
    return(res)
}

stringhash <- function(rad = 2017, modx = 10007) { # Hashes arbitrary arrays of fixed alphabet size
    env <- new.env(hash = TRUE)
    env$radix = rad
    env$mod = modx
    env$powers = new.env(hash = TRUE)
    env$powers[["0"]] = 1
    
    env$pow <- function(exponent = 0) {
        if (exponent <= 0) return(1)
        if (!is.null(env$powers[[toString(exponent)]])) return(env$powers[[toString(exponent)]])
        
        if (exponent %% 2 == 1) {
            res = env$pow(exponent - 1)
            res = (res * env$radix) %% env$mod
            env$powers[[toString(exponent)]] = res
            return(res)
        }
        
        res = env$pow(floor(exponent / 2))
        res = (res * res) %% env$mod
        env$powers[[toString(exponent)]] = res
        return(res)
    }
    
    env$phash <- function(arr) {
        n = length(arr)
        res = memset(n, 0)
        res[1] = arr[1]
        for (i in 2 : n) {
            res[i] = (env$radix * res[i - 1] + arr[i]) %% env$mod
        }
        return(res)
    }
    
    env$subhash <- function(v, a, b, in1 = FALSE) { # [a ... b]. Be aware that this is 0 indexed.
        if (!in1) {
            a = a + 1
            b = b + 1
        }
        
        if (a == 1) return(v[b])
        res = v[b]
        subscale = v[a - 1] * env$pow(b - a + 1)
        subscale = subscale %% env$mod
        res = res - subscale
        while (res < 0) res = res + env$mod
        return(res %% env$mod)
    }
    
    return(env)
}

vec = c(1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
vec2 = c(3, 5, 8, 13, 21)

hasher = stringhash()

v1 = hasher$phash(vec)
print(v1)
v2 = hasher$phash(vec2)
print(v2)
print(hasher$subhash(v1, 2, 6)) # Matches the last entry in the hashes of v2
