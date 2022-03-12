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

# Constructs the Z-array
zenith <- function(arr) {
    n <- length(arr)
    res <- memset(n, 0)
    res[1] <- -1
    x <- 0
    y <- 0
    
    for (i in 1 : n - 1) {
        temp <- c(res[i - x + 1], y - i + 1)
        res[i + 1] <- max(c(0, min(temp)))
        while ((i + res[i + 1] < n) & (arr[res[i + 1] + 1] == arr[res[i + 1] + i + 1])) {
            x <- i
            y <- i + res[i + 1]
            res[i + 1] <- res[i + 1] + 1
        }
    }
    
    return(res)
}


# Searches for the subarray needle inside array arr
search <- function(arr, needle) {
    n <- length(arr)
    spectrum <- append(needle, -1)
    spectrum <- append(spectrum, arr)
    
    z <- zenith(spectrum)
    res <- c()
    for (i in 1 : n) {
        if (z[i + length(needle) + 1] == length(needle)) res <- append(res, i)
    }
    return(res)
}

# Example

arr <- chararr("PROTOTYPEPROTOGENPROFESSIONALS")
arr
zenith(arr)

search(arr, chararr("PRO")) # 1 10 18
search(arr, chararr("GEN")) # 15
