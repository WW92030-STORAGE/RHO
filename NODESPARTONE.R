# DISCLAIMER - Pointers are something that can behave very unpredictably across languages.
# DISCLAIMER - If something goes wrong please contact me through Discord (Server link is on my profile).

# Nested environments

env1 <- new.env(hash = TRUE)
env1[["x"]] <- new.env(hash = TRUE)

env1[["x"]][["y"]] <- 12
env1[["x"]][["y"]] # 12

env2 <- env1[["x"]]
env2[["y"]] # 12

# Node structure (generalized)

node <- function(x = NULL) {
    retval <- new.env(hash = TRUE)
    retval$value <- x
    retval$children <- new.env(hash = TRUE)
    
    retval$set <- function(x) { # Sets the value of value
        retval$value <- x
    }
    
    retval$addChild <- function(a, b) {
        retval$children[[toString(a)]] <- node(b)
    }
    
    retval$get <- function(x) {
        return((retval$children)[[toString(x)]])
    }
    
    retval$contains <- function(x) {
        return(!is.null(retval$get(x)))
    }
    
    return(retval)
}

# Linked List

listnode <- function(x = NULL) {
    retval <- new.env(hash = TRUE)
    retval$value <- x
    retval$link <- NULL
    retval$set <- function(x) { # Sets the value of value
        retval$value <- x
    }
    
    retval$setNext <- function(n = 0) {
        n0 <- listnode(n)
        retval$link <- n0
    }
    
    retval$getNext <- function() {
        return(retval$link)
    }
    
    return(retval)
}

# Binary Search Tree

bintreenode <- function(x = NULL) {
    retval <- new.env(hash = TRUE)
    retval$value <- x
    retval$left <- NULL
    retval$right <- NULL
    
    retval$setLeft <- function(n = 0) {
        n0 <- bintreenode(n)
        retval$left <- n0
    }
    
    retval$setRight <- function(n = 0) {
        n0 <- bintreenode(n)
        retval$right <- n0
    }
    
    retval$search <- function(x) {
        n0 <- retval
        res <- n0
        
        while (!is.null(n0)) {
            val <- n0$value
            if (val == x) return(TRUE)
            if (val < x) n0 <- n0$right
            else n0 <- n0$left
        }
        return(FALSE)
    }
    
    retval$insert <- function(x) {
        if (is.null(retval$value)) {
            retval$value <- x
            return(retval)
        }
        
        n0 <- retval
        res <- n0
        
        while (!is.null(n0)) {
            val <- n0$value
            if (val == x) {
                retval <- res
                return(res)
            }
            if (val < x) {
                if (is.null(n0$right)) {
                    n0$setRight(x)
                    retval <- res
                    return(res)
                }
                n0 <- n0$right
            }
            else {
                if (is.null(n0$left)) {
                    n0$setLeft(x)
                    retval <- res
                    return(res)
                }
                n0 <- n0$left
            }
        }
        retval <- res
        return(res)
    }
    
    return(retval)
}

# Example linked list

n0 <- listnode(4)
n <- n0 # Points the variable n to n0

n0$setNext(5)
n0 <- n0$getNext()
n0$setNext(6)

while (!is.null(n)) { # As n0 is updated, n is also
    print(n$value)
    n <- n$getNext()
} # OUTPUT: 4 5 6. This is the list n, which is identical to n0.

# Example binary search tree

b0 <- bintreenode()
arr <- c(5, 6, 2, 14, 7, 15, 10)
for (i in 1 : length(arr)) b0$insert(arr[i])

for (i in 1 : 16) cat(i, b0$search(i), "\n") # Only the numbers in the array arr return true
