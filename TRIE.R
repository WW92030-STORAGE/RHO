trienode <- function(x = NULL) {
    retval <- new.env(hash = TRUE)
    retval$value <- x
    retval$children <- new.env(hash = TRUE)
    retval$end <- FALSE
    
    retval$set <- function(x) { # Sets the value of value
        retval$value <- x
    }
    
    retval$addChild <- function(a, b = NULL) {
        retval$children[[toString(a)]] <- trienode(b)
    }
    
    retval$get <- function(x) {
        return((retval$children)[[toString(x)]])
    }
    
    retval$contains <- function(x) {
        return(!is.null(retval$get(x)))
    }
    
    retval$push <- function(x) {
        if (is.null(retval$get(x))) retval$addChild(x)
        return(retval$get(x))
    }
    
    retval$isLeaf <- function() {
        return(length(retval$children) == 0)
    }
    
    return(retval)
}

trie <- function() {
    retval <- new.env(hash = TRUE)
    retval$root <- trienode()
    
    retval$insert <- function(s) {
        n <- nchar(s)
        node <- retval$root
        res <- node
        
        for (i in 1 : n) {
            c <- substr(s, i, i)
            node <- node$push(c)
        }
        node$end <- TRUE
        retval$root <- res
    }
    
    retval$search <- function(s) {
        n <- nchar(s)
        node <- retval$root
        res <- node
        
        for (i in 1 : n) {
            c <- substr(s, i, i)
            if (!node$contains(c) || is.null(node$get(c))) {
                return(FALSE)
            }
            node <- node$push(c)
        }
        return(node$end)
    }
    
    retval$erase <- function(n0, s, depth) {
        n <- nchar(s)
        if (is.null(n0)) return(NULL)
        if (depth == n) {
            if (n0$end) n0$end <- FALSE
            if (n0$isLeaf()) n0 <- NULL
            return(n0)
        }
        
        c <- substr(s, depth + 1, depth + 1)
        n0$children[[c]] <- retval$erase(n0$get(c), s, depth + 1)
        
        if (n0$isLeaf() && !n0$end) n0 <- NULL
        return(n0)
    }
    
    retval$remove <- function(s) {
        retval$erase(retval$root, s, 0)
        return(s)
    }
    
    return(retval)
}

strings <- c("protogen", "primagen", "species", "terrain", "terraria", "terra", "terrarium", "prototype", "spectral", "deception", "darkness")
queries <- c("proto", "pro", "prime", "specs", "species", "terrarian", "terra", "spectrum", "deceptive", "protogen", "deceive", "dark")

ex <- trie()
for (i in 1 : length(strings)) ex$insert(strings[i])
for (i in 1 : length(queries)) {
    cat(queries[i], ex$search(queries[i]), "\n") 
} # "species terra protogen" are the only TRUE ones

ex$remove("proto")

for (i in 1 : length(queries)) {
    cat(queries[i], ex$search(queries[i]), "\n") 
} # "proto" is now FALSE however "protogen" is still TRUE
