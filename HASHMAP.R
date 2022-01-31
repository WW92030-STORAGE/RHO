# Insert [k, v] into hashmap h
insert <- function(h, k, v) {
    h[[k]] <- v
}

# Retrieve the value that k maps to in hashmap h
get <- function(h, k) {
    return(h[[k]])
}

# Does k exist as a key in hashmap h?
contains <- function(h, k) {
    res <- get(h, k)
    return(is.null(res))
}

# Remove the pair [k, v] of given key k from hashmap h
remove <- function(h, k) {
    rm(list = c(k), envir = h)
}

# array of keys for hashmap h
keySet <- function(h) {
    return(unlist(ls(h)))
}

# Example HashMap instance

dr <- new.env(hash = TRUE)
insert(dr, "kris", "human")
insert(dr, "susie", "lizard")
insert(dr, "ralsei", "goat")
insert(dr, "noelle", "deer")
insert(dr, "lancer", "card")
get(dr, "kris") # "human"
get(dr, "ralsei") # "goat"
get(dr, "hyperlinkblocked") # NULL
remove(dr, "lancer")
get(dr, "lancer") # NULL

ks <- keySet(dr)
for (i in 1 : length(ks)) cat(get(dr, ks[i]), " ") # human deer goat lizard
