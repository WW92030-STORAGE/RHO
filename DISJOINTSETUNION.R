dsu <- function(arr) {
    env <- new.env(hash = TRUE)
    env$link <- new.env(hash = TRUE)
    env$size <- new.env(hash = TRUE)
    for (i in 1 : length(arr)) {
        x <- toString(arr[i])
        env$link[[x]] <- x
        env$size[[x]] <- 1
    }
    
    # Get the representative of the group containing x
    env$find <- function(x2) {
        x <- toString(x2)
        if (is.null(env$link[[x]])) return(NULL)
        while (x != env$link[[x]]) x <- env$link[[x]]
        return(x)
    }
    
    # Check if 2 elements are in the same group
    env$same <- function(x2, y2) {
        x <- env$find(toString(x2))
        y <- env$find(toString(y2))
        if (is.null(x) | is.null(y)) return(NULL)
        return(x == y)
    }
    
    # Combine 2 groups
    env$unite <- function(x2, y2) {
        x <- env$find(toString(x2))
        y <- env$find(toString(y2))
        if (is.null(x) | is.null(y)) return(NULL)
        if (env$size[[x]] >= env$size[[y]]) {
            env$size[[x]] <- env$size[[x]] + env$size[[y]]
            env$link[[y]] <- x
        }
        else {
            env$size[[y]] <- env$size[[y]] + env$size[[x]]
            env$link[[x]] <- y
        }
    }
    return(env)
}

# Example
su <- dsu(c(0, 1, 2, 3, 4, 5, 6))
su$unite(3, 4)
su$size[["3"]] # 2

