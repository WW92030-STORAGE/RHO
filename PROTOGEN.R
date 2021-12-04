hex <- function(n) {
    n <- n %% 256
    p1 <- n %/% 16
    p2 <- n %% 16
    c1 <- paste(p1)
    c2 <- paste(p2)
    
    if (p1 >= 10) c1 <- intToUtf8(p1 - 10 + 65)
    if (p2 >= 10) c2 <- intToUtf8(p2 - 10 + 65)
    
    return(paste(c1, c2, sep = ""))
}

primagen <- function() {
    return(floor(runif(3, 0, 255)))
}

protogen <- function() { // Generate a random hex color
    rgb <- floor(runif(3, 0, 256))
    return(paste("\U0023", hex(rgb[1]), hex(rgb[2]), hex(rgb[3]), sep = ""))
}

for (x in 1 : 16) print(protogen())






