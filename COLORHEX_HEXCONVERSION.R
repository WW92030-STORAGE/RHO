hex <- function(n) { // Integer to hex string
    n <- n %% 256
    p1 <- n %/% 16
    p2 <- n %% 16
    c1 <- paste(p1)
    c2 <- paste(p2)
    
    if (p1 >= 10) c1 <- intToUtf8(p1 - 10 + 65)
    if (p2 >= 10) c2 <- intToUtf8(p2 - 10 + 65)
    
    return(paste(c1, c2, sep = ""))
}

inv <- function(c) { // Hex string to decimal integer.
  d1 <- utf8ToInt(substr(c, 1, 1))
  d2 <- utf8ToInt(substr(c, 2, 2))
  
  if (d1 >= 48 & d1 < 58) d1 <- d1 - 48
  else d1 <- d1 - 65 + 10
  
  if (d2 >= 48 & d2 < 58) d2 <- d2 - 48
  else d2 <- d2 - 65 + 10
  
  return(16 * d1 + d2)
}
