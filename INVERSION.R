inv <- function(c) { // Change a hex string into a decimal integer.
  d1 <- utf8ToInt(substr(c, 1, 1))
  d2 <- utf8ToInt(substr(c, 2, 2))
  
  if (d1 >= 48 & d1 < 58) d1 <- d1 - 48
  else d1 <- d1 - 65 + 10
  
  if (d2 >= 48 & d2 < 58) d2 <- d2 - 48
  else d2 <- d2 - 65 + 10
  
  return(16 * d1 + d2)
}

invert <- function(c) { # Invert a hex color. The input is of the form "#XXXXXX"
  red <- 255 - inv(substr(c, 2, 3))
  green <- 255 - inv(substr(c, 4, 5))
  blue <- 255 - inv(substr(c, 6, 7))
  
  return(paste("\U0023", hex(red), hex(green), hex(blue), sep = ""))
}
