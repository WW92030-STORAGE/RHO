rectToPolar <- function(arr) { # arr is an array of 3 integers from 0 to 255.
    ray <- arr / 255
    value <- max(ray)
    minimum <- min(ray)
    chroma <- value - minimum;
    
    hue <- 0;
    sixty <- acos(0.5);
    if (value == ray[1]) hue <- sixty * (ray[2] - ray[3]) / chroma
    if (value == ray[2]) hue <- sixty * (2 + ((arr[3] - ray[1]) / chroma))
    if (value == ray[3]) hue <- sixty * (4 + ((ray[1] - ray[2]) / chroma))
    if (chroma == 0) hue <- 0
    
    saturation <- 0;
    if (value != 0) saturation <- chroma / value;
    return(c(hue, saturation, value))
}

polarToRect <- function(arr) { # arr = [hue (rad), saturation (0 ... 1), value (0 ... 1)]
    chroma <- arr[2] * arr[3]
    sector <- arr[1] / acos(0.5)
    sector <- (sector + 12) %% 6
    inter <- chroma * (1 - abs((sector %% 2) - 1))
    
    rgb <- c(0, 0, 0)
    if (0 <= sector & sector < 1) rgb <- c(chroma, inter, 0);
    if (1 <= sector & sector < 2) rgb <- c(inter, chroma, 0);
    if (2 <= sector & sector < 3) rgb <- c(0, chroma, inter);
    if (3 <= sector & sector < 4) rgb <- c(0, inter, chroma);
    if (4 <= sector & sector < 5) rgb <- c(inter, 0, chroma);
    if (5 <= sector & sector < 6) rgb <- c(chroma, 0, inter);
    
    rgb <- rgb + (arr[3] - chroma)
    return(floor(rgb * 255 + 0.5))
}

rotate <- function(arr, x) { # arr is a RGB triple. The function rotates the hue by x radians.
    hsv <- rectToPolar(arr)
    return(polarToRect(c(hsv[1] + x, hsv[2], hsv[3])))
}

col1 = c(255, 0, 0) # Example
hsv = rectToPolar(col1)
col2 = rotate(col1, acos(-0.5));
col2 # [0, 255, 0]
col3 = rotate(col1, acos(-0.5) * 2);
col3 # [0, 0, 255]


