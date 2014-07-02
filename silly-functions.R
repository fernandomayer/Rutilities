## Amplitude
amp <- function(x, na.rm = TRUE){
    max(x, na.rm = na.rm) - min(x, na.rm = na.rm)
}

## CV
cv <- function(x, na.rm = TRUE){
    (sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)) * 100
}

## Z score
z <- function(x, na.rm = TRUE){
    (x - mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm)
}
