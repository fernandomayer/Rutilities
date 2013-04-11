shift <- function(x){
    # Rescale 'x' to the range 0 to 1. In contrast with the original
    # function, missing values _are_ allowed, and infinite values are
    # discarded.
    # Author: J.M. Chambers, Green Book, pg. 31--32
    # R port: F.P. Mayer
    a <- min(x[is.finite(x)], na.rm = T)
    b <- max(x[is.finite(x)], na.rm = T) - a
    (x - a)/b
}
    
