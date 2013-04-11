as.factor.NA <- function(x, NA.string){
    # Function to transform other styles of missing values in standard R
    # missing values (NA). After this, the levels of the factor will be
    # updated to contain NA and drop out the strange string. Arguments:
    # x: a factor
    # na: a character string with the name to be converted to NA
    if(!is.factor(x)) stop("'x' must be a factor")
    x[x == NA.string] <- NA
    x <- factor(x)
    return(x)
}
