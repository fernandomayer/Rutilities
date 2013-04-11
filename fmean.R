fmean <- function(f, x){
# function to calculate the mean of a frequency distribution.
# arguments:
#   f: vector of frequencies
#   x: vector of classes
# first it will calculate the mid-point of the classes, which is
# needed for this purpose. Then it will return the mean of the dist.
    mp <- sum(x[1],x[2])/2
    mp <- abs(x[1] - mp)
    xbar <- sum(f * (x + mp))/sum(f)
    return(xbar)
}
