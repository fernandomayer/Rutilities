moda <- function(x){
    tab <- table(x)
    md <- as.numeric(rownames(tab)[which.max(tab)])
    return(md)
}
