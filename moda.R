moda <- function(x){
    tab <- table(x)
    md <- as.numeric(names(which(tab == max(tab))))
    return(md)
}
