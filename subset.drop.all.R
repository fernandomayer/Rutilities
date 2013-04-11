subset.drop.all <- function(x, ...){
# funcao para fazer subset de um data.frame e 'dropar' fora *todos* os
# niveis excluidos de *todas* as colunas que sao fatores
    if(!is.data.frame(x)) stop("'x' must be a data.frame")
    x <- subset(x, ...)
    factors <- which(sapply(x, class) %in% "factor")
    for(i in factors){
        x[,i] <- sapply(x[,i], "[", drop=T) 
    }
    return(x)
}
