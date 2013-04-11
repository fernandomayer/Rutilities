find.seq <- function(term, resp, data, offset = NULL,
                     fun = c("glm","glm.nb","zeroinfl","hurdle"),...){
    ## funcao para determinar automaticamente, atraves do AIC, a
    ## sequencia de variaveis que devem entrar em um modelo
    term <- term
    m.AIC <- data.frame(Terms = term, AIC = NA)
    fun <- match.arg(fun)
    for(i in seq(along = term)){
        form <- ifelse(is.null(offset),
                       paste(resp, "~", term[i], sep = ""),
                       paste(resp, "~", "offset(", offset, ")",
                                     "+", term[i], sep=""))
        form <- as.formula(form)
        mod <- switch(fun,
                      glm = {glm(formula=form,data=data,...)},
                      glm.nb = {glm.nb(formula=form,data=data,...)},
                      zeroinfl = {zeroinfl(formula=form,data=data,...)},
                      hurdle = {hurdle(formula=form,data=data,...)}
                      )
        m.AIC$AIC[i] <- AIC(mod)
    }
    m.AIC <- m.AIC[order(m.AIC$AIC),]
    row.names(m.AIC) <- 1:nrow(m.AIC)
    return(m.AIC)
}
