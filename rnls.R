rnls <- function(nls.obj, resp){
    # function to calculate the R-squared for a non-linear
    # regression. So, bear in mind this number may be somewhat
    # biased. For comparisons between models, use AIC
    # instead. Arguments:
    # - nls.obj: the result of a nls call
    # - resp:    the response variable
    if(!inherits(nls.obj, "nls")){
        stop("Object must have class 'nls'")
    }
    # (n - 1) * var(y), which is equivalent to sum((y - bar(y))^2)
    totalSS <- (length(resp[!is.na(resp)]) - 1) * var(resp, na.rm=T)
    # from the residuals: the deviance of the model
    residualSS <- deviance(nls.obj)
    # R-squared is regressionSS/totalSS, so:
    Rsquared <- 1 - (residualSS/totalSS)
    names(Rsquared) <- "R-squared for a 'nls' object"
    return(Rsquared)
}

