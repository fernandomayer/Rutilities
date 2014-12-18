## Z com sigma conhecido
teste.hip.z <- function(alfa, xbarra, mu0, sigma, n,
                        alt = c("b", "e", "d")){
    alt <- match.arg(alt)
    vcrit <- switch(alt,
                    b = c(qnorm(alfa/2),
                        qnorm(alfa/2, lower.tail = FALSE)),
                    e = qnorm(alfa),
                    d = qnorm(alfa, lower.tail = FALSE))
    estat <- (xbarra - mu0)/(sigma/sqrt(n))
    saida <- list("valor critico" = vcrit,
                  "estatistica de teste" = estat,
                  "decisao" = ifelse(abs(estat) > abs(vcrit[1]),
                      "rejeita H0", "nao rejeita H0"))
    return(saida)
}

## Z para proporcao
teste.hip.zp <- function(alfa, pi0, n, x, p = NULL,
                         alt = c("b", "e", "d")){
    alt <- match.arg(alt)
    vcrit <- switch(alt,
                    b = c(qnorm(alfa/2),
                        qnorm(alfa/2, lower.tail = FALSE)),
                    e = qnorm(alfa),
                    d = qnorm(alfa, lower.tail = FALSE))
    p <- ifelse(is.null(p), x/n, p)
    estat <- (p - pi0)/(sqrt((pi0 * (1-pi0))/n))
    saida <- list("valor critico" = vcrit,
                  "estatistica de teste" = estat,
                  "decisao" = ifelse(abs(estat) > abs(vcrit[1]),
                      "rejeita H0", "nao rejeita H0"))
    return(saida)
}

## teste t
teste.hip.t <- function(alfa, xbarra, mu0, dp, n,
                        alt = c("b", "e", "d")){
                        # 'b' bilateral, 'e' esquerda, 'd' direita
    gl <- n - 1
    alt <- match.arg(alt)
    vcrit <- switch(alt,
                    b = c(qt(alfa/2, gl),
                        qt(alfa/2, gl, lower.tail = FALSE)),
                    e = qt(alfa, gl),
                    d = qt(alfa, gl, lower.tail = FALSE))
    estat <- (xbarra - mu0)/(dp/sqrt(n))
    saida <- list("valor critico" = vcrit,
                  "estatistica de teste" = estat,
                  "decisao" = ifelse(abs(estat) > abs(vcrit[1]),
                      "rejeita H0", "nao rejeita H0"))
    return(saida)
}

## teste t 2 amostras com sigma1 = sigma2
teste.hip.t2 <- function(alfa, xbarra1, xbarra2, var1, var2, n1, n2){
    gl <- n1 + n2 - 2
    s2c <- (((n1 - 1) * var1) + ((n2 - 1) * var2))/gl
    vcrit <- qt(alfa, gl)
    estat <- (xbarra1 - xbarra2)/(sqrt(s2c/n1 + s2c/n2))
    saida <- list("valor critico" = vcrit,
                  "estatistica de teste" = estat,
                  "decisao" = ifelse(abs(estat) > abs(vcrit),
                      "rejeita H0", "nao rejeita H0"))
    return(saida)
}
