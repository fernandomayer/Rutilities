## intervalo de confianca: sigma conhecido
icz <- function(alfa, xbarra, sigma, n, ic = 0){
    vcrit <- qnorm(alfa/2, lower.tail = FALSE)
    erro <- vcrit * (sigma/sqrt(n))
    icz <- xbarra + erro * c(-1,1)
    if(ic == 0){
        saida <- list("valor critico" = vcrit,
                      "margem de erro" = erro)
                      #"intervalo de confiança" = icz)
    } else{
        if(ic == 1){
            saida <- list(#"valor critico" = vcrit,
                          #"margem de erro" = erro)
                "intervalo de confiança" = icz)
    } else{
        if(ic == 2){
            saida <- list("valor critico" = vcrit,
                          "margem de erro" = erro,
                          "intervalo de confiança" = icz)
        }
    }}
    return(saida)
}
## tamanho da amostra
nam <- function(alfa, sigma, erro){
    vcrit <- qnorm(alfa/2, lower.tail = FALSE)
    n <- ((vcrit * sigma)/erro)^2
    n <- ceiling(n)
    return(n)
}
## intervalo de confiança: proporcao
iczp <- function(alfa, p, n, ic = 0){
    vcrit <- qnorm(alfa/2, lower.tail = FALSE)
    erro <- vcrit * sqrt((p * (1 - p))/n)
    iczp <- p + erro * c(-1,1)
    if(ic == 0){
        saida <- list("valor critico" = vcrit,
                      "margem de erro" = erro)
                      #"intervalo de confiança" = iczp)
    } else{
        if(ic == 1){
            saida <- list(#"valor critico" = vcrit,
                          #"margem de erro" = erro)
                "intervalo de confiança" = iczp)
    } else{
        if(ic == 2){
            saida <- list("valor critico" = vcrit,
                          "margem de erro" = erro,
                          "intervalo de confiança" = iczp)
        }
    }}
    return(saida)
}

## tamanho da amostra proporcao
namp <- function(alfa, p, erro){
    vcrit <- qnorm(alfa/2, lower.tail = FALSE)
    n <- (vcrit/erro)^2 * (p * (1 - p))
    n <- ceiling(n)
    return(n)
}
## intervalo de confiança: sigma desconhecido
ict <- function(alfa, xbarra, s, n, ic = 0){
    gl <- n - 1
    vcrit <- qt(alfa/2, df = gl, lower.tail = FALSE)
    erro <- vcrit * (s/sqrt(n))
    ict <- xbarra + erro * c(-1,1)
    if(ic == 0){
        saida <- list("valor critico" = vcrit,
                      "margem de erro" = erro)
                      #"intervalo de confiança" = ict)
    } else{
        if(ic == 1){
            saida <- list(#"valor critico" = vcrit,
                          #"margem de erro" = erro)
                "intervalo de confiança" = ict)
    } else{
        if(ic == 2){
            saida <- list("valor critico" = vcrit,
                          "margem de erro" = erro,
                          "intervalo de confiança" = ict)
        }
    }}
    return(saida)
}
## escore z
z <- function(xbarra, mu, sigma, n){
    z <- (xbarra - mu)/(sigma/sqrt(n))
    return(round(z, 2))
}
