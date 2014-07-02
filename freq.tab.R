##======================================================================
## Funcoes para calculos em tabelas de frequencia
##======================================================================

## Funcao para calcular as frequencias: relativa, acumulada,
## relativa acumulada, dadas as frequencias absolutas em uma tabela
## de frequencias
freq.tab <- function(f){
    # total
    n <- sum(f)
    # frequencia relativa
    fr <- f/n
    # frequencia relativa em percentual
    frp <- fr * 100
    # frequencia acumulada
    F <- cumsum(f)
    # frequencia acumulada relativa
    Fr <- F/n
    # frequencia acumulada relativa em percentual
    Frp <- Fr * 100
    # saida
    saida <- data.frame(f = f, fr = fr, frp = frp,
                        F = F, Fr = Fr, Frp = Frp)
    return(saida)
}

## Funcao para calcular a media de uma tabela de frequencia, dados o
## ponto medio da classe (x) e a frequencia absoluta (f)
freq.mean <- function(x, f){
    n <- sum(f)
    xbar <- 1/n * sum(x * f)
    return(xbar)
}

## Funcao para calcular a variancia de uma tabela de frequencia, dados o
## ponto medio da classe (x) e a frequencia absoluta (f)
freq.var <- function(x, f){
    n <- sum(f)
    varf <- (1/(n-1)) * (sum(x^2 * f) - (sum(x * f)^2/n))
    return(varf)
}
