shade <- function(x, xf, gl){
    x1 <- seq(0,xf,0.1)
    y1 <- dchisq(x1,gl)
    x2 <- c(seq(x,xf,0.1),xf,x)
    y2 <- c(dchisq(seq(x,xf,0.1),gl),0,0)
    plot(x1,y1,type="l",xlab=expression(chi^2),ylab="Densidade")
    polygon(x2,y2,density=NA,col="black")
    abline(h=0)
}
