wktPapier <- function(x, qdist=qnorm, probs=NULL, line=TRUE,
                      xlab=NULL,
                      ylab="Wahrscheinlichkeit in %", ...)
{
# Fritz Leisch, 2005
#
    DOTARGS <- as.list(substitute(list(...)))[-1]
    DOTARGS <- paste(names(DOTARGS), DOTARGS, sep="=",
                     collapse=", ")

    xlab=deparse(substitute(x))

    x <- sort(x)
    QNAME <- deparse(substitute(qdist))
    qdist <- match.fun(qdist)
    
    y <- qdist(ppoints(length(x)), ...)

    if(is.null(probs)){
        probs <- c(.01, .05, seq(.1,.9, by=.1), .95, .99)
        if(length(x)>=1000)
            probs <- c(0.001, probs, .999)
    }

    qprobs <- qdist(probs, ...)
      
    plot(x, y, axes=FALSE, type="l", ylim=range(c(y,qprobs)),
         xlab=xlab, ylab=ylab)
    box()
    
    abline(h=qprobs, col="grey")
    axis(1)
    axis(2, at=qprobs, labels=100*probs)

    #points(x, y)

    QTEXT <- paste("Quantile: ", QNAME, sep="")
    if(nchar(DOTARGS))
        QTEXT <- paste(QTEXT, DOTARGS, sep=", ")
    mtext(QTEXT, side=1, line=3, adj=1)
    
    if(line){
        xl <- quantile(x, c(0.25, 0.75))
        yl <- qdist(c(0.25, 0.75), ...)
        slope <- diff(yl)/diff(xl)
        int <- yl[1] - slope * xl[1]
        abline(int, slope, col="red")
    }
}
