# Rmetrics
# unit 2
# Paper Fractals and intrinsic time
# theussl

library("fSeries")

# load data
URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/Wuertz/data/usdchf30min.csv"
download.file(URL, "usdchf30min.csv")
X = readSeries("usdchf30min.csv")

# plot data
par(mfrow = c(2, 1), cex = 0.7)
plot(X, type = "l", col = "steelblue", ylab = "FX  Rate", 
        main = "USDCHF 30 min Rates")
grid()

# calculate returns
R = returnSeries(X, percentage = TRUE, digits = 12)
plot(R, type = "l", col = "steelblue", ylab = "FX Return",
        main = "USDCHF 30 min Returns")
grid()   
abline(h = 0, col = "grey")

returnsAllDays = as.vector(seriesData(R))
returnsWeekdays = returnsAllDays[isWeekday(seriesPositions(R))]


# empirical cumulative distribution function
# with function ecdf
ECDF <- ecdf(as.vector(R))
ECDF
plot(ECDF)
par(mfrow=c(1,1))
plot(ECDF,type)
hist(as.vector(R),breaks=100)

# with formula
n <- sort(as.vector(R))
v <- (c(1:length(as.vector(R)))-1/2)/length(as.vector(R))
plot(n,v,type="l")


# Scaling Law Plot:
par(mfrow = c(2, 2), cex = 0.7)
scalinglawPlot(returnsAllDays)
scalinglawPlot(returnsWeekdays)
    
# function probability net
pnet <- function(x, qdist=qnorm, xlab="Quantiles", ylab="Probability [%]", ... ){

  x <- sort(x)
  qdist <- match.fun(qdist)
  y <- qdist(ppoints(length(x)))
  probs <- c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999)
  qprobs <- qdist(probs)
  
  # build plot
  plot(x, y, axes=FALSE, ylim=range(c(y,qprobs)),xlab=xlab, ylab=ylab, ... )
  box()
  abline(h=qprobs, col="grey")

  axis(1)  
  axis(2, at=qprobs, labels=100*probs)

  xl <- quantile(x, c(0.25, 0.75))
  yl <- qdist(c(0.25, 0.75))
  slope <- diff(yl)/diff(xl)
  intercept <- yl[1] - slope * xl[1]
  abline(intercept, slope, col="red")
  
}

# probability net of empirical data
vecR <- as.vector(R)
pnet(vecR)


# probability net of simulated data 
m <- mean(vecR)
s <- sd(vecR)
simNorm <- rnorm(lengt(vecR),m,s)
pnet(simNorm)

# probability net of aggregated data
Rdaily <- apply(matrix(as.vector(returnsWeekdays),nrow=48),2,sum)

pnet(Rdaily)

