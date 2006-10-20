# Rmetrics
# unit 2
# Paper Fractals and intrinsic time
# theussl

library("fSeries")
source("wktPapier.R")

URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/Wuertz/data/usdchf30min.csv"
download.file(URL, "usdchf30min.csv")
X = readSeries("usdchf30min.csv")
    
par(mfrow = c(2, 1), cex = 0.7)
plot(X, type = "l", col = "steelblue", ylab = "FX  Rate", 
        main = "USDCHF 30 min Rates")
grid()

R = returnSeries(X, percentage = TRUE, digits = 12)
plot(R, type = "l", col = "steelblue", ylab = "FX Return",
        main = "USDCHF 30 min Returns")
grid()   
abline(h = 0, col = "grey")

ECDF <- ecdf(as.vector(R))
ECDF
plot(ECDF)
par(mfrow=c(1,1))
plot(ECDF,type)
hist(as.vector(R),breaks=100)

n <- sort(as.vector(R))
v <- (c(1:length(as.vector(R)))-1/2)/length(as.vector(R))
plot(n,v,type="l")

wktPapier(as.vector(R))

vecR <- as.vector(R)
m <- mean(vecR)
s <- sd(vecR)


simNorm <- rnorm(length(as.vector(R)),m,s)

wktPapier(simNorm)

returnsAllDays = as.vector(seriesData(R))
returnsWeekdays = returnsAllDays[isWeekday(seriesPositions(R))]

Rdaily <- apply(matrix(as.vector(returnsWeekdays),nrow=48),2,sum)

wktPapier(Rdaily)

