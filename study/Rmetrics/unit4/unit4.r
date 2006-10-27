# Rmetrics
# unit 2
# Paper Fractals and intrinsic time
# theussl

require("fMultivar")

data(spc1970)
data <- spc1970
index <- "spc1970"

par(mfrow=c(2,1))

C <- log(data[,5])
time <- length(C)
dat <- 1970 + time/252
plot(x=date, y= 100*(C-C[1]),col="steelblue",
     type="l", main = paste("Index:", index))

position <- c(0,diff(cdoTA(C, 5, 34, 7)))
position <- sign(position)
signals <- abs(c(0, diff(position),0))
tl <- emaTA(diff(1:length(signals)[signals > 0.5]),2/1261)
plot(tl,type="l",xlab="Number of Trades", main="Averaged Trade Length")

