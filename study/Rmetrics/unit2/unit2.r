# Rmetrics
# unit 2
# Paper Fractals and intrinsic time
# theussl
# 1. Download from the Rmetrics data base data sets for the USDCHF High 
# Frequency FX rates: USDCHF30m.csv. The data are half hourly Rates
# including weekends swith zero rate changes. Plot the time series of FX
# rates and returns, display the return distribution including and excluding
# the weekends, and finally compute basic statistics. 


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
plot(ECDF)
hist(as.vector(R),breaks=100)

# with formula
n <- sort(as.vector(R))
v <- (c(1:length(as.vector(R)))-1/2)/length(as.vector(R))
plot(n,v,type="l")


# 2. Plot the mean absolute change of the logarithmic price against the 
# time interval size over which it has been observed, figure 1.

# Scaling Law Plot:
par(mfrow = c(2, 2), cex = 0.7)
scalinglawPlot(returnsAllDays)
scalinglawPlot(returnsWeekdays)
    

# 3. Plot the cumulative distribution functions of price changes, observed
# over time intervals of 30 minutes, 1 day, and 1 week, figure 2. 
    
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
pnet(vecR,type="l")


# probability net of simulated data 
m <- mean(vecR)
s <- sd(vecR)
simNorm <- rnorm(length(vecR),m,s)
pnet(simNorm,type="l")

# probability net of aggregated data
Rdaily <- apply(matrix(as.vector(returnsWeekdays),nrow=48),2,sum)

pnet(Rdaily,type="l")



# 4. Plot the autocorrelation function of 30-minute price changes and the 
# absolute values of the price changes computed on the physical time 
# scale, figure 3.

    # Standard Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    acf(returnsAllDays, lag.max = 2*24*9)
    acf(abs(returnsAllDays), lag.max = 2*24*9)
    
    # Tailored Plot:
    ACF = acf(abs(returnsAllDays), lag.max = 2*24*9, plot = FALSE)
    ACF = as.vector(ACF$acf)
    LAG = 0:(length(ACF)-1)
    plot(LAG, ACF, type = "l", col = "steelblue", ylim = c(-0.1, 0.5))
    abline(h = 0, col = "grey")
    for (i in 1:9) {
        abline(v = i*48, col = "grey", lty = 3)
        abline(h = -0.1 + i*0.1, lty = 3, col = "grey")
    }
    title(main = "USDCHF: 30 Minutes Time Lags")
    # Add Returns:
    ACF = acf(returnsAllDays, lag.max = 2*24*9, plot = FALSE)
    ACF = as.vector(ACF$acf)
    lines(LAG, ACF, col = "brown")
    
    
# 5. Create an intra-weekly histogram of mean absolute hourly price changes,
# figure 4.
    
    # Get Data:
    Data = c(NA, abs(returnsAllDays))
    Data = matrix(Data, byrow = TRUE, ncol = 2*24*7)
    
    # Plot Frame:
    par(mfrow = c(2, 1), cex = 0.7)
    # 30-min Data:
    meanData = colMeans(Data, na.rm = TRUE)
    # Hourly Data:
    hourlyMeanData = rowSums(matrix(meanData, byrow = TRUE, ncol= 2))
    plot(x = 0:(24*7-1)/24, y = hourlyMeanData, type = "h",
        ylim = c(0, 0.3), xlab = "Day Of Week", ylab = "Hourly Mean Data")
    for (i in 0:7) abline(v = i, lty = 3, col = "grey")
    title(main = "USDCHF: Weekly Price Changes of Hourly Data")
    days = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    text(x = 1:7 - 0.5, y = rep(0.275, 7), days)


# ------------------------------------------------------------------------------
# Read the paper on "Apparent Scaling"


# 6. Fit the FX returns using distributions from the family of generalized
# hyperbolic distribution functions, figure 1, and create a qq-plot, figure 2.

    
    # Estimate NIG Parameters:
    args(nigFit)
    par(mfrow = c(1, 1))
    x = as.vector(returnsWeekdays)
    fit = nigFit(x[abs(x)>0], trace = TRUE)
    
    # Print(Parameters:
    slotNames(fit)
    names(fit@fit)
    print(fit@fit$estimate)
    
    # Assign Parameters:
    alpha = fit@fit$estimate[1]
    beta = fit@fit$estimate[2]
    delta = fit@fit$estimate[3]
    mu = fit@fit$estimate[4] 
    
    # Create Histogram Plot:
    par(mfrow = c(1,1))
    hist(returnsWeekdays, nclass = 100, probability = TRUE, 
        xlim = c(-0.4, 0.4), col = "steelblue", border = "white",
        main = "Excluding Weekends")
    s = seq(-0.4, 0.4, length = 501)
    lines(s, dnorm(s, mean(Returns), sd(Returns)), lwd = 2, col = "orange") 
    lines(s, dnig(s, alpha, beta, delta, mu), lwd = 2, col = "brown")    
    
    # Create Quantile Plot:
    par(mfrow = c(1,1))
    set.seed(4711)
    x = rnig(1000, alpha, beta, delta, mu)
    x = sort(x)
    p = (1:length(x) - 1/2)/length(x)
    # Quantiles - this will take some time ...
    y = qnig(p = p, alpha, beta, delta, mu) 
    plot(x, y, pch = 19, col = "steelblue", type="l",
        xlab = "Empirical Quantiles", ylab = "estimated Quantiles",
        xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6))
    grid()
    lines(x = c(-0.6, 0.6), y = c(-0.6, 0.6), col = "orange")
    title(main = "USDCHF - NIG Quantile Plot")
    
    
    # Homewok: Normal QQ-Plot.

    Q_25 <- quantile(x, 0.25)
    Q_75 <- quantile(x, 0.75)
    
    sim_Q_25 <- quantile(rnig(1000, alpha, beta))#mean(x), sd(x)), 0.25)
    sim_Q_75 <- quantile(rnig(1000, alpha, beta))#mean(x), sd(x)), 0.75)
    
    slope <- (Q_75-Q_25) / (sim_Q_75 - sim_Q_25)
    intercept <- sim_Q_25 - slope * Q_25
      abline(intercept, slope, col="blue")
    
   
      
    
    r_weekdays=sort(returnsWeekdays)
    #y=sort(rnorm(length(r_weekdays),mean(r_weekdays),sd(r_weekdays)))
    
    p = (1:length(r_weekdays) - 1/2)/length(r_weekdays)
    plot(r_weekdays,y)
    
    qqplot(r_weekdays,qnig(p=p))
    
    qqnorm(y)
   qqline(rnorm(1000,mean(returnsWeekdays),sd(returnsWeekdays)),col="red")
   y=qnorm(p)
   points(x,y,type="l")
   lines(
      
    
# 7. Write a R function which creates a NIG Shape Tringle. Estimate the
# parameters for USCHF on different time horizons and display
# the results in the NIG shape triangle, figure 3.

    # 30 min Horizon:
    par(mfrow = c(1, 1))
    xTriangle = c(-1, 0, 1, -1)
    yTriangle = c( 1, 0, 1,  1)
    plot(xTriangle, yTriangle, type = "l", xlab = "chi", ylab = "xi")
    xi =  1 /  sqrt( 1 + delta * sqrt(alpha^2 - beta^2) )
    chi = ( beta / alpha ) / xi
    points(chi, xi, pch = 19, cex = 1.5)
    title(main = "NIG Shape Triangle USDCHF")
    
    # Homework: NIG Triangle
    #   Add points from longer horizons to the plot. Note, that you can
    #   create several sample, eg. for 1 h data you can start at the full
    #   hour, but also every half hour.
    
    
# 8. Create a scaling law plot for the estimated parameters, figure 5.
# Show the influence on slightly modifying the skewness parameter, figures 6.

    # Arguments:
    args(scalinglawPlot)
    # function (x, span = ceiling(log(length(x)/252)/log(2)), doplot = TRUE, 
    #   labels = TRUE, details = TRUE, ...) 
    
    # Scaling law Plot:
    x = rnig(100000, alpha, beta, delta, mu)
    scalinglawPlot(x, span = 8)
    
    # Homework:
    #   Investigate the Influence for changing beta.
