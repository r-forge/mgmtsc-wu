# EDAVIS Übung 3
# Stefan Theussl
# 0352689

library(rrcov)
library(stats)
library(MASS)


# Einlesen der Daten
data(brain)

# log
x<-log(brain[,1])
y<-log(brain[,2])

# Ausgangslage
plot(x,y,main="Robuste Statistik")

# Least Square Fit
abline(lsfit(x,y)$coef,col="blue")
legend(2,2,"lsfit",fill="blue",col="blue")

# Tukey
abline(line(x,y)$coef,col="green")
legend(5,2,"tukey",fill="green",col="green")

# Theil

theil <- function(x,y){
  n <- length(x)
  beta <- NULL
  k <- 1
  for(i in 1:n)
    for(j in 1:n)
      if(i!=j){
        beta[k]<-(y[i]-y[j])/(x[i]-x[j])
        k <- k+1
      }
  bt <- median(beta)
  intercept <- median(y)/(bt*median(x))
  c(intercept,bt)
}

abline(theil(x,y),col="purple")
legend(8,2,"theil",fill="purple",col="purple")


# Siegel

siegel <- function(x,y){
  n <- length(x)
  beta <- matrix(nrow=n,ncol=n)
  for(i in 1:n)
    for(j in 1:n)
      if (i!=j)
        beta[i,j] <- (x[i]-x[j])/(y[i]-y[j])
  brm <- median(apply(beta,1,median,na.rm=TRUE))
  intercept = median(y)/(brm*median(x))
  c(intercept,brm)
}

abline(siegel(x,y),col="brown")
legend(2,0,"siegel",fill="brown",col="brown")


# LMS Regression
abline(lmsreg(y~x)$coef,col="red")
legend(5,0,"lmsreg",fill="red",col="red")

# LTS Regression
abline(ltsReg(x,y)$coef,col="yellow")
legend(8,0,"ltsreg",fill="yellow",col="yellow")
