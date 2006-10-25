# Response Models
# Marketing Engineering p.33-37
# implemented by theussl

linear.model <- function(x,a,b)
  a + b*x

power.series.model <- function(x,coef){
  len <- length(coef)
  if(len==1)
    return(coef)
  out <- c(coef[len]*x^(len-1),power.series.model(x,coef[-len]))
  sum(out)
}

fractional.root <- function(x,a,b,c)
  a + b * x^c

semilog.model <- function(x,a,b)
  a + b * log(x)

exponential.model <- function(x,a,b)
  a * exp(b*x)

modexp.model <- function(x,a,b,c)
  a * (1 - exp(-b*x)) + c

logistic.model <- function(x,a,b,c,d)
  a / (1 + exp(-(b+c*x))) + d

gompertz.model <- function(x,a,b,c,d)
  a * b^(c*x) + d

adbudg.model <- function(x,a,b,c,d)
  b + (a - b) * x^c/(d + x^c)

x <- c(1:1000)

example.plots <- function(){
  par(mfrow=c(2,2))
  plot(x,exponential.model(x,5,0.004),type="l",main="Exponential Model")
  plot(x,modexp.model(x,800,0.004,100),type="l",main="Modified Exponential Model")
  plot(x,logistic.model(x,800,-1,0.005,100),type="l",main="Logistic Model")
  plot(x,adbudg.model(x,900,100,2,5000),type="l",main="ADBUDG Model")
}
