## Homework
## Implementation of Hyperbolic Distribution
## Group: Köb Gerold, Theussl Stefan, Gartner Martin

library(fMultivar)
library(fSeries)
library(tseries)

## Hyperbolic Distribution (ohne Parameter):

dhyp <- function(x){
  1 / (2 * cosh(pi * x * 0.5))
}

phyp <- function(x){
  0.5 + 1/ pi * atan(sinh(pi * x * 0.5))
}

qhyp <- function(q){
  -2 / pi * asinh(tan(pi / 2 - pi * q))
}

rhyp <- function(x){
  y = runif(x,0,1)
  r = qhyp(y)
  r
}


    ## Test, ob Zufallszahlen zur theoretischen Dichte passen:

    x = seq(-5, 5, by = 0.001)
    hist(rhyp(100000), breaks = 100, probability = T)
    lines(x, dhyp(x), col=2)



## Generalised Hyperbolic Distribution:

    ## Definition von verwendeten Funktionen:
    ## Quelle: http://de.wikipedia.org/wiki/Arccot

    acot = function(z){
      pi/2-atan(z)
    }

    acoth = function(x){
      1 / 2 * log((x + 1) / (x - 1))
    }

dgsh <- function(x, t){
  if(-pi < t && t < 0){
    a <- cos(t)
    c2 <- sqrt((pi^2 - t^2) / 3)
    c1 <- sin(t) / t * c2
  }
  if(t < (-pi)){
    warning("t must be between -pi and infinity")
  }
  if(t == 0){
    a <- cos(t)

    c2 <- sqrt((pi^2 - t^2) / 3)
    c1 <- 1 * c2
  }
  else {
    a <- cosh(t)
    c2 <- sqrt((pi^2+t^2)/3)
    c1 <- sinh(t)/t*c2
  }   
  c1 * exp(c2 * x) / (exp(2 * c2 * x) + 2 * a * exp(c2 * x) + 1)
}

pgsh <- function(x, t){
  if(-pi < t && t < 0){
    c2 <- sqrt((pi^2 - t^2) / 3)
    p = 1 + 1 / t * acot(-(exp(c2 * x) + cos(t)) / sin(t))
  }
  if(t < (-pi)){
    warning("t between -pi and infinity")
  }
  if(t == 0){
    p = exp(pi * x / sqrt(3)) / (1 + exp(pi * x / sqrt(3)))
  }
  else {
    c2 <- sqrt((pi^2 + t^2) / 3)
    p = 1 - 1 / t * acoth((exp(c2 * x) + cosh(t)) / sinh(t))
  }
  p
}

qgsh = function(u, t){
  if(-pi < t && t < 0) {
    c2 <- sqrt((pi^2 - t^2) / 3)
    q = 1 / c2 * log(sin(t * u) / (sin(t * (1 - u))))
  }
  if(t < (-pi)) {
    warning("t between -pi and infinity")
  }
  if(t == 0) {
    q = sqrt(3) / pi * log(u / (1 - u))
  }
  else
    {
      c2 <- sqrt((pi^2 + t^2) / 3)
      q = 1 / c2 * log((sinh(t * u)) / (sinh(t * (1 - u))))
    }
  q
}


rgsh = function(x, t) {
  y = runif(x, 0, 1)
  r = qgsh(y, t)
  r
}

    ## Funktion zur Parameteranpassung:

gshFit <- function(x, t = 1) {
  LogLikeli <- function(x ,y = x) {
    ## t darf während der Optimierung nicht kleiner als -pi werden!!!
    if (x[1] <= -pi) {
      return(Inf)
    }
    f = -sum(log(dgsh(y, x[1])))
    f
  }
  r = nlm(f = LogLikeli, p = c(t), y = x)
  r
}

    ## Test: Zufallszahlen und theoretische Dichte:

    x = seq(-15, 15, by = 0.001)
    hist(rgsh(100000, -2), breaks = 100, probability = T)
    lines(x, dgsh(x, -2), col = 2)
    lines(x, dnorm(x), col = 3)    ## Norm.Vert. zum Vergleich




## SGSH Distribution:

dsgsh = function(x, t, g) {
  (2 /(g + 1 / g)) * ((dgsh(x / g, t) * (x < 0))+ (dgsh((x * g), t)) * (x >= 0))
}
 
psgsh <- function(x, t, g) {
  2 * g^2 / (g^2 + 1) * (pgsh(x / g, t) * (x < 0)+ ((g^2 - 1 + 2 * pgsh(g * x, t)) / (2 * g^2)) * (x >= 0))
}


qsgsh <- function(x, t, g) {
  y <- NULL
  for(i in 1 : length(x)) {
    if(x[i] < g^2 / (1 + g)) {
      y <- c(y, g * qgsh( (x[i] * (g^2 + 1) / (2 * g^2)), t))
    }
    else  {
      y <- c(y, 1 / g * qgsh((x[i] * (g + 1) / 2 - (g - 1) / 2), t))
    }
  }
  y
}


rsgsh = function(x, t, g) {
  y = runif(x, 0, 1)
  r = qsgsh(y, t, g)
  r
}


sgshFit <- function(x, t = 1, g = 1) {
  LogLikeli <- function(x ,y = x) {
    ## t darf während der Optimierung nicht kleiner als -pi werden!!!
    if (x[1] <= -pi) {
      return(Inf)
    }
    f = -sum(log(dsgsh(y, x[1], x[2])))
    f
  }
  r = nlm(f = LogLikeli, p = c(t, g), y = x)
  r
}


## Bsp: Histogramm ueber Zufallszahlen und theoretische Dichte:
x = seq(-5, 20, by = 0.001)
hist(rsgsh(1000, 1, 0.3), breaks = 100, probability = T)
lines(x, dsgsh(x, 1, 0.3), col = 2)



#########################################
############### Beispiele ###############
#########################################


## Beispiele: Anpassung mit sgshFit an 1000 Zufallszahlen (erzeugt mit "rsgsh"):

a <- rsgsh(1000, 2, 0.25)
erg <- sgshFit(a)
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
hist(a, breaks = 100, probability = T)
lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), t_est, g_est), col = 2)
lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), 2, 0.25), col = 3)


t <- 6
g <- 0.9
a <- rsgsh(10000, t, g)
erg <- sgshFit(a)
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
hist(a, breaks = 100, probability = T)
lines(seq(-5, 20, 0.01), dsgsh(seq(-5, 20, 0.01), t_est, g_est), col = 2)
lines(seq(-5, 20, 0.01), dnorm(seq(-5, 20, 0.01), 0.02, 0.8), col = 3)



## Anpassung mit sgshFit an ATX-Daten:

atx <- get.hist.quote("^ATX",quote="Close")
atx <- timeSeries(coredata(atx), index(atx))
r_atx <- returnSeries(atx, percentage = TRUE)
hist(as.vector(r_atx), breaks = 100, xlim = c(-5, 5), probability = TRUE)
erg <- sgshFit(as.vector(r_atx))
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
lines(seq(-5, 5, 0.01), dsgsh(seq(-5, 5, 0.01), t_est, g_est), col = 2)
lines(seq(-5, 5, 0.01), dnorm(seq(-5, 5, 0.01), mean(as.vector(r_atx)), sd(as.vector(r_atx))), col = 3)

## Anpassung mit sgshFit an Nikkei-Daten:
## nikkei225

nik <- get.hist.quote("^N225",quote="Close")
nik = timeSeries(coredata(nik),index(nik))
r_nik  <- returnSeries(nik, percentage = TRUE)
hist(as.vector(r_nik),breaks=100,probability=TRUE)
erg <- sgshFit(as.vector(r_nik))
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
lines(seq(-5, 5, 0.01), dsgsh(seq(-5, 5, 0.01), t_est, g_est), col = 2)
lines(seq(-5, 5, 0.01), dnorm(seq(-5, 5, 0.01), mean(as.vector(r_nik)), sd(as.vector(r_nik))), col = 3)


## Anpassung mit sgshFit an S&P500-Renditen:
## S&P 500: Datei enthält bereits Renditen (aber nicht in %)

X = readSeries("sp500dge.csv")
x <- 100 * as.vector(X)
hist(x, breaks = 100, probability = TRUE, xlim = c(-7, 7))
erg <- sgshFit(x)
t_est <- erg $estimate[1]
g_est <- erg $estimate[2]
lines(seq(-5, 5, 0.01), dsgsh(seq(-5, 5, 0.01), t_est, g_est), col = 2)
lines(seq(-5, 5, 0.01), dnorm(seq(-5, 5, 0.01), mean(x), sd(x)), col = 3)


## Beispiel: Anpassung mit gshFit an 1000 Zufallszahlen

a <- rgsh(1000, t = 3)
erg <- gshFit(a)
t_est <- erg $estimate[1]
hist(a, breaks = 100, probability = T)
lines(seq(-5, 20, 0.01), dgsh(seq(-5, 20, 0.01), t_est), col = 2)
lines(seq(-5, 20, 0.01), dgsh(seq(-5, 20, 0.01), 2), col = 3)


## Beispiel: Anpassung mit gshFit an S&P500-Renditen

X = readSeries("sp500dge.csv")
x <- 100 * as.vector(X)
hist(x, breaks = 200, probability = TRUE, xlim = c(-7, 7))
erg <- gshFit(x)
t_est <- erg $estimate[1]
lines(seq(-5, 5, 0.01), dgsh(seq(-5, 5, 0.01), t_est), col = 2)
lines(seq(-5, 5, 0.01), dnorm(seq(-5, 5, 0.01), mean(x), sd(x)), col = 3)
