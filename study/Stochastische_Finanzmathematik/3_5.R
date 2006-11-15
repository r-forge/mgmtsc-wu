###################################################################
## Aufgabe 3.5:                                                   ##
####################################################################

U <- 1.025
D <- 0.97
p <- 0.6
S0 <- 100
K <- 110
T <- 50
### ACHTUNG: r???
r <- log(1.1)


## Matrix mit möglichen Kursentwicklungen:

x <- T:0
ST <- S0 * U^x * D^(T - x)
ST

Y <- matrix(0, T+1, T+1)
Y[, T+1] <- ST
for(n in T:1){
  for(i in n:1) {
    Y[i ,n] <- Y[i, n+1] / U
  }
}

## Calloption:

CT <- exp(-T*r) * (ST - K)*(ST - K > 0)  ## damit im Geld!!
CT

## Matrix mit Barwerten:

X <- matrix(0, 51, 51)
X[, (T+1)] <- CT
for(n in T:1){
  for(i in n:1) {
    X[i ,n] <- X[i, n+1]*p + X[i+1, n+1]*(1-p)
  }
}

X[1:5, 1:5]

library(fOptions)
par(mfrow = c(2,2))
BinomialTreePlot(X[1:5, 1:5])
BinomialTreePlot(Y[1:5, 1:5])


## Putoption:

PT <- exp(-T*r) * (K - ST)*(K - ST > 0)  ## damit im Geld!!
PT

## Matrix mit Barwerten:

X <- matrix(0, 51, 51)
X[, (T+1)] <- PT
for(n in T:1){
  for(i in n:1) {
    X[i ,n] <- X[i, n+1]*p + X[i+1, n+1]*(1-p)
  }
}

X[1:5, 1:5]

#library(fOptions)
#par(mfrow = c(2,2))
BinomialTreePlot(X[1:5, 1:5])
BinomialTreePlot(Y[1:5, 1:5])








## Wahrscheinlichkeiten:

X[1:5,1:P <- NULL
for(x in 0:T) {
  P[x] <- p^x * (1-p)^(T-x)
}



for(n in T:1) {
  for(i in n:1) {
  X[i, n] <- X[i, n+1]*q + X[i+1, n+1]*(1-q)
}
}

library(fOptions)
par(mfrow = c(2,2))
BinomialTreePlot(X[1:5, 1:5])
BinomialTreePlot(Y[1:5, 1:5])


## Wahrscheinlichkeitsmatrix:

P <- matrix(0, T, T)
for(i in T:0) {
  P[i, ] <- c(rep(1-p, i-1), rep(p, T-i+1))
}

P[1:5, 1:5]
sum(apply(P,1,prod))




dim(P)
diag <- diag(P)
P <- cbind(rep(1, T), P)
P <- upper.tri(P, diag = TRUE) * P
P[1:5, 1:5]
P[P == 0] <- 1
P[1:5, 1:5]





P <- matrix(0, 50, 50)
for(n in T:n) {
  x <- 0:n
  P[x+1, T] <- 0.6^x*0.4^(1-x)
}


P <- matrix(0, T+1, T+1)
P[,T +1] <- PT


P <- matrix(0, 51, 51)
P[,51] <- p^x*(1-p)^(51-x)
for(n in T:1){
  for(i in n:1) {
    P[i,n] <- P[i, n+1] / p
  }
}


