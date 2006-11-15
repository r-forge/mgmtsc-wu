## Beispiel 3.10:

## Calloption:

T <- 3
K <- 110
q <- 2/3
S0 <- 100
U <- 1.21
D <- 0.88
r <- log(1.1)

x <- T:0
S3 <- S0 * U^x * D^(T - x)

## untersuchen, wo Call im Geld ist

S3[S3 - K < 0] <- 0

## abzinsen:

C3 <- exp(-3*r) * (S3 - 100)*(S3 - 100 > 0)   ## Achtung: Ausuebungspreis des Calls!
C3

## Berechnung aller Knotenwerten:

C2 <- NULL
for(i in 1:T ) {
  C2 <- c(C2, C3[i]*q + C3[i+1]*(1-q))
}

## Anmerkung: hier gehört eigentlich noch zweimal aufgezinst!
C2

C1 <- NULL
for(i in 1:(T-1)) {
  C1 <- c(C1, C2[i]*q + C2[i+1]*(1-q))
}

## Anmerkung: hier gehört eigentlich noch einmal aufgezinst!
C1

C0 <- NULL
for(i in 1:(T-2)) {
  C0 <- c(C0, C1[i]*q + C1[i+1]*(1-q))
}

C0


length(C0) <- 4
length(C1) <- 4
length(C2) <- 4
length(S0) <- 4
length(S1) <- 4
length(S2) <- 4

X <- matrix(c(C0, C1, C2, C3), 4, 4, byrow = FALSE)
Y <- matrix(c(S0, S1, S2, S3), 4, 4, byrow = FALSE)


library(fOptions)
par(mfrow = c(2,2))
BinomialTreePlot(Y, digits = 1, cex = 0.7, dy = 0.17)
BinomialTreePlot(X, digits = 1, cex = 0.7, dy = 0.17)


#######################################################################
#######################################################################
## Putoption:                                                        ##
#######################################################################
#######################################################################

T <- 3
K <- 110
q <- 2/3
S0 <- 100
U <- 1.21
D <- 0.88
r <- log(1.1)

x <- T:0
S3 <- S0 * U^x * D^(T - x)
S3

## abzinsen:

C3 <- exp(-3*r) * (K - S3)*(K - S3 > 0)  ## Letzte Bedingung sagt, dass Put im Geld sein muss
C3

## Berechnung aller Knotenwerten:

C2 <- NULL
for(i in 1:T ) {
  C2 <- c(C2, C3[i]*q + C3[i+1]*(1-q))
}

## Anmerkung: hier gehört eigentlich noch zweimal aufgezinst!
C2

C1 <- NULL
for(i in 1:(T-1)) {
  C1 <- c(C1, C2[i]*q + C2[i+1]*(1-q))
}

## Anmerkung: hier gehört eigentlich noch einmal aufgezinst!
C1

C0 <- NULL
for(i in 1:(T-2)) {
  C0 <- c(C0, C1[i]*q + C1[i+1]*(1-q))
}

C0


## Graph:

x <- (T-1):0
S2 <- S0 * U^x * D^((T-1) - x)
x <- (T-2):0
S1 <- S0 * U^x * D^((T-2) - x)



length(C0) <- 4
length(C1) <- 4
length(C2) <- 4
length(S0) <- 4
length(S1) <- 4
length(S2) <- 4

X <- matrix(c(C0, C1, C2, C3), 4, 4, byrow = FALSE)
Y <- matrix(c(S0, S1, S2, S3), 4, 4, byrow = FALSE)

library(fOptions)
par(mfrow = c(2,2))
BinomialTreePlot(Y, digits = 1, cex = 0.7, dy = 0.17)
BinomialTreePlot(X, digits = 1, cex = 0.7, dy = 0.17)

