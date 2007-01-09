 
 MLE für Normalverteilung:
 
 nFit <- function (y){ sum(log(dnorm(y, mean(y),sd(y))))}