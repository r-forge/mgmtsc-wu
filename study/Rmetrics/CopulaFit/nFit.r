 
 MLE f�r Normalverteilung:
 
 nFit <- function (y){ sum(log(dnorm(y, mean(y),sd(y))))}