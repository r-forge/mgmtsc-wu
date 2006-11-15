###################################################
## Aufgabe 3.9                                   ##
###################################################


n = 10000
t = 5
K = 100
Z = rnorm(n, 0, sqrt(t))
St = 100 * exp(0.02 * t + 0.3 * Z)

## P(PT > 0)
PT <- (K - St) * (K - St > 0)
head(PT)
length(PT[PT > 0]) / length(PT)


## E(PT)
mean(PT)



###################################################
## Aufgabe 3.11                                  ##
###################################################



t = c(1:5)
n = 10000
St = NULL
for(i in 1:length(t)){
  Z = rnorm(n, 0, sqrt(t[i]))
  St = cbind(St, 100 * exp(0.02 * t[i] + 0.3 * Z))
}

mi = NULL
for(i in 1:nrow(St)){
  mi = c(mi,(min(St[i,])))
}

put = 100 - mi
put[put < 0] <- 0

mean(put)
length(put[put > 0]) / length(put)









