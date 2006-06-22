## EDAVIS Exercise 10
## Stefan Theuﬂl, 0352689

### Teil 1

library("datasets")
library("MASS")
library("classPP")
data("Seatbelts")

## Projection Pursuit mit skalierten Daten

X <- scale(as.matrix(Seatbelts[,1:7]))
grp <- as.vector(Seatbelts[,8])

PP.opt <- PP.optimize.Huber("LDA",2,X,grp,cooling=0.999,r=1)

PP.optimize.plot(PP.opt,X,grp)


## Visualisierung der Hauptkomponenten 1:2

pc <- princomp(X)
biplot(pc$scores[,1:2],pc$loadings[,1:2])
scatterplot.matrix(X)

par(mfrow=c(1,2))

hist(pc$sco[,1],freq=F, xlab="1. Hauptkomponente",ylab="",main="",cex.lab=1.2)
lines(density(pc$sco[,1]))

hist(pc$sco[,2],freq=F, xlab="2. Hauptkomponente",ylab="",main="",cex.lab=1.2)
lines(density(pc$sco[,2]))


### Teil 2

n <- 500
p <- 50
set.seed(100)
trans <- eigen(cov(matrix( runif(n*p, 0.95, 1),ncol=p)))$vec
library("mvtnorm")
X1 <- rmvnorm(n, rep(0,p),  diag(p:1)) %*% trans
X2 <- rmvnorm(n, rep(1.5,p),diag(p:1)) %*% trans
X <- scale(rbind(X1,X2))
grp <- c(rep(1,n),rep(2,n))



## Projection Pursuit

PP.opt <- PP.optimize.Huber("LDA",2,X,grp,cooling=0.999,r=1)

PP.optimize.plot(PP.opt,X,grp)

# Man erkennt eine rechte und eine linke Gruppe

## Visualisierung der Hauptkomponenten 1:2
par(mfrow=c(1,2))
pc <- princomp(X)
hist(pc$sco[,1],freq=F, xlab="1. Hauptkomponente",ylab="",main="",cex.lab=1.2)
lines(density(pc$sco[,1]))


hist(pc$sco[,2],freq=F, xlab="2. Hauptkomponente",ylab="",main="",cex.lab=1.2)
lines(density(pc$sco[,2]))

