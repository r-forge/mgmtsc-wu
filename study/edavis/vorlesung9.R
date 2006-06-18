######################################################
# Kapitel 3
######################################################

# Parallele Koordinaten

data(iris)
library(MASS)
parcoord(iris[,1:4],col=(c(rep(2,50),rep(3,50),rep(4,50))))

parcoord(mtcars[,1:7],col=gray(0.2))

# Platzer
cd ~/praktika/platzer
/usr/java/jdk1.5.0_05/bin/java -classpath /home/filz/praktika/platzer ParCoord                 

library(mvoutlier)
data(chorizon)
x=chorizon[,101:110]
parcoord(x)
parcoord(log10(x))

library(robustbase)
res=covMcd(log10(x))
parcoord(x,col=res$mcd.wt+2)
data(kola.background)
pkb()
points(chorizon[,"XCOO"],chorizon[,"YCOO"],col=res$mcd.wt+2)



######################################################
# Kapitel 4
######################################################

# Richtung der PC
acroread ~/latex/papers/florenz06/vortrag/mov.pdf &

# Beispieldaten fuer PCA
library(mvoutlier)
data(chorizon)
x=log10(chorizon[,101:105])
boxplot(x)

# zentrierte Daten
x=scale(x,T,F)
boxplot(as.data.frame(x))
plot(as.data.frame(x))

# Gesamtvariation
sum((x-mean(x))^2)
sig=cov(x)

# robuste Kovarianz
rob=covMcd(x)
sig1=rob$cov
cent1=rob$cent

# Eigenwertzerlegung
eig=eigen(sig)
A=eig$vectors
L=eig$values

# Hauptkomponenten
Y=x%*%A

# Variation
sum((Y-mean(Y))^2)
sum((Y[,1]-mean(Y))^2)

sum(L*605)

# Proble Eigenwertzerlegung
cov(x)%*%A-A%*%diag(L)

# andere orthogonale Matrix
B=eigen(cov(scale(log10(chorizon[,106:110]),T,F)))$vec
Z=x%*%B
sum((Z-mean(Z))^2)
sum((Z[,1]-mean(Z))^2)
diag(cov(Z))*605

det(cov(Z[,1:3]))

# Plot der Kov.-Struktur
library(ellipse)
plot(x[,1:2])
lines( ellipse(sig[1:2,1:2]))
lines( ellipse(sig1[1:2,1:2],centre=cent1),col=2)


# Vergleich theoretische und empirische PCs
# multivariate Nvt
library(mvtnorm)
set.seed(100)
Sigma.theo=matrix(c(4,3,3,6),ncol=2)
met=c(10,8)
x1=rmvnorm(100,mean=met,sigma=Sigma.theo)
Sig1=cov(x1)
me=apply(x1,2,mean)
A1=eigen(Sig1)$vec
Y1=x1 %*% A1

par(pty="s")
plot(x1[,1:2],xlim=c(0,20),ylim=c(0,20))
lines( ellipse(Sigma.theo, centre=met), col=3)
lines( ellipse(Sig1, centre=apply(x1,2,mean)), col=4)

arrows(me[1],me[2],10*A1[1,1]+me[1],10*A1[2,1]+me[2],col=4,lwd=2)
arrows(me[1],me[2],10*A1[1,2]+me[1],10*A1[2,2]+me[2],col=4,lwd=2)

# theoretisch
A1t=eigen(Sigma.theo)$vec
arrows(met[1],met[2],10*A1t[1,1]+met[1],10*A1t[2,1]+met[2],col=3,lwd=2)
arrows(met[1],met[2],10*A1t[1,2]+met[1],10*A1t[2,2]+met[2],col=3,lwd=2)


# Projektionen der Punkte in Unterraum:
B1=A[,1:2]
Y1=x%*%B1
sumdist=0
for (i in 1:nrow(x)){
   for (j in 1:nrow(x)){
     sumdist=sumdist+sum((Y1[i,]-Y1[j,])^2)
   }
}
plot(Y1)

B2=A[,2:3]
Y2=x%*%B2
sumdist=0
for (i in 1:nrow(x)){
   for (j in 1:nrow(x)){
     sumdist=sumdist+sum((Y2[i,]-Y2[j,])^2)
   }
}
points(Y2,col=2)



# Distorsion
B1=A[,1:2]
Y1=x%*%B1
sum(sqrt(apply((Y1%*%t(Y1) - x%*%t(x))^2, 1, sum)))

B2=A[,2:3]
Y2=x%*%B2
sum(sqrt(apply((Y2%*%t(Y2) - x%*%t(x))^2, 1, sum)))


