#Homework Implementation of Hyperbolic Distribution

dhyp <- function(x){
1/(2*cosh(pi*x*0.5))}

phyp <- function(x){
0.5+1/pi*atan(sinh(pi*x*0.5))}

qhyp <- function(q){
-2/pi * asinh(tan(pi/2 - pi*q))}

rhyp <- function(x){
y=runif(x,0,1)
r=qhyp(y)
r
}

x=seq(-5,5,by=0.001)
hist(rhyp(100000),breaks=100,probability=T)
lines(x,dhyp(x),col=2)
