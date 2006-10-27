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


#Generalised Hyperbolic Distribution
acot=function(z){
pi/2-atan(z)
}

#http://de.wikipedia.org/wiki/Arccot

acoth=function(x){
1/2*log((x+1)/(x-1))
}

#http://de.wikipedia.org/wiki/Tanh

dgsh<-function(x,t){

if(-pi<t && t<=0){
a<-cos(t)
c2<-sqrt((pi^2-t^2)/3)
c1<-sin(t)/t*c2}
if(t< (-pi)){
warning("t between -pi and infinity")
}
else{
a<-cosh(t)
c2<-sqrt((pi^2-t^2)/3)
c1<-sinh(t)/t*c2}   

c1*exp(c2*x)/(exp(2*c2*x)+2*a*exp(c2*x)+1)
}

pgsh <- function(x,t){
if(-pi<t && t<0){
c2<-sqrt((pi^2-t^2)/3)
p=1+1/t *acot(-(exp(c2*x)+cos(t))/sin(t))}

if(t< (-pi)){
warning("t between -pi and infinity")
}

if(t==0)
p=exp(pi*x/sqrt(3))/(1+exp(pi*x/sqrt(3)))

else
{
c2<-sqrt((pi^2-t^2)/3)
p=1-1/t*acoth((exp(c2*x)+cosh(t))/sinh(t))
}
p
}


qgsh=function(u,t){
if(-pi<t && t<0){
c2<-sqrt((pi^2-t^2)/3)
q=1/c2*log(sin(t*u)/(sin(t*(1-u))))}

if(t< (-pi)){
warning("t between -pi and infinity")
}

if(t==0)
q=sqrt(3)/pi*log(u/(1-u))

else
{
c2<-sqrt((pi^2-t^2)/3)
q=1/c2*log((sinh(t*u))/(sinh(t*(1-u))))
}
q
}

rgsh=function(x,t){
y=runif(x,0,1)
r=qgsh(y,t)
r
}

x=seq(-15,15,by=0.001)
hist(rgsh(100000,-2),breaks=100,probability=T)
lines(x,dgsh(x,-2),col=2)
lines(x,dnorm(x),col=3)
