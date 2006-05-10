## EDAVIS Exercise 7
## Stefan Theußl, 0352689

# Übung 1

library(forward)
data(rainfall)
plot(rainfall[,1],rainfall[,2])
i<-lowess(rainfall[,1],rainfall[,2])
lines(i$x,i$y)
r <- rainfall[,2]-i$y
lx<-i$x[r<0]
ux<-i$x[r>=0]
rl<-lowess(lx,r[r<0])$y
ru<-lowess(ux,r[r>=0])$y

lines(lx,i$y[r<0]+rl)
lines(ux,i$y[r>=0]+ru)


# übung 2

library(MASS)
data(iris)
par(mfrow=c(2,2))
attach(iris)

plot(iris$Sepal.Length,iris$Sepal.Width)

d<-kde2d(iris$Sepal.Length,iris$Sepal.Width)

image(d,zlim=c(0,0.5))

contour(d,nlevels=15)

persp(d, phi = 30, theta = 20, d = 10)



