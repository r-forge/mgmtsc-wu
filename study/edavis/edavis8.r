## EDAVIS Exercise 8
## Stefan Theußl, 0352689

# Teil 1

library("datasets")
library("MASS")
data("Seatbelts")

plot(as.data.frame(Seatbelts[,3:4]),col=Seatbelts[,8]+1)

x <- Seatbelts[,3:4]
x.nolaw <- x[Seatbelts[,8]==0,]
x.law <- x[Seatbelts[,8]==1,]
length(x.law) <- length(x.nolaw)

parcoord(data.frame(x.law,as.vector(x.nolaw)))
stars(data.frame(x.law,as.vector(x.nolaw)))

# Teil 2

load("olive.R")
names(olive)




