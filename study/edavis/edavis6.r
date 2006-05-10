## EDAVIS Exercise 6, Nachtrag
## Stefan Theußl, 0352689

# Übung 1

library(MASS)
data(Boston)
attach(Boston)
dat<-read.table("boston.txt",header=TRUE,sep="\t")
attach(dat)

plot(ptratio,tax)

plot(ptratio,jitter(tax,factor=50))


# Visualisierung der Punkte
plot(LAT,LON)
qu<-quantile(tax,0.75) # mich interessieren die punkte über dem 75% quantil
points(LAT[tax>=qu],LON[tax>=qu],col="red")

# übung 2

sboxplot <- function(x,count=5){
  len <- length(x)
  n <- ceiling(len/count)
  length(x)<-n*count
  m <- matrix(x,ncol=count)
  boxplot(as.data.frame(m))
}

ldis <- log(dis)
lcrim <- log(crim)

sboxplot(lcrim[order(ldis)])
# die kriminalität nimmt mit der distanz zu den arbeitsämtern ab.
