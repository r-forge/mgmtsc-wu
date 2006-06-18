################################################################################
# Projection Pursuit
################################################################################

library(classPP)

# 1-dim Projection
data(iris)
ir <- scale(iris[,1:4])

# auf PC1
a <- princomp(ir)
hist(a$sco[,1],freq=F, xlab="1. Hauptkomponente",ylab="",main="",cex.lab=1.2)
lines(density(a$sco[,1]))
grp<-c(rep(1,50),rep(2,50),rep(3,50))
text(a$sco[,1],grp/10,grp)

PP.opt<-PP.optimize.Huber("LDA",1,ir,iris[,5],cooling=0.999,r=1)
hist(as.matrix(ir)%*%PP.opt$proj.best,freq=F,
xlab="Projektionsrichtung X",ylab="",main="",cex.lab=1.2)
lines(density(as.matrix(ir)%*%PP.opt$proj.best))
grp<-c(rep(1,50),rep(2,50),rep(3,50))
text(as.matrix(ir)%*%PP.opt$proj.best,grp/10,grp)


# 2d-Dichteschaetzung:
set.seed(101)
PP.opt<-PP.optimize.Huber("LDA",2,ir,iris[,5],cooling=0.999,r=1)
plot(as.matrix(ir)%*%PP.opt$proj.best,type="n",
xlab="Projektionsrichtung X1",ylab="Projektionsrichtung X2",main="",cex.lab=1.2)
text(as.matrix(ir)%*%PP.opt$proj.best,label=grp)

library(MASS)
projdat <- as.matrix(ir)%*%PP.opt$proj.best
f2 <- kde2d(projdat[,1], projdat[,2], n = 50)
persp(f2, phi = 30, theta = 20, d = 5,
xlab="Projektionsrichtung X1",ylab="Projektionsrichtung X2",
zlab="Dichteschätzung",cex.lab=1.2)


