######################################################
# Kapitel 3
######################################################


data(iris)

# Scatter plot
plot(iris[,1:2],cex=iris[,3]/2,pch=1,cex.lab=1.2)
points(iris[,1:2],cex=iris[,3]/2,col=gray(iris[,4]/max(iris[,4])),pch=16)
title("Petal.Length = size; Petal.Width = gray level")


# alle Paare von Scatter plots
pairs(iris[1:4],pch = 21, bg = c("white", "black", "gray")[unclass(iris$Species)])



# 3-dim Visualisierung
library(rgl)
example(rgl.surface)


# GGobi
cd ~/programme/ggobi/data
../bin/ggobi


# Profile (bar plots)
data(VADeaths, package = "datasets")

barplot(VADeaths, beside = TRUE,
col=gray(c(0.1,0.3,0.5,0.7,0.9)),
legend = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)

barplot(t(VADeaths), beside = TRUE,
col=gray(c(0.2,0.4,0.6,0.8)),
legend = colnames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)


# Stars
data(mtcars)

# ohne Skalierung
stars(mtcars[, 1:7], key.loc = c(14, 1.5), main = "",flip.labels=FALSE, scale=F)
# Standardisierung
stars((scale(mtcars[, 1:7])+3)/6, key.loc = c(14, 1.5), main = "",flip.labels=FALSE, scale=F)
# Bereich 0-1
stars(mtcars[, 1:7], key.loc = c(14, 1.5), main = "",flip.labels=FALSE)

# mit Ortsbezug
library(mvoutlier)
data(humus)
data(kola.background)
set.seed(104)
sel=sample(1:nrow(humus),70)
el=c("Cu","Co","Ni","Na","Sr","Bi","Pb","Rb")
x=humus[sel,el]
xy=humus[sel,c("XCOO","YCOO")]
pkb()
stars(log(x),locations=xy,add=T,labels=NULL,len=15000)



# Segments
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
draw.segments = TRUE, col.segments=gray(seq(from=0.1,to=0.9,length=7)))


# Boxes
source("~/praktika/juschitz/ver2/boxes.R")
library(mvoutlier)
data(humus)
data(kola.background)
set.seed(104)
sel=sample(1:nrow(humus),70)
el=c("Cu","Co","Ni","Na","Sr","Bi","Pb","Rb")
x=humus[sel,el]
xy=humus[sel,c("XCOO","YCOO")]
pkb()
boxes(x,location=xy,len=10000,leg=TRUE)


# Parallele Koordinaten

data(iris)
library(MASS)
parcoord(iris[,1:4],col=(c(rep(2,50),rep(3,50),rep(4,50))))

parcoord(mtcars[,1:7],col=gray(0.2))

# Platzer
cd ~/praktika/platzer
/usr/java/jdk1.5.0_05/bin/java -classpath /home/filz/praktika/platzer ParCoord                 

