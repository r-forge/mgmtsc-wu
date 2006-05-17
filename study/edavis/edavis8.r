## EDAVIS Exercise 8
## Stefan Theu�l, 0352689

# Teil 1

library("datasets")
library("MASS")
data("Seatbelts")

plot(as.data.frame(Seatbelts[,3:4]),col=Seatbelts[,8]+1)


stars(Seatbelts[,c(1,3,4)], key.loc = c(27, 1), main = "StarPlot Seatbelts",flip.labels=FALSE,col.stars=(Seatbelts[,8]==1)+2)
# hier sieht man sehr deutlich, dass nach Einf�hrung der Gurtpflicht (gr�ne Sterne) die Zahl der Todesopfer zur�ckging.


parcoord(as.data.frame(Seatbelts[,c(1,3,4,8)]))
# hier ist auch ein R�ckgang erkennbar, aber nicht so deutlich wie vorhin.

# Teil 2

load("olive.R")
names(olive)

parcoord(olive[,4:10],col = olive$Area,cex.main=1.5,main="Fetts�uregehalt von Oliven�l")
# man erkennt, dass bei den meisten Fetts�uren ein regionaler Zusammenhang vorhanden ist. vorallem bei linoleic und oleic sieht man eine deutliche Gruppierung

stars(as.data.frame(olive[,4:10]), key.loc = c(50,0), main = "stars per region",labels="",col.stars=(olive$Area))
# �ber stars kann man grob regionale Eigenheiten feststellen. Details sind aber schwer erkennbar



