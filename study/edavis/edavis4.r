# EDAVIS Übung 4
# Stefan Theussl
# 0352689

library(mvoutlier)

# Einlesen der Daten
data(humus)
as<-humus[,6]
pb<-humus[,26]

las<-log10(as)
lpb<-log10(pb)

# Density-Plot AS
plot(density(as))
plot(density(las))

# Density-Plot PB
plot(density(pb))
plot(density(lpb))

# Plot - Empirische Verteilungsfunktion

plot(ecdf(as))
plot(ecdf(las))
plot(ecdf(pb))
plot(ecdf(lpb))

# Wahrscheinlichkeitsnetz
# die folgende Funktion muss noch um die Skalierung erweitert werden
# konnte die Funktion axis nicht anwenden...

plot.pnet <- function(x){
  nv <- (x - mean(x))/sd(x)
  plot(sort(x),pnorm(sort(nv)),main="Wahrscheinlichkeitsnetz")
}

plot.pnet(pb)
plot.pnet(as)
plot.pnet(log(pb))
plot.pnet(log(as))
