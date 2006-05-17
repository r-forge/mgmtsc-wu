# R Code
# Explorative Datenanalyse und Visualisierung
# Uebung 1, Stefan Theußl 0352689

#help.start()
#data()

# Daten als Vektor
sunsp<-as.vector(sunspots)

pdf("edavis1.pdf")

par(mfrow=c(3,1))

#verschiedene eindimensionale Streudiagramme

stripchart(sunsp)
stripchart(sunsp,method="stack")
stripchart(sunsp,method="jitter")

dev.off()
