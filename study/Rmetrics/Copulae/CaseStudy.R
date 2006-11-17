## Case Study zur Copula:
## Index Dow Jones (30) und DAX (30) runterladen
## Welche Daten fehlen, auffuellen
## Berechnung der normalen Korrelation
## Fuehren Einbrueche im DI auch zu Einbruechen im

## ^DJI und ^GDAX

library(tseries)

## Einlese mit Funktion von Hornik:

## dj <- as.timeSeries(get.hist.quote("^DJI",quote="Close") )
## dax <- as.timeSeries(get.hist.quote("^GDAXI",quote="Close"))
## IDX <- merge(dj,dax)


require(fSeries)
library(fBasics)

## Versuch mit yahooSeries - funktioniert nicht:

# args(yahooSeries)
# x <- yahooSeries(c("^DJI", "^GDAXI"), quote = "Close", getReturns = TRUE, nDaysBack = 2500)

## 

idx <- readSeries(file = "idx.csv")
print(idx)

dji <- as.vector(idx[, 1])
dax <- as.vector(idx[, 2])


plot(dax, dji, col = "steelblue", pch = 19)
grid()


## Wie kommt man auf Copula?
## B(x, y) = f(x) g(x) C(F(x), G(x))

## C
