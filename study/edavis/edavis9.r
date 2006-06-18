## EDAVIS Exercise 9
## Stefan Theuﬂl, 0352689

data(mtcars)

cars <- mtcars[,1:7]

s.cars <- scale(cars)

pc.cars <- princomp(s.cars)

eigen(cov(scale(cars)))    ## entspricht den loadings in princomp, siehe wie folgt

pc.cars$loadings

pc.cars$scores             ## m¸sste der Matrix A entsprechen

sing.cars <- svd(s.cars)

# U ist jene Matrix, die nach einer weiteren Zerlegung von X entsteht. Die Matrix
# ist von Dimension n x r mit orthonormalen Spalten. ( eigen(X%*%t(X)) )
# Weiters erh‰lt man die Matrix A: p x r mit orthonormalen Spalten. ( eigen(t(X)%*%X) )

biplot(pc.cars)

# Interpretation des Biplots
# der plot zeigt die Richtung und St‰rke der einzelnen Komponenten. im Hintergrund erkennt man
# die einzelnen Gruppierungen der Fahrzeuge je nach Komponente.
