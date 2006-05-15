options(STERM='iESS', editor='emacsclient')
foo <- function(x, ...) UseMethod("foo")
foo
foo(1)
class(1)
foo.numeric <- function(x, ...) writeLines("Hallo.")
foo(1)
foo(1:10)
length(1 : 10)
class(1 : 10)
foo(pi)
foo(TRUE)
class(TRUE)
foo
foo.numeric
foo.logical <- function(x, ...) x
foo(1)
foo(TRUE)
foo.default <- function(x, ...) stop("You cannot be serious.")
foo(1)
foo(TRUE)
foo("Daniel")
foo.default <- function(x, ...) writeLines("You cannot be serious.")
foo("Daniel")
class(foo)
foo(foo)
foo.integer <- function(x, ...) writeLines("Hallo2 ...")
foo(1)
class(1)
typeof(1)
foo(1 : 10)
foo.bar <- function(x, ...) writeLines("I am a bar.")
x <- 1 : 10
class(x) <- "bar"
foo(x)
class(x)
class(x) <- c("bar", "numeric")
foo(x)
rm(foo.bar)
foo(x)
class(x)
rm(foo.numeric)
foo(x)
x <- 1 : 10
class(x) <- "logical"
foo(x)
y <- "Daniel"
class(y) <- "numeric")
class(y) <- "numeric"
class(y) <- c("character", "numeric")
print
methods("print")
x
class(x)
print(x)
class(x) <- "bar"
print.bar <- function(x, ...) cat("A bar object of length", length(x), "\n")
x
methods(plot)
methods(summary)
summary.aov
methods(summary)
getAnywhere("summary.nls")
summary.nls
getAnywhere("summary.nls")
methods("summary")
summary.lm
y <- runif(100)
x <- 1 : 100
z <- lm(y ~ x)
z
class(z)
unclass(z)
class(unclass(z))
typeof(unclass(z))
names(unclass(z))
z $ call
z $ coef
z
summary(z)
sz <- summary(z)
class(sz)
print.summary.lm
getAnywhere("print.summary.lm")
names(unclass(sz))
sz $ r.squared
z
summary(z)
plot(z)
plot(x, y)
abline(z)
abline
methods(lines)
class(hist(x))
x
y
hist(y)
z <- hist(y)
z
class(z)
plot(z)
plot(z, density = TRUE)
plot(z, density = FALSE)
z <- hist(y, density = TRUE)
z <- hist(y, proba = TRUE)
plot(z)
hist(y, proba = TRUE)
density(y)
lines(density(y))
methods(lines)
class(density(y))
lines.default
methods(xy.coords)

xy.coords(density(y))
methods(lines)
lines.default
names(density(y))
z <- 1 : 10
class(z) <- "bar"
z + z
unclass(z + z)
M <- matrix(c(1, 1, 1, 0, 1, 1, 0, 0, 1), nr = 3, byrow = TRUE)
M
class(M) <- "relation"
M
print.relation <- function(x, ...) cat("A binary relation on", nrow(x), "objects.\n")
M
M + 1
unclass(M + 1)
M ^ 3
unclass(M ^ 3)
unclass(- M)
class(M)
`+`
unclass(M)
unclass(M) + 1
M
Inz <- M
Inz <- unclass(M)
Inz
M1 <- list(incidences = Inz)
M1
class(M1) <- "relation"
M1
print.relation
print.relation <- function(x, ...) cat("A binary relation on", nrow(x$incidences), "objects.\n")
M1
M1 + 1
M1 ^ 2
unclass(M1)
M2 <- list(incidences = Inz, object_names = c("Mensa", "McD", "Zaus"))
M2
class(M2) <- "relation"
M2
M2
M2
M2 $ incidences
source("unit5.R")
incidences(M2)
M2
names(M2)
x <- c(1, 3, 5)
x
outer(x, x, "<=")
outer(x, x, "<=") * 1
source("unit5.R")
Inz
as.relation(Inz)
x
as.relation(x)
x <- runif(12)
as.relation(x)
z <- as.relation(x)
z
incidences(z)
source("unit5.R")
as.relation(Inz)
as.relation
methods(as.relation)
as.relation.matrix
source("unit5.R")
M2
dual(M2)
incidences(M2)
incidences(dual(M2))
q()
