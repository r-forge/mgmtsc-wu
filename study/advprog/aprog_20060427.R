options(STERM='iESS', editor='emacsclient')
Sweave("unit4.Rnw")
x <- "sin(x + y)"
parse(text = x)
length(parse(text = x))
parse(text = x))[[1]]
parse(text = x)[[1]]
length(parse(text = x)[[1]])
parse(text = x)[[1]][[1]]
parse(text = x)[[1]][[2]]
parse(text = x)[[1]][[2]][[1]]
parse(text = x)[[1]][[2]][[2]]
parse(text = x)[[1]][[2]][[3]]
read.csv("unit4.csv")
read.csv("unit4.csv")
x <- read.csv("unit4.csv")
typeof(x)
class(x)
x <- read.csv("unit4.csv")
x
x$AGE
class(x$AGE)
class(x$WEIGHT)
class(x$EMPLOYED)
x <- read.csv("unit4.csv")
x
x <- read.csv("unit4.csv", na.strings = "-999")
x
class(x$WEIGHT)
x <- read.csv("unit4.csv")
x
x$WEIGHT
x$WEIGHT
class(x$WEIGHT)
typeof(x$WEIGHT)
as.numeric(x$WEIGHT)
levels(x$WEIGHT)
as.character(x$WEIGHT)
as.numeric(as.character(x$WEIGHT))
as.numeric("4,32")
x
prompt(x)
f <- function() writeLines("Hello world.")
package.skeleton(list = c("f", "x"), name = "Unit4")
library()
ls()
rm(list = ls())
f
x
library("Unit4")
f
x
data("x")
x
rm(list = ls())
detach("Unit4")
detach("package:Unit4")
library("Unit4")
vignette()
vignette("unit4")
detach("package:Unit4")
library("Unit4")
vignette("unit4")
vignette("unit4")
optim
s <- 3
n <- 8
f <- function(p) p ^ s * (1 - p) ^ (n - s)
optim(0.5, f)
optim(0.5, f, lower = 0, upper = 1)
optim(0.3, f, lower = 0, upper = 1)
optim(0.3, f, lower = 0, upper = 1, method = "L-BFGS-B")
f <- function(p)  - p ^ s * (1 - p) ^ (n - s)
optim(0.3, f, lower = 0, upper = 1, method = "L-BFGS-B")
f
source("unit4.R")
x <- rep(c(1, 0), each = c(3, 5))
x <- rep(c(1, 0), c(3, 5))
x
ls()
rm(s, n)
s
n
x
MLE2(x)
length(MLE2)
class(MLE2)
typeof(MLE2)
source("unit4.R")
x
L 
L <- make_L(x)
L
environment(L)
objects(environment(L))
get("s", environment(L))
environment(L)$s
s
source("unit4.R")
L <- make_L(x)
environment(L)$s
environment(L)
objects(environment(L))
optim(s / n, L, lower = 0, upper = 1, method = "L-BFGS-B")$par
optim(runif(1), L, lower = 0, upper = 1, method = "L-BFGS-B")$par
L
make_L
L <- make_L(x)
optim(runif(1), L, lower = 0, upper = 1, method = "L-BFGS-B")$par
optim(runif(1), L, lower = 0, upper = 1, method = "L-BFGS-B")$par
L
environment(L)
objects(environment(L))
environment(L)$s
environment(L)$n
environment(L)$x
plot(L, 0, 1)
L(0.1)
L(seq(0.1, 1, by = 0.1))
L
optim
optim
hist
methods(hist)
hist.Date
graphics:::hist.Date
hist
methods(hist)
hist.default
methods(hist)
q()
