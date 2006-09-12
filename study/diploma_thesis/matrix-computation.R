## chapter: parallel matrix multiplication
## Stefan Theussl, 2006

set.seed(170782)
## quadratic matrices
nrows <- 10000

A <- matrix(runif(nrows^2),nrow=nrows)
B <- matrix(runif(nrows^2),nrow=nrows)

C <- A%*%B
