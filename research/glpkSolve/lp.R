
is.lp <- function(x) inherits(x, "lp")

as.lp <- function(x) UseMethod("as.lp")

as.lp.list <- function(x){
  .lp_from_list(x)
}

print.lp <- function(x, ...){
  if (!inherits(x, "lp"))
        stop("'x' must inherit from class \"lp\"")
  cat("A linear program with", x$n.obj, "variables\n")
  cat("and", x$n.const, "constraints.\n")
  cat("The objective is to", x$dir, "the problem\n")
  cat("The Optimum is", x$optimum,"\n")
  invisible(x)
}

.lp_from_list <- function(x){
  lp <- x
  lp$n.obj <- length(x$objectives)
  lp$optimum <- NA
  if(is.vector(x$const.matrix)){
    lp$n.const <- 1
    lp$const.mat.i <- rep(1,lp$n.obj)
    lp$const.mat.j <- c(1:lp$n.obj)
    lp$const.mat.v <- lp$const.matrix
  }
  if(is.matrix(x$const.matrix)){
    lp$n.const <- nrow(x$const.matrix)
    # build the matrix (index form)
    lp$const.mat.i <- as.vector(row(x$const.matrix))
    lp$const.mat.j <- as.vector(col(x$const.matrix))
    lp$const.mat.v <- as.vector(x$const.matrix)
    # any elements of the constraint-matrix are 0?
    if(any(lp$const.mat.v==0)){
      w<-which(lp$const.mat.v==0)
      lp$const.mat.i <- lp$const.mat.i[-w]
      lp$const.mat.j <- lp$const.mat.j[-w]
      lp$const.mat.v <- lp$const.mat.v[-w]
    }
  }
  class(lp) <- "lp"
  lp
}
