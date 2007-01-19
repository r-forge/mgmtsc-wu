rm(list = ls())
library(tseries)
library(fSeries)
#source("IFM.R")
#source("CML.R")
#source("EML2.R")
#source("fCopulae.R")

source(file = "C:/Dokumente und Einstellungen/Home/Desktop/cvs/study/Rmetrics/CopulaFit/IFM.R")
source(file = "C:/Dokumente und Einstellungen/Home/Desktop/cvs/study/Rmetrics/CopulaFit/CML.R")
source(file = "C:/Dokumente und Einstellungen/Home/Desktop/cvs/study/Rmetrics/CopulaFit/EML2.R")
source(file = "C:/Dokumente und Einstellungen/Home/Desktop/cvs/study/Rmetrics/CopulaFit/fCopulae.R")


CopulaFit <- function(x, y, method = "CML", returns = FALSE, ...){
  UseMethod("CopulaFit")
}

## method ... IFM, CML oder EML
## returns ... sind x und y datenpunkte oder schon renditen
## ml ... klasse fuer CopulaFit


CopulaFit.default <- function(x, y, method, returns, ...) {
  if(length(x) != length(y)) {
    stop("x and y must be of same length!")
  }
  as.ml(x, returns)
  as.ml(y, returns)
  CopulaFit.ml(x, y, method, ...)
}

as.ml <- function(x, returns = FALSE){
  library(fSeries)
  if(returns == FALSE){
    x <- as.timeSeries(x)
    x <- returnSeries(x)
    x <- x@Data
  }
  class(x) <- "ml"
  x
}



CopulaFit.ml <- function(x, y, method, ...) {
  #browser()
  x <- unclass(x)
  y <- unclass(y)
  if(method != "CML" && method != "IFM" && method != "EML" && method != "all"){
    stop("Invalid method: Please choose 'CML', 'IFM', 'EML' or 'all'!")
  }  
  fit <- switch(method,
                "CML" = .CMLfit(x, y),
                "IFM" = .IFMfit(x, y),
                "EML" = .EMLfit(x, y),
                "all" = c(.CMLfit(x, y), .IFMfit(x,y)))#, .EMLfit(x,y))
  
  #if(method == "all"){
  #  fit <- NULL
  #  fit <- .CMLfit(x, y)
  #  fit <- c(fit, .IFMfit(x, y))
  #  fit <- c(fit, .EMLfit(x, y))
  #}
  #else stop("Invalid method: Please choose 'CML', 'IFM', 'EML' or 'all'!")
  class(fit) <- "mloutput"
  fit
}

print.ml <- function(x, ...){
  cat("This is a time series containing ", length(x),
  "returns.\nPrepared for calling the function \"CopulaFit\" \n")
  invisible(x)

}

print.mloutput <- function(fit, iterations = FALSE, convergence =
  FALSE, message = FALSE, ...) {
  if(is.list(fit $par)){
    FIT <- data.frame(fit $family, fit $parCopula, fit $objective, fit
  $method, fit $convergence, fit $AIC)
    if(iterations) FIT <- data.frame(FIT, fit $iterations)
    if(convergence) FIT <- data.frame(FIT, fit $convergence)
    if(message) FIT <- data.frame(FIT, fit $message)
  }
  else {
    FIT <- data.frame(fit $family, fit $par, fit $objective, fit
  $method, fit $convergence, fit $AIC)
    if(iterations) FIT <- data.frame(FIT, fit $iterations)
    if(convergence) FIT <- data.frame(FIT, fit $convergence)
    if(message) FIT <- data.frame(FIT, fit $message)
  }
  print(FIT)
  
  ## suche nach wert in fit, wo objective minimal ist
  ## ausgabe von fit $family, fit $par
}

summary.mloutput <- function(a, ...){
  ind <- which(a $AIC == min(a $AIC, na.rm = TRUE))  
  if(is.list(a $par)){
    cat(" Die best-fit Copula ist Typ: ", a $family[ind], "\n",
        "Minimaler Zielfunktionswert: ", a $objective[ind], "\n\nSummary:\n")
    A <- data.frame(a $family[ind], a $objective[ind], a $method[ind], a
                    $parCopula[ind], a$AIC[ind])    
  }
  else {
    
    cat(" Die best-fit Copula ist Typ: ", a $family[ind], "\n",
        "Minimaler Zielfunktionswert: ", a $objective[ind], "\n\nSummary:\n")
    A <- data.frame(a $family[ind], a $objective[ind], a $method[ind], a
                    $par[ind], a $AIC[ind])
  }
  A
  # invisible(ind)
}
## wähle nach minimaler AIC



## Beispiel:
# load(file = "x.Rda")
# load(file = "y.Rda")

# oder:

# x <- rnorm(100, 760, 23)
# y <- rnorm(100, 500, 99)
# x <- as.ml(x, returns = FALSE)
# y <- as.ml(y, returns = FALSE)

# a <- CopulaFit(x, y, method = "CML", returns = TRUE)
# b <- CopulaFit(x, y, method = "IFM", returns = TRUE)
# c <- CopulaFit(x, y, method = "EML", returns = TRUE)
