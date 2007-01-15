CopulaFit <- function(x, y, method = "CML", returns = FALSE...){
  UseMethod("ml")
}

## method ... IFM, CML oder EML
## returns ... sind x und y datenpunkte oder schon renditen
## ml ... klasse fuer CopulaFit


CopulaFit.default <- function(x, y, method = "CML", ...) {
  as.ml(x, y, returns)
  CopulaFit.ml(x, y, method, ...)
}

as.ml <- function(x, y, returns){
  if(length(x) != length(y)) {
    stop("x and y must be of same length!")
  }

  ## require(fSeries)

  ## if(returns)
  ## wenn returns = TRUE --> berechnung der renditen
  
}



CopulaFit.ml <- function(x, y, method = "CML", ...) {
  if(method = "CML"){
    fit <- .CMLfit(x, y)
  }
  if(method = "IFM"){
    fit <- .IFMfit(x, y)
  }
  if(method = "EML"){
    fit <- .EMLfit(x, y)
  }
  if(method = "all"){
    fit <- NULL
    fit <- .CMLfit(x, y)
    fit <- c(fit, .IFMfit(x, y))
    fit <- c(fit, .EMLfit(x, y))
  }
  else stop("Invalid method: Please choose 'CML', 'IFM', 'EML' or 'all'!")
  fit
}

print.ml <- function(fit, ...){
  ## suche nach wert in fit, wo objective minimal ist
  ## ausgabe von fit $family, fit $par
}

## summary-funktion fuer summary.ml
