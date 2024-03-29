## Sch�tzung nach IFM-Methode


## Sch�tzung der Randverteilungen (DistributionsFit)
.marginFit <- function(x){
  # browser()
  ergebnis <- NULL
  ergebnis $estimate[[1]] <- c(mean(x), sd(x))
  ergebnis $minimum[[1]] <- sum(log(dnorm(x, mean(x),sd(x))))
  ergebnis $distr[[1]] <- "Normal Distribution"
  ergebnis $AIC[[1]] <- -2*(ergebnis $minimum[[1]]) + 2*2
  ## Formatierung, 1. Element = mean, 2. = sd
  for(i in c(2:5)){ 
    fit <- switch(paste(i),
                  "2" = tFit(x, doplot = FALSE, trace = 0),
                  "3" = ghFit(x, doplot = FALSE, trace = 0),
                  "4" = hypFit(x, doplot = FALSE, trace = 0),
                  "5" = nigFit(x, doplot = FALSE, trace = 0),
                  "6" = stableFit(x, doplot = FALSE, trace = FALSE))
    ergebnis $estimate[[i]] <- fit @fit$estimate
    ergebnis $minimum[[i]] <- fit @fit$minimum
    ergebnis $distr[[i]] <- fit @title
    ergebnis $AIC[[i]] <- 2*(fit @fit$minimum) + 2*length(fit @fit$estimate)
  }
  # ind <- which(ergebnis $minimum == min(ergebnis $minimum))
  ind <- which(ergebnis $AIC == min(ergebnis $AIC))
  best <- NULL
  best $AIC <- ergebnis $AIC[[ind]]
  best $estimate <- ergebnis $estimate[[ind]]
  best $minimum <- ergebnis $minimum[[ind]]
  best $distr <- ergebnis $distr[[ind]]
  best
}


.pmarginFit <- function(x, a) {
  switch(a $distr,
              "Normal Distribution" = pnorm(x, a $estimate[1], a
              $estimate[2]),
              "Student-t Parameter Estimation" = pt(x, a
              $estimate[1]),
              "Generalized Hyperbolic Parameter Estimation" = pgh(x,
              a $estimate[1], a $estimate[2], a $estimate[3], a
              $estimate[4], a $estimate[5]),
              "Hyperbolic Parameter Estimation" = phyp(x, a
              $estimate[1], a $estimate[2], a $estimate[3], a
              $estimate[4]),
              "Normal Inverse Gaussian Parameter Estimation" = pnig(x,
              a $estimate[1], a $estimate[2], a $estimate[3], a
              $estimate[4]))
}




## Sch�tzung der Copula

.IFMfit <- function(x, y){
  # browser()
  a <- .marginFit(x)
  b <- .marginFit(y)
  
  u <- .pmarginFit(x, a)
  v <- .pmarginFit(y, b)
             
  ## zun�chst fuer alle archmCopulae
  fit <- NULL
  fit $margin1 <- a $estimate
  fit $distrmargin1 <- a $distr
  fit $margin2 <- b $estimate
  fit $distrmargin2 <- b $distr
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren
  # auch nicht
  ind <- 1
  for(i in t){  ## eigentlich von 1:22

    type <- i
    alpha = .archmParam(type)$param
    range <- .archmRange(type)
        
    fun = function(x, type) {
      erg <- -mean(log(darchmCopula(u = u, v = v, alpha = x, type = type,
    alternative = TRUE)))
      if(erg == "NaN") {erg <- 10}
      else erg
    }
  
    y = nlminb(start = alpha, objective = fun, lower = range[1],
      upper = range[2], type = type) #, ...)

    fit $family <- c(fit $family, paste("Archm.", type))
    fit $par <- c(fit $par, y $par)
    fit $objective <- c(fit $objective, y $objective)
    fit $AIC <- c(fit $AIC, 2*(fit $objective)[ind] + 2*1)
    fit $convergence <- c(fit $convergence, y $convergence)
    fit $message <- c(fit $message, y $message)
    fit $iterations <- c(fit $iterations, y $iterations)
    ## fit $evaluations[[i]] <- y $evaluations   ????
    fit $method <- c(fit $method, "IFM")
    ind <- ind+1
  }
  fit

  ## Ueberlegen, wie ausgegeben werden kann, wie Randverteilung aussieht
    
}





## Bsp:

#library(tseries)
#x <- get.hist.quote("^atx", quote = "Close")
#y <- get.hist.quote("^n225", quote = "Close")
#length(y) <- length(x)
#x <- as.timeSeries(x)
#y <- as.timeSeries(y)
#x <- as.vector(returnSeries(x))
#y <- as.vector(returnSeries(y))
#y <- y[1:length(x)]

#a <- .IFMfit(x, y)
