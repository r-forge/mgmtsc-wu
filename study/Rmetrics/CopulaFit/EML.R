## Schätzung nach EML-Methode

liste <- NULL
ind <- 0
for(i in 1:5){
  for(j in 1:5){
    ind <- ind + 1
    liste[[ind]] <- c(i, j)
  }
}
    
.marginFitEML <- function(x){
  browser()
  ergebnis <- NULL
  ergebnis $estimate[[1]] <- c(mean(x), sd(x))
  ergebnis $minimum[[1]] <- sum(log(dnorm(x, mean(x),sd(x))))
  ergebnis $distr[[1]] <- "Normal Distribution"
  a[[1]] $estimate  <- ergebnis $estimate[[1]]
  a[[1]] $minimum <- ergebnis $minimum[[1]]
  a[[1]] $distr <- ergebnis $distr[[1]]
  a[[1]] $p <- .pmarginFit(x, a[[1]])
  ## Formatierung, 1. Element = mean, 2. = sd
  for(i in c(2:5)){ 
    fit <- switch(paste(i),
                  "2" = tFit(x, doplot = FALSE, trace = FALSE),
                  "3" = ghFit(x, doplot = FALSE, trace = FALSE),
                  "4" = hypFit(x, doplot = FALSE, trace = FALSE),
                  "5" = nigFit(x, doplot = FALSE, trace = FALSE),
                  "6" = stableFit(x, doplot = FALSE, trace = FALSE))
    ergebnis $estimate[[i]] <- fit @fit$estimate
    ergebnis $minimum[[i]] <- fit @fit$minimum
    ergebnis $distr[[i]] <- fit @title
    a[[i]] $estimate <- ergebnis$estimate[[i]]
    a[[i]] $minimum <- ergebnis$minimum[[i]]
    a[[i]] $distr <- ergebnis$distr[[i]]
    a[[i]] $p <- .pmarginFit(x, a[[i]])
    
  }
  # ergebnis $p[[i]] <- .pmarginFit(x, ergebnis[i])
  a
}




.EMLfit <- function(x, y){
  ## browser()
  liste <- NULL
  ind <- 0

  U <- .marginFitEML(x)
  V <- .marginFitEML(y) 
  
  for(i in 1:5){   ## 5 ... Anzahl der Funktionen, die in .marginFit sind
    for(j in 1:5){
      ind <- ind + 1
      liste[[ind]] <- c(i, j)
    }
  }
  
  ## zunächst fuer alle archmCopulae
  fit <- NULL
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren
  # auch nicht

  for(j in 1:25){
    
  
  for(i in t){  ## eigentlich von 1:22
    cat(paste("Iteration: Type", i, "\n"))

    type <- i
    alpha = .archmParam(type)$param
    range <- .archmRange(type)

        fun = function(x, type) {
      -mean(log(darchmCopula(u = U[[liste[[j]][1] ]]$p, v =
      V[[liste[[j]] [2] ]]$p, alpha = x, type = type, alternative =
      TRUE)))
    }
  
    y = nlminb(start = alpha, objective = fun, lower = range[1],
      upper = range[2], type = type) #, ...)

    fit $family <- c(fit $family, paste("Archimedian Type", type))
    fit $par <- c(fit $par, y $par)
    fit $objective <- c(fit $objective, y $objective)
    fit $convergence <- c(fit $convergence, y $convergence)
    fit $message <- c(fit $message, y $message)
    fit $iterations <- c(fit $iterations, y $iterations)
    ## fit $evaluations[[i]] <- y $evaluations   ????
    fit $method <- c(fit $method, "IFM")
    fit $margin1 <- c(fit $margin1, U[[liste[[j]] [1] ]] $distr)
    fit $margin2 <- c(fit $margin2, V[[liste[[j]] [2] ]] $distr)
    fit $margin1est <- c(fit $margin1est, U[[liste[[j]] [1] ]] $estimate)
    fit $margin2est <- c(fit $margin2est, V[[liste[[j]] [2] ]] $estimate)
  }
}
  fit

  ## Ueberlegen, wie ausgegeben werden kann, wie Randverteilung aussieht
    
}
