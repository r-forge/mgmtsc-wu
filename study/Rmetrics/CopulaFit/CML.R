## Schätzung nach der CML-Methode

## Es wird eine Beschränkung von theta benötigt, siehe Nelsen. Dazu
## hat Prof. Wuertz die Funktion .archmRange(##type) geschrieben.

.CMLfit <- function(x, y, ...){
  ## zunächst fuer alle archmCopulae
  u <- pemp(x, x)
  v <- pemp(y, y)
  fit <- NULL
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren
  # auch nicht
  for(i in t){  ## eigentlich von 1:22

    type <- i
    alpha = .archmParam(type)$param
    range <- .archmRange(type)
        
    fun = function(x, type) {
      -mean(log(darchmCopula(u = u, v = v, alpha = x, type = type,
    alternative = TRUE)))
    }
  
    y = nlminb(start = alpha, objective = fun, lower = range[1],
      upper = range[2], type = type, ...)

    fit $family <- c(fit $family, paste("Archimedian Type", type))
    fit $par <- c(fit $par, y $par)
    fit $objective <- c(fit $objective, y $objective)
    fit $convergence <- c(fit $convergence, y $convergence)
    fit $message <- c(fit $message, y $message)
    fit $iterations <- c(fit $iterations, y $iterations)
    ## fit $evaluations[[i]] <- y $evaluations   ????
    fit $method <- c(fit $method, "CML")
  }
  fit
    
}







## Implementation der empirischen Verteilungsfunktion

## x ... eine Datenreihe
## t ... zu untersuchender Wert der Datenreihe x
  
pemp <- function(t, x){
  F <- NULL
  for(i in 1:length(t)){
    F <- c(F, 1 / length(x) * sum(x <= t[i]))
  }
  F
}

## Beispiel fuer pemp:
## x <- rnorm(100)
## pemp(x[1], x)
## pemp(sort(x)[1], x) ## muss 1/n ergeben
