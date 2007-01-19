## Schätzung nach der CML-Methode

## Es wird eine Beschränkung von theta benötigt, siehe Nelsen. Dazu
## hat Prof. Wuertz die Funktion .archmRange(##type) geschrieben.

.archmParam <- function (type = 1:22) {
  type = as.integer(type[1])
  B = Inf
  lower = c(-1, 1, -1, 1, -B, 1,  0,  1,  0,  0,   0  , 1, 0,   1   , 1, 0,  -B, 2, 0, 0, 1, 0)
  upper = c( B, B,  1, B,  B, B,  1,  B,  1,  1,   0.5, B, B,   B   , B, B,   B, B, B, B, B, 1)
  Alpha = c( 0, 2,0.5, 1,  1, 1, 0.5, 2, 0.5, 0.5, 0.2, 1, 1,   1   , 2, 1, 0.5, 3, 1, 1, 2, 0.5)
  ans = list(copula = type)
  ans$param = c(alpha = Alpha[type])
  ans$range = c(lower[type], upper[type])
  ans
}


.CMLfit <- function(x, y, ...){
  #browser()
  ## zunächst fuer alle archmCopulae
  u <- pemp(x, x)
  v <- pemp(y, y)
  fit <- NULL
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren
  # auch nicht
  ind <- 1
  for(i in t){  ## eigentlich von 1:22

    type <- i
    alpha = .archmParam(type)$param
    range <- .archmRange(type)
        
    fun = function(x, type) {
      -mean(log(darchmCopula(u = u, v = v, alpha = x, type = type, alternative = TRUE)))
    }
  
    z = nlminb(start = alpha, objective = fun, lower = range[1],
      upper = range[2], type = type) #, ...)

    fit $family <- c(fit $family, paste("Archimedian Type", type))
    fit $par <- c(fit $par, z $par)
    fit $objective <- c(fit $objective, z $objective)
    fit $AIC <- c(fit $AIC, -2*(fit $objective)[ind] + 2*1)
    fit $convergence <- c(fit $convergence, z $convergence)
    fit $message <- c(fit $message, z $message)
    fit $iterations <- c(fit $iterations, z $iterations)
    ## fit $evaluations[[i]] <- z $evaluations   ????
    fit $method <- c(fit $method, "CML")
    ind <- ind + 1
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
