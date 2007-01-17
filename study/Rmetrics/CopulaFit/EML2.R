    drand <- function(x, a, mean = 0, sd = 1, df = 4, alpha = 1, beta = 0, delta =1, mu = 0, lambda = 1){
      switch(paste(a),
             "1" = dnorm(x, mean, sd),
             "2" = dt(x, df),
             "3" = dgh(x, alpha, beta, delta, mu, lambda),
             "4" = dhyp(x, alpha, beta, delta, mu, pm = c(1, 2, 3, 4)),
             "5" = dnig(x, alpha, beta, delta, mu))
    }

    prand <- function(x, a, mean = 0, sd = 1, df = 4, alpha = 1, beta = 0, delta =1, mu = 0, lambda = 1){
      switch(paste(a),
             "1" = pnorm(x, mean, sd),
             "2" = pt(x, df),
             "3" = pgh(x, alpha, beta, delta, mu, lambda),
             "4" = phyp(x, alpha, beta, delta, mu),
             "5" = pnig(x, alpha, beta, delta, mu))
    }

    param <- function(a){
      switch(paste(a),
             "1" = c(mean = 0, sd = 1),
             "2" = c(df = 4),
             "3" = c(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1),
             "4" = c(alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4)),
             "5" = c(alpha = 1, beta = 0, delta = 1, mu = 0)
             )
    }


makelist <- function(){
  liste <- NULL
  ind <- 0
  for(i in 1:5){   ## 5 ... Anzahl der Funktionen, die in .marginFit sind
    for(j in 1:5){
      ind <- ind + 1
      liste[[ind]] <- c(i, j)
    }
  }
  liste
}


.EMLfit <- function(x, y){
  browser()

  liste <- makelist()
            
  ## zunächst fuer alle archmCopulae
  fit <- NULL
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren
  # auch nicht
  for(i in t){  ## eigentlich von 1:22

    type <- i
    alpha = .archmParam(type)$param
    range <- .archmRange(type)



    c <<- c(mean =0, sd = 1, df = 4, alpha = 1, beta = 0, delta =1, mu = 0, lambda = 1)
    
    for(j in 1:25){
      a <- liste[[j]][1]
      b <- liste[[j]][2]
    
      fun = function(c, type, x, y) {
        -mean(log(darchmCopula(prand(x, a, c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8]), prand(y, b, c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8]), alpha = x, type = type, alternative = TRUE))) - mean(log(drand(x, a, c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8]))) - mean(log(drand(y, b, c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8])))
      }      
      
      y = nlminb(start = c(alpha, c), objective = fun, lower = range[1],
        upper = range[2], type = type, x = x, y = y) #, ...)
      ## fuer start wird wieder switch-funktion benötigt
    

      fit $family <- c(fit $family, paste("Archimedian Type", type))
      fit $par <- c(fit $par, y $par)
      fit $objective <- c(fit $objective, y $objective)
      fit $convergence <- c(fit $convergence, y $convergence)
      fit $message <- c(fit $message, y $message)
      fit $iterations <- c(fit $iterations, y $iterations)
      ## fit $evaluations[[i]] <- y $evaluations   ????
      fit $method <- c(fit $method, "IFM")
    }
  }
  fit

  ## Ueberlegen, wie ausgegeben werden kann, wie Randverteilung aussieht
    
}
