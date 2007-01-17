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





## VERSUCH mit do.call()


.EMLfit <- function(x, y){
  # browser()
  liste <- makelist()
            
  ## zunächst fuer alle archmCopulae
  fit <- NULL
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren
  # auch nicht

  dichte_Randverteilung <- c("dnorm", "dt", "dgh", "dhyp", "dnig")
  p_Randverteilung <- c("pnorm", "pt", "pgh", "phyp", "pnig")
  param_Randverteilung <- list(
     list(x, mean = 0, sd = 1),
     list(x, df = 4),
     list(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1),
     list(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4)),
     list(x, alpha = 1, beta = 0, delta = 1, mu = 0))

  startwerte <- list(
                     c(mean = 0.5, sd = 1),
                     c(df = 4),
                     c(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1),
                     c(alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4)),
                     c(alpha = 1, beta = 0, delta = 1, mu = 0))
  
  fit <- NULL
  
  for(n in t){  ## eigentlich von 1:22

    type <- n
    alpha = .archmParam(type)$param
    range <- .archmRange(type)


    for(i in 1:1){
    #for(i in 1:length(liste)){      
      fun <- function(star, type, ...){
        alpha <- star[1]
        star <- star[-1]
        star2 <- list(x, star[(1:length(liste[[i]])    )])
        star <- star[-(1:length(liste[[i]]))]
        star3 <- list(y, star)

        (-mean(log(.darchm2Copula(
               ## do.call(p_Randverteilung[liste[[i]][1]], param_Randverteilung[[liste[[i]][1] ]]),   ## u
               do.call(p_Randverteilung[liste[[i]][1]], star2) ,   ## u
               ## do.call(p_Randverteilung[liste[[i]][2]], param_Randverteilung[[liste[[i]][2] ]]),   ## v
               do.call(p_Randverteilung[liste[[i]][2]], star3),   ## v
               alpha = alpha,  ## = x,
               type = type)))
         #- mean(log(do.call(dichte_Randverteilung[liste[[i]][1]], param_Randverteilung[[liste[[i]][1] ]])))
         - mean(log(do.call(dichte_Randverteilung[liste[[i]][1]], star2)))
         #- mean(log(do.call(dichte_Randverteilung[liste[[i]][2]], param_Randverteilung[[liste[[i]][2] ]])))
         - mean(log(do.call(dichte_Randverteilung[liste[[i]][2]], star3)))
         )
      }
      
      
      z = nlminb(start = c(alpha, startwerte[[liste[[i]][1] ]], startwerte[[liste[[i]][2] ]]),
        objective = fun, lower = range[1], upper = range[2], type = type)
      
      fit $family <- c(fit $family, paste("Archimedian Type", type))
      fit $par <- c(fit $par, list(z $par))
      fit $objective <- c(fit $objective, z $objective)
      fit $convergence <- c(fit $convergence, z $convergence)
      fit $message <- c(fit $message, z $message)
      fit $iterations <- c(fit $iterations, z $iterations)
      ## fit $evaluations[[i]] <- z $evaluations   ????
      fit $method <- c(fit $method, "EML")
    }
    fit
  }
}




## Grenzen fuer Parameter der Verteilungen
lowerbounds <- list(
               c(mean = -Inf, sd = -Inf),
               c(df = 1),
               c(alpha = 0.0001, beta = abs(alpha), delta = 0.0001, mu = -Inf, lambda = -Inf),
               c(alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4)),
               c(alpha = 1, beta = 0, delta = 1, mu = 0))



#             "4" = dhyp(x, alpha, beta, delta, mu, pm = c(1, 2, 3, 4)),
#             "5" = dnig(x, alpha, beta, delta, mu))


do.call(dichte_Randverteilung[1], param_Randverteilung[[1]]) -> b
dnorm(x, 0, 1) == unclass(b)
