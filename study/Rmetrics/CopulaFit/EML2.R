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
  #browser()
  liste <- makelist()
  ## Grenzen fuer Parameter der Verteilungen
  lowerbounds <- list(
    c(-1,0),
    c(1),
    c(alpha = 0.0001, beta = 1, delta = 0.0001, mu = -Inf, lambda = -Inf),  ## eigentlich: abs(beta) <= alpha
    c(alpha = -Inf, beta = -Inf, delta = -Inf, mu = Inf),
    c(alpha = 0.5, beta = 0 , delta = 0.0001, mu = Inf)                   ## eigentlich: abs(beta) <= alpha   
  )

  upperbounds <- list(
                      c(1, 5),
                      c(Inf),
                      c(alpha = Inf, beta = 1, delta = Inf, mu = Inf, lambda = Inf),## eigentlich: abs(beta) <= alpha
                      c(alpha = Inf, beta = Inf, delta = Inf, mu = Inf),
                      c(alpha = Inf, beta = 0.5, delta = Inf, mu = Inf))           ## eigentlich: abs(beta) <= alpha


  ## zunächst fuer alle archmCopulae
  fit <- NULL
  t <- c(1, 3:6, 9:10, 12:14, 16:17)  ## 19 und 20 funktionieren auch nicht

  dichte_Randverteilung <- c("dnorm", "dt", "dgh", "dhyp", "dnig")
  p_Randverteilung <- c("pnorm", "pt", "pgh", "phyp", "pnig")

  startwerte <- list(
                     c(0.5, 1),
                     c(4),
                     c(1, 0, 1, 0, 1),
                     c(1, 0, 1, 0),
                     c(1, 0, 1, 0))
  
  fit <- NULL  
  for(n in 1){  ## in t ...  eigentlich von 1:22

    type <- n
    alpha = .archmParam(type)$param
    range <- .archmRange(type)

    for(i in 1){
    #for(i in c(1,2,6,7)){
    #for(i in 1:length(liste)){      
      fun <- function(star, type, ...){
        alpha <- star[[1]]
        star <- star[-1]

        star2 <- c(list(x), star[(1:length(startwerte[[ liste[[i]][1] ]]) )]  )  ##star[(1:length(liste[[i]]))]
        star <- star[-(1:length(startwerte[[ liste[[i]][1] ]]))]   ##liste[[i]]))]
        star3 <- c(list(y), star)

        (-mean(log(.darchm2Copula(
               do.call(p_Randverteilung[liste[[i]][1]], star2) ,  ## u
               do.call(p_Randverteilung[liste[[i]][2]], star3),   ## v
               alpha = alpha,  ## = x,
               type = type)))
         - mean(log(do.call(dichte_Randverteilung[liste[[i]][1]], star2)))
         - mean(log(do.call(dichte_Randverteilung[liste[[i]][2]], star3)))
         )
      }
            
      z = nlminb(start = c(alpha, startwerte[[liste[[i]][1] ]], startwerte[[liste[[i]][2] ]]),
        objective = fun, lower = c(range[1], lowerbounds[[liste[[i]][1]]], lowerbounds[[liste[[i]][2]]]),
        upper = c(range[2], upperbounds[[liste[[i]][1]]], upperbounds[[liste[[i]][2]]]),
        type = type, control = list())
      
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









  #param_Randverteilung <- list(
  #   list(x, mean = 0, sd = 1),
  #   list(x, df = 4),
  #   list(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1),
  #   list(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4)),
  #   list(x, alpha = 1, beta = 0, delta = 1, mu = 0))


pgh <- 
function (q, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1) 
{
  q[q == -Inf] <- -10000
    if (alpha <= 0) 
        stop("alpha must be greater than zero")
    if (delta <= 0) 
        stop("delta must be greater than zero")
    if (abs(beta) >= alpha) 
        stop("abs value of beta must be less than alpha")
    ans = NULL
    for (Q in q) {
        Integral = integrate(dgh, -Inf, Q, stop.on.error = FALSE, 
            alpha = alpha, beta = beta, delta = delta, mu = mu, 
            lambda = lambda)
        ans = c(ans, as.numeric(unlist(Integral)[1]))
    }
    ans
}
