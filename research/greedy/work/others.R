
## Also hier mal die vollständige Distanzmatrix: ACHTUNG: Das file ("dist.csv") liegt derzeit noch im gleichen Ordner wie "GRASP.R"!

dist <- read.csv("dist.csv", header = TRUE, sep = ";")
dist <- dist + t(dist)
z <- as.matrix(dist)


# ineffiziente Funktionen

objective_function <- function(T) {
  distances <- NULL
  while(length(T) > 1) {
    i <- T[1]
    j <- T[2]
    distances <- c(distances, dist[[i, j]])
    T <- T[-1]
  }
  sum(distances)
}

objective_function2 <- function(T) {
  x <- T[-length(T)]
  y <- T[-1]
  sum(diag(z[x[1:length(x)], y[1:length(y)]]))
}


## eine funktionierende aber ineffiziente lösung:

grasp <- function(iterations = 1000) {                                ## # of iterations (as stop criterion)
  optimal_value <- 1000000000000000                            ## a very high number
  i <- 1
  while(i < iterations) {
    T <- .construct_greedy()
    # T <- foo.min(dist)
    T <- local_search(T)
    objective_value <- objective_function(T)
    
    if(objective_value < optimal_value) {
      optimal_value <- objective_value
      optimal_route <- T
    }
    else {
      not_optimal_route <- T                                   ## diese Routen sollten wir mit einer hash-tabelle
                                                               ## verknuepfen, damit wir nicht dieselben routen
                                                               ## nochmals durchlaufen lassen
    }
    i <- i + 1
  }
  #c(names[optimal_route], optimal_value)			#wie man sieht kommen manche Städte doppelt vor...
  #optimal_value
  list(names[optimal_route], optimal_value)
}


foo.min <- function(dist) {
  T <- 29
  i <- 0
  while(i < 29) {

  min <- min(dist[, T[1]][-T])
  T <- c(which(dist[, T[1]] == min), T)
  i <- i + 1
}
rev(T)
}
