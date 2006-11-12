`.construct_greedy` <-
function (x, alpha = runif(1, 0, 1)) 
{
    route <- x$start
    city.indices <- c(1:x$n.cities)[-x$start]
    x$alpha <- c(x$alpha, alpha)
    while (length(city.indices) != 0) {
        current.dists <- x$matrix[, route[1]][-route]
        c0 <- min(current.dists)
        c1 <- max(current.dists)
        dummy <- c0 + alpha * (c1 - c0)
        candidates <- which(current.dists <= dummy)
        z <- candidates[order(runif(length(candidates), 0, 1)) == 
            1]
        route <- c(city.indices[z], route)
        city.indices <- city.indices[-z]
    }
    x$random.solution <- append(rev(route), x$start)
    x
}
`.random_start` <-
function (x) 
{
    samp <- sample(1:x$n.cities)
    x$random.solution <- append(c(x$start, samp[-which(samp == 
        x$start)]), x$start)
    x
}
`.objective_function` <-
function(x,route=x$route)
{
  i <- route[-(x$n.cities+1)]
  j <- route[-1]
  sum(x$matrix[(j-1)*nrow(x$matrix) + i])
}
`.opt_move_schlecht` <-
function (x, k) 
{
    random.indices <- sample(2:(length(x) - 1), k)
    indices.to.change <- x[random.indices]
    places.to.change <- random.indices[c(order(runif(k, 0, 1)))]
    while (any(places.to.change == indices.to.change)) {
        places.to.change <- random.indices[c(order(runif(k, 0, 
            1)))]
    }
    new.route <- replace(x, places.to.change, indices.to.change)
    new.route
}
`.k_opt_move` <-
function (x, k) 
{
    random.indices <- sample(2:(length(x) - 1), k)
    indices.to.change <- x[random.indices]
    new.route <- replace(x, random.indices[c(2:k, 1)], indices.to.change)
    new.route
}
`.three_opt_move` <-
function (x) 
{
    random.indices <- sample(2:(length(x) - 1), 3)
    indices.to.change <- x[random.indices]
    new.route <- replace(x, random.indices[c(2, 3, 1)], indices.to.change)
    new.route
}
`.tsp_from_matrix` <-
function (x) 
{
    tsp <- NULL
    if (all(x[row(x) > col(x)] == 0)) 
        tsp$matrix <- x + t(x)
    else {
        tsp$matrix <- x
    }
    tsp$n.cities <- ncol(tsp$matrix)
    tsp$city.names <- colnames(x)
    class(tsp) <- "tsproblem"
    tsp
}
