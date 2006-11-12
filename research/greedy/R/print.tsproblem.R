`print.tsproblem` <-
function (x, ...) 
{
    if (!inherits(x, "tsproblem")) 
        stop("'x' must inherit from class \"tsp\"")
    cat("A travelling salesman problem with", x$n.cities, "cities\n")
    cat("Minimum distance found so far:", x$optimum, "\n")
    invisible(x)
}
