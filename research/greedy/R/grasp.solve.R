`grasp.solve` <-
function (x, start, iterations = 100, n = 1000, alpha = "2", 
    k = 3, ...) 
{
    if (k <= 1) 
        stop("k must be an integer between 2 and the number of the cities!!!")
    tsp <- as.tsproblem(x)
    tsp$optimum <- sum(x)
    tsp$start <- start
    for (i in 1:iterations) {
        tsp <- switch(alpha, "0" = .random_start(tsp), "1" = .construct_greedy(tsp, 
            alpha = 0), "2" = .construct_greedy(tsp, alpha = runif(1, 
            0, 1)), "3" = .construct_greedy(tsp, alpha = rgamma(1, 
            1, 6)), "4" = .construct_greedy(tsp, alpha = runif(1, 
            0, 0.2)), "5" = .construct_greedy(tsp, alpha = runif(1, 
            0.05, 0.06)))
        new.route <- local_search(tsp, tsp$random.solution, n, 
            k)
        optimum <- .objective_function(tsp, new.route)
        tsp$solutions <- c(tsp$solutions, optimum)
        plot(tsp$solutions, type = "l")
        if (tsp$optimum > optimum) {
            tsp$optimum <- optimum
            tsp$optimum_vector <- c(tsp$optimum_vector, tsp$optimum)
            tsp$route <- new.route
        }
    }
    tsp
}
