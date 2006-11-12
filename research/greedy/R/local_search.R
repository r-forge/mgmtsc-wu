`local_search` <-
function (x, route, n, k) 
{
    start_value <- .objective_function(x, route)
    new.route <- .k_opt_move(route, k)
    new_value <- .objective_function(x, new.route)
    i <- 0
    while ((start_value < new_value) && (i <= n)) {
        new.route <- .k_opt_move(route, k)
        new_value <- .objective_function(x, new.route)
        i <- i + 1
    }
    new.route
}
