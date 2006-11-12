`plot.tsproblem` <-
function (x, ...) 
{
    coords <- cmdscale(x$matrix)
    plot(-coords[, c(2, 1)], type = "n")
    text(-coords[, c(2, 1)], x$city.names)
    lines(-coords[x$route, c(2, 1)], col = "lightgray")
    mtext(optimal.route(x), side = 3)
}
