`optimal.route` <-
function (x) 
{
    paste(x$city.names[x$route], collapse = " -> ")
}
