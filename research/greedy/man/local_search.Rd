\name{local_search}
\alias{local_search}
\title{Starting a local-search-heuristic for a minimizing problem}
\description{
Local Search is a heuristic approach for solving hard problems. This
special algorithm is a function which was developped for the function
\code{grasp.solve()}. That's why this special Local Search is a algorithm for solving
Travelling-Salesman-problem.
}
\usage{
local_search(x, route, n, k)
}
\arguments{
  \item{x}{\code{x} is a data matrix containing the distances between
    the cities. Class of \code{x} must be 'tsproblem'. The class is
    automatically generated, if the function \code{grasp.solve()} is
    used. If you want to use local search without GRASP, use
    \code{as.tsproblem()} for assigning the right class to \code{x}. For further details see also \code{help.search(grasp.solve()}}
  \item{route}{\code{route} is a starting route, which will be optimized
  by sampling k cities. When using \code{grasp.solve()} a greedy
  starting route is generated, which will be automatically used here.}
  \item{n}{The local search will continue till a better objective value
    is found. If there's no better value found \code{n} is the stopping
    criterion for the algorithm.}
  \item{k}{\code{k} describes the number of the cities which are sampled
  during the local search.}
}
\details{
It's recommended not to use this function on its own, but only in
combination with the function \code{grasp.solve()}.
}
\value{
The function returns a vector containing the indices of the cities,
which describe the optimal route. Use \code{optimal.route()} for
receiving the city names.
}
\references{coming soon ...}
\author{Martin Gartner, Gerold Koeb, Goran Lovric, Stefan Theussl}
\seealso{See also \code{grasp.solve()}}
\examples{
data(dist)
a <- as.tsproblem(dist)
# local_search(a, 1:31, 10000, 3)


## The function is currently defined as
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
}
\keyword{manip}

