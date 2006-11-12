\name{grasp.solve}
\alias{grasp.solve}
\title{Use the GRASP heuristic for solving a Travelling-Salesman-Problem}
\description{
  \code{grasp.solve} uses Greedy Randomized Adaptive Searching
  Procedures, i.e. that 'greedy' random start values are generated which
  are the basis for a local search heuristic.
}
\usage{
grasp.solve(x, start, iterations = 100, n = 1000, alpha = "2", k = 3, ...)
}
\arguments{
  \item{x}{\code{x} is the input matrix. Usually it is a matrix
    including the distances between the cities. \code{x} inherits from
    class 'tsproblem'. \code{x} doesn't have to be symmetric, so the
    algorithm can easily adapted to various problems. If the problem is
    symmetric, \code{x} can be a triangular matrix.}
  \item{start}{\code{start} is the index of the city where you want to
    start. The starting city is also the ending city.}
  \item{iterations}{\code{iterations} is the condition for stopping the
    calculation. \code{iterations} is equivalent to the number of local
    searches. See \code{help.search(local_search)}.}
  \item{n}{\code{n} is the condition for stopping the local
    search. Within the local search only n iterations are allowed.}
  \item{alpha}{\code{alpha} is a parameter for evaluating the random
    start solutions. \code{alpha} should be a stochastic variable. The
    following values are available: \code{"0" ... .random_start(tsp)
    "1" ... alpha = 0)
    "2" ... alpha = runif(1,0,1))
    "3" ... alpha = rgamma(1,1,6))
    "4" ... alpha = runif(1,0,0.2))
    "5" ... alpha = runif(1,0.05,0.06))}
    }
  \item{k}{\code{k} specifies the k-opt-move, i.e. the number of cities
    which are exchanged when trying to search a better solution.}
  \item{...}{\code{...}}
}
\value{
  A heuristic approach of the minimal route in the units used in the
  data matrix.
  }
\references{coming soon}
\author{Martin Gartner, Gerold Koeb, Goran Lovric, Stefan Theussl}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(dist)
sol <- grasp.solve(dist, 29, k = 3, alpha = 5, iterations = 100, n =
1000)
sol
sol $route
sol $optimum
sol $matrix
sol $n.cities
sol $city.names[sol $route]
sol $start
sol $alpha
sol $solutions
sol $optimum_vector
sol $random.solution




## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

