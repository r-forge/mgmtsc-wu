\name{GRASP-package}
\alias{GRASP-package}
\alias{GRASP}
\docType{package}
\title{Heuristic Algortithm for solving a Travelling-Salesman-Problem or similiar problems}
\description{
GRASP stands for Greedy Randomized Adaptive Search Procedures. GRASP is
a heuristic approach for solving a Travelling-Salesman-Problem where you
try to minimize the objective function.
}
\details{
  Input is a data matrix which includes the distances (or the cost of
  travelling) from one city to another. The data don't have to be a
  symmetric matrix. This makes the algorithm very flexible so that it
  easily can be adapted to other problems.
  For further details see \code{help.search("grasp.solve")}
\tabular{ll}{
Package: \tab GRASP\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2006-10-18\cr
License: \tab GPL 2.1 or newer\cr
}
}
\author{Martin Gartner, Gerold Koeb, Goran Lovric, Stefan Theussl 
Maintainer: Stefan Theussl <stefan.theussl@wu-wien.ac.at>
}
\references{fehlt noch!!!
}
\keyword{manip}

\seealso{\code{\link{grasp.solve} for using the heuristic.}
  \code{\link{local_search} for searching local optima.}
  \code{\link{plot.tsproblem} for plotting the calculated route.}
}
\examples{
data(dist)
sol <- grasp.solve(dist, 29)
sol
}
