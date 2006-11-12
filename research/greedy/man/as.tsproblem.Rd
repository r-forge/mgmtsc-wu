\name{as.tsproblem}
\alias{as.tsproblem}
\alias{as.tsproblem.matrix}
\title{Prepare a distance matrix for solving a
  Travelling-Salesman-problem using GRASP}
\description{
  The function provides a spezial class for solving a
  Travelling-Salesman-problem when using the function \code{grasp.solve()}
}
\usage{
as.tsproblem(x)
}
\arguments{
  \item{x}{\code{x} is matrix containing distances between the
    cities. Also see \code{help.search(grasp.search)}}}
\details{
  This function is automatically used when \code{grasp.solve()} is used.
}
\value{
A short summary about the distance matrix is returned. You can use the
attributes \$matrix, \$n.cities, \$city.names.
}
\references{coming soon ...}
\author{Martin Gartner, Gerold Koeb, Goran Lovric, Stefan Theussl}
\seealso{See also \code{grasp.search()} and \code{local_search()}. }
\examples{
data(dist)
a <- as.tsproblem(dist)
a $n.cities
a $city.names
a $matrix

## The function is currently defined as
function (x) 
UseMethod("as.tsproblem")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
