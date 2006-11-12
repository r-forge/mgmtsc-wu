\name{dist}
\alias{dist}
\docType{data}
\title{A matrix containing distances between 30 cities in Europe.}
\description{
  This matrix contains the distances between 30 cities in Europe. It is
  matrix which can be used when calculating the minimal route of a Travelling-Salesman-Problem.
}
\usage{data(dist)}
\format{
  The format is:
 int [1:30, 1:30] 0 0 0 0 0 0 0 0 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:30] "1" "2" "3" "4" ...
  ..$ : chr [1:30] "Amsterdam" "Athen" "Bergen" "Berlin" ...
}

\examples{
data(dist)
}
\keyword{datasets}
