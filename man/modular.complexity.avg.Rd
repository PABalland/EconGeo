% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modular.complexity.avg.r
\name{modular.complexity.avg}
\alias{modular.complexity.avg}
\title{Compute a measure of average modular complexity of technologies}
\usage{
modular.complexity.avg(mat, sparse = FALSE, list = FALSE)
}
\arguments{
\item{mat}{A bipartite adjacency matrix (can be a sparse matrix)}

\item{sparse}{Logical; is the input matrix a sparse matrix? Defaults to FALSE, but can be set to TRUE if the input matrix is a sparse matrix}

\item{list}{Logical; is the input a list? Defaults to FALSE (input = adjacency matrix), but can be set to TRUE if the input is an edge list}
}
\description{
This function computes a measure of average modular complexity of technologies (average complexity of patent documents in a given technological class) from technological classes - patents (incidence) matrices
}
\examples{
## generate a technology - patent matrix
set.seed(31)
mat <- matrix(sample(0:1,30,replace=T), ncol = 5)
rownames(mat) <- c ("T1", "T2", "T3", "T4", "T5", "T6")
colnames(mat) <- c ("US1", "US2", "US3", "US4", "US5")

## run the function
modular.complexity.avg (mat)

## generate a technology - patent sparse matrix
library (Matrix)

## run the function
smat <- Matrix(mat,sparse=TRUE)

modular.complexity.avg (smat, sparse = TRUE)
## generate a regular data frame (list)
list <- get.list (mat)

## run the function
modular.complexity.avg (list, list = TRUE)
}
\author{
Pierre-Alexandre Balland \email{p.balland@uu.nl}
}
\references{
Fleming, L. and Sorenson, O. (2001) Technology as a complex adaptive system: evidence from patent data, \emph{Research Policy} \strong{30}: 1019-1039
}
\seealso{
\code{\link{ease.recombination}}, \code{\link{TCI}}, \code{\link{MORt}}
}
\keyword{complexity}

