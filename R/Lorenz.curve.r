#' Plot a Lorenz curve from regional industrial counts
#'
#' This function plots a Lorenz curve from regional industrial counts. This curve gives an indication of the unequal distribution of an industry accross regions.
#' @param ind A vector of industrial regional count
#' @keywords inequality concentration
#' @export
#' @examples
#' ## generate vectors of industrial count
#' ind <- c(0, 10, 10, 30, 50)
#'
#' ## run the function
#' Lorenz.curve (ind)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{Hoover.curve}}, \code{\link{Gini}}
#' @references Lorenz, M. O. (1905) Methods of measuring the concentration of wealth, \emph{Publications of the American Statistical Association} \strong{9}: 209–219

Lorenz.curve <- function(ind) {
  x <- ind
  weights = rep(1, length = length(x))
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  p <- c(0,p)
  nu <- c(0,nu)
  plot (p, nu, type = "l", main = "Lorenz curve",
        xlab="Cumulative distribution of population shares", ylab="Cumulative distribution of industry shares",
        xlim=c(0, 1), ylim=c(0, 1))
  return(abline (0,1, col = "red"))

}
