#' Compute the Hoover curve from regional counts in population and industry
#'
#' This function computes the Hoover curve (locational Lorenz curve) from regional shares in population and industry 
#' @param ind A vector of industrial regional count 
#' @param pop A vector of population regional count 
#' @keywords location quotient, relative comparative advantage, Hoover.curve
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(5, 10, 10, 25, 30)
#' 
#' ## run the function
#' Hoover.curve (ind, pop)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location.quotient}}, \code{\link{locational.Gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171

Hoover.curve <- function(ind, pop) {
  ind <- c(0, ind)
  pop <- c(0, pop)
  oind <- order(ind)
  ind <- ind[oind]
  pop <- pop[oind]
  cind <- cumsum(ind)/max(cumsum(ind))
  cpop <- cumsum(pop)/max(cumsum(pop))
  plot (cpop, cind, type = "l", main = "Locational Lorenz",
  xlab="Cumulative distribution of population shares", ylab="Cumulative distribution of industry shares",
  xlim=c(0, 1), ylim=c(0, 1)) 
  return(abline (0,1, col = "red"))
}


