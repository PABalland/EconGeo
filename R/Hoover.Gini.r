#' Compute the Hoover Gini from regional counts in population and industry
#'
#' This function computes the Hoover Gini from regional shares in population and industry
#' @param ind A vector of industrial regional count
#' @param pop A vector of population regional count
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(5, 10, 10, 25, 30)
#'
#' ## run the function
#' Hoover.Gini (ind, pop)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location.quotient}}, \code{\link{locational.Gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171

Hoover.Gini <- function(ind, pop) {
  ind <- c(ind)
  pop <- c(pop)
  o = ind/pop
  o[is.na(o)] = 0
  oind <- order(o)
  ind <- ind[oind]
  pop <- pop[oind]
  cind <- cumsum(ind)/max(cumsum(ind))
  cpop <- cumsum(pop)/max(cumsum(pop))
  nn = length (cind)
  sum(cind[-1] * cpop[-nn]) - sum(cind[-nn] * cpop[-1])

}

