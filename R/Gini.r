#' Compute the Gini coefficient from regional industrial counts 
#'
#' This function computes the Gini coefficient from regional industrial count. This index gives an indication of the unequal distribution of an industry accross regions. 
#' @param ind A vector of industrial regional count 
#' @keywords location quotient, relative comparative advantage, Gini
#' @export
#' @examples
#' ## generate vectors of industrial count
#' ind <- c(0, 10, 10, 30, 50)
#' 
#' ## run the function
#' Gini (ind)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{Hoover.curve}}
#' @references Gini, C. (1921) Measurement of Inequality of Incomes, \emph{The Economic Journal} \strong{31}: 124-126 

Gini <- function(ind) {
    x <- ind
    weights = rep(1, length = length(x))
    ox <- order(x)
    x <- x[ox]
    weights <- weights[ox]/sum(weights)
    p <- cumsum(weights)
    nu <- cumsum(weights * x)
    n <- length(nu)
    nu <- nu/nu[n]
    sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}

