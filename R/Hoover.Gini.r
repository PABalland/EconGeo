#' Compute the Hoover Gini from regional counts in population and industry
#'
#' This function computes the Hoover Gini from regional shares in population and industry
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pop A vector of population regional count
#' @param pdf Logical; shall a pdf be saved to your current working directory? Defaults to FALSE. If set to TRUE, a pdf with all Hoover Ginis will be compiled and saved to your current working directory.
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(10, 15, 20, 25, 30)
#'
#' ## run the function (30% of the population produces 50% of the industrial output)
#' Hoover.Gini (ind, pop)
#'
#' ## generate a region - industry matrix
#' mat = matrix (
#' c (0, 10, 0, 0,
#' 0, 15, 0, 0,
#' 0, 20, 0, 0,
#' 0, 25, 0, 1,
#' 0, 30, 1, 1), ncol = 4, byrow = T)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' Hoover.Gini (mat, pop)
#'
#' ## run the function by aggregating all industries
#' Hoover.Gini (rowSums(mat), pop)
#'
#' ## run the function for industry #1 only
#' Hoover.Gini (mat[,1], pop)
#'
#' ## run the function for industry #2 only (perfectly proportional to population)
#' Hoover.Gini (mat[,2], pop)
#'
#' ## run the function for industry #3 only (30% of the pop. produces 100% of the output)
#' Hoover.Gini (mat[,3], pop)
#'
#' ## run the function for industry #4 only (55% of the pop. produces 100% of the output)
#' Hoover.Gini (mat[,4], pop)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{Hoover.curve}}, \code{\link{locational.Gini}}, \code{\link{locational.Gini.curve}}, \code{\link{Lorenz.curve}}, \code{\link{Gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171


Hoover.Gini <- function (mat, pop) {

  mat = as.matrix (mat)

  HG <- function(mat, pop, col = 1) {

    ind <- c(0, mat[,col])
    pop <- c(0, pop)
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

  if (ncol(mat) == 1) {
    x = HG (mat, pop)
  } else {

  y  <- NULL;
  for (i in unique(1:ncol(mat)))
  {
    tmp <- HG (mat, pop, i)
    y <- rbind(y, tmp)
  }

  x = data.frame (colnames (mat),  y[,1])
  colnames (x) = c ("Industry", "Hoover.Gini")

  }

  return (x)

}



