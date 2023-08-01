#' Compute the Hoover Gini
#'
#' This function computes the Hoover Gini, named after Hedgar hoover_ The Hoover index is a measure of spatial inequality. It ranges from 0 (perfect equality) to 1 (perfect inequality) and is calculated from the Hoover curve associated with a given distribution of population, industries or technologies and a reference category. In this sense, it is closely related to the Gini coefficient and the Hoover index. The numerator is given by the area between the Hoover curve of the distribution and the uniform distribution line (45 degrees line). The denominator is the area under the uniform distribution line (the lower triangle).
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pop A vector of population regional count
#' @return The Hoover Gini value(s). If the input matrix has a single column, the function returns a numeric value representing the Hoover Gini index. If the input matrix has multiple columns, the function returns a data frame with two columns: "Industry" (names of the industries) and "hoover_gini" (corresponding Hoover Gini values).
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(10, 15, 20, 25, 30)
#'
#' ## run the function (30% of the population produces 50% of the industrial output)
#' hoover_gini(ind, pop)
#'
#' ## generate a region - industry matrix
#' mat <- matrix(
#'   c(
#'     0, 10, 0, 0,
#'     0, 15, 0, 0,
#'     0, 20, 0, 0,
#'     0, 25, 0, 1,
#'     0, 30, 1, 1
#'   ),
#'   ncol = 4, byrow = TRUE
#' )
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' hoover_gini(mat, pop)
#'
#' ## run the function by aggregating all industries
#' hoover_gini(rowSums(mat), pop)
#'
#' ## run the function for industry #1 only
#' hoover_gini(mat[, 1], pop)
#'
#' ## run the function for industry #2 only (perfectly proportional to population)
#' hoover_gini(mat[, 2], pop)
#'
#' ## run the function for industry #3 only (30% of the pop. produces 100% of the output)
#' hoover_gini(mat[, 3], pop)
#'
#' ## run the function for industry #4 only (55% of the pop. produces 100% of the output)
#' hoover_gini(mat[, 4], pop)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{hoover_curve}}, \code{\link{locational_gini}}, \code{\link{locational_gini_curve}}, \code{\link{lorenz_curve}}, \code{\link{gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171


hoover_gini <- function(mat, pop) {
  mat <- as.matrix(mat)

  hg <- function(mat, pop, col = 1) {
    ind <- c(0, mat[, col])
    pop <- c(0, pop)
    c <- data.frame(ind, pop)
    c <- c[complete.cases(c), ]
    ind <- c$ind
    pop <- c$pop
    o <- ind / pop
    o[is.na(o)] <- 0
    oind <- order(o)
    ind <- ind[oind]
    pop <- pop[oind]
    cind <- cumsum(ind) / max(cumsum(ind))
    cpop <- cumsum(pop) / max(cumsum(pop))
    nn <- length(cind)
    round(sum(cind[-1] * cpop[-nn]) - sum(cind[-nn] * cpop[-1]), 3)
  }

  if (ncol(mat) == 1) {
    x <- hg(mat, pop)
  } else {
    y <- NULL
    for (i in seq_len(ncol(mat))) {
      tmp <- hg(mat, pop, i)
      y <- rbind(y, tmp)
    }

    x <- data.frame(colnames(mat), y[, 1])
    colnames(x) <- c("Industry", "hoover_gini")
  }

  return(x)
}
