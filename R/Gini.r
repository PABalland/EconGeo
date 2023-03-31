#' Compute the Gini coefficient
#'
#' This function computes the Gini coefficient. The Gini index measures spatial inequality. It ranges from 0 (perfect income equality) to 1 (perfect income inequality) and is derived from the Lorenz curve. The Gini coefficient is defined as a ratio of two surfaces derived from the Lorenz curve. The numerator is given by the area between the Lorenz curve of the distribution and the uniform distribution line (45 degrees line). The denominator is the area under the uniform distribution line (the lower triangle). This index gives an indication of the unequal distribution of an industry accross n regions. Maximum inequality in the sample occurs when n-1 regions have a score of zero and one region has a positive score. The maximum value of the Gini coefficient is (n-1)/n and approaches 1 (theoretical maximum limit) as the number of observations (regions) increases.
#' @param mat A region-industry count matrix
#' @keywords inequality concentration
#' @export
#' @examples
#' ## generate vectors of industrial count
#' ind <- c(0, 10, 10, 30, 50)
#'
#' ## run the function
#' gini(ind)
#'
#' ## generate a region - industry matrix
#' mat <- matrix(
#'   c(
#'     0, 1, 0, 0,
#'     0, 1, 0, 0,
#'     0, 1, 0, 0,
#'     0, 1, 0, 1,
#'     0, 1, 1, 1
#'   ),
#'   ncol = 4, byrow = TRUE
#' )
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' gini(mat)
#'
#' ## run the function by aggregating all industries
#' gini(rowSums(mat))
#'
#' ## run the function for industry #1 only (perfect equality)
#' gini(mat[, 1])
#'
#' ## run the function for industry #2 only (perfect equality)
#' gini(mat[, 2])
#'
#' ## run the function for industry #3 only (perfect unequality: max gini = (5-1)/5)
#' gini(mat[, 3])
#'
#' ## run the function for industry #4 only (top 40% produces 100% of the output)
#' gini(mat[, 4])
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{hoover_gini}}, \code{\link{locational_gini}}, \code{\link{locational_gini_curve}}, \code{\link{lorenz_curve}}, \code{\link{hoover_curve}}
#' @references Gini, C. (1921) Measurement of Inequality of Incomes, \emph{The Economic Journal} \strong{31}: 124-126

gini <- function(mat) {
  mat <- as.matrix(mat)

  gi <- function(mat, col = 1) {
    x <- mat[, col]
    x <- x[complete.cases(x)]
    weights <- rep(1, length = length(x))
    ox <- order(x)
    x <- x[ox]
    weights <- weights[ox] / sum(weights)
    p <- cumsum(weights)
    nu <- cumsum(weights * x)
    n <- length(nu)
    nu <- nu / nu[n]
    round(sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1]), 3)
  }

  if (ncol(mat) == 1) {
    x <- gi(mat)
  } else {
    y <- NULL
    # for (i in unique(1:ncol(mat))) {
    for (i in seq_len(ncol(mat))) {
      tmp <- gi(mat, i)
      y <- rbind(y, tmp)
    }

    x <- data.frame(colnames(mat), y[, 1])
    colnames(x) <- c("Industry", "Gini")
  }

  return(x)
}
