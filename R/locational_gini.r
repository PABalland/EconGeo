#' Compute the locational Gini coefficient from regions - industries matrices
#'
#' This function computes the locational Gini coefficient as proposed by Krugman from regions - industries matrices. The higher the coefficient (theoretical limit = 0.5), the greater the industrial concentration. The locational Gini of an industry that is not localized at all (perfectly spread out) in proportion to overall employment would be 0.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @return A data frame with two columns: "Industry" and "Loc_gini". The "Industry" column contains the names of the industries, and the "Loc_gini" column contains the locational Gini coefficient computed for each industry from the regions - industries matrix.
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate a region - industry matrix
#' mat <- matrix(
#'   c(
#'     100, 0, 0, 0, 0,
#'     0, 15, 5, 70, 10,
#'     0, 20, 10, 20, 50,
#'     0, 25, 30, 5, 40,
#'     0, 40, 55, 5, 0
#'   ),
#'   ncol = 5, byrow = TRUE
#' )
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4", "I5")
#'
#' ## run the function
#' locational_gini(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{hoover_gini}}, \code{\link{locational_gini_curve}}, \code{\link{hoover_curve}}, \code{\link{lorenz_curve}}, \code{\link{gini}}
#' @references Krugman P. (1991) \emph{Geography and Trade}, MIT Press, Cambridge (chapter 2 - p.56)


locational_gini <- function(mat) {
  loc_gini <- function(mat, col = 1) {
    share_city_total <- rowSums(mat) / sum(mat)
    ind <- mat[, col]
    pop <- share_city_total
    oind <- order(ind / pop)
    ind <- ind[oind]
    pop <- pop[oind]
    cind <- cumsum(ind) / max(cumsum(ind))
    cpop <- cumsum(pop) / max(cumsum(pop))
    nn <- length(cind)
    (sum(cind[-1] * cpop[-nn]) - sum(cind[-nn] * cpop[-1])) / 2
  }

  y <- NULL
  for (i in seq_len(ncol(mat))) {
    tmp <- loc_gini(mat, i)
    y <- rbind(y, tmp)
  }

  x <- data.frame(colnames(mat), y[, 1])
  colnames(x) <- c("Industry", "Loc_gini")
  return(x)
}
