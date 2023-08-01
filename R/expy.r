#' Compute the expy index of regions from regions - industries matrices
#'
#' This function computes the expy index of regions from (incidence) regions - industries matrices, as proposed by Hausmann, Hwang & Rodrik (2007). The index is a measure of the productivity level associated with a region's specialization pattern.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param vec A vector that gives GDP, R&D, education or any other relevant regional attribute that will be used to compute the weighted average for each industry
#' @return A numeric vector representing the expy index of regions computed from the regions - industries matrix
#' @keywords diversity specialization
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## a vector of GDP of regions
#' vec <- c(5, 10, 15, 25, 50)
#' ## run the function
#' expy(mat, vec)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}
#' @references Balassa, B. (1965) Trade Liberalization and Revealed Comparative Advantage, \emph{The Manchester School} \strong{33}: 99-123 \cr
#' \cr
#' Hausmann, R., Hwang, J. & Rodrik, D. (2007) What you export matters, \emph{Journal of economic growth} \strong{12}: 1-25.

expy <- function(mat, vec) {
  p <- (vec %*% rca(mat, binary = FALSE)) / colSums(rca(mat, binary = FALSE))

  tt <- t(mat / rowSums(mat))

  e <- p %*% tt

  return(e)
}
