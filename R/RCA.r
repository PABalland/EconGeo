#' Compute an index of revealed comparative advantage (RCA) from regions - industries matrices
#'
#' This function computes an index of revealed comparative advantage (RCA) from (incidence) regions - industries matrices. The numerator is the share of a given industry in a given region. The denominator is the share of a this industry in a larger economy (overall country for instance). This index is also refered to as a location quotient, or the Hoover-Balassa index.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param binary Logical; shall the returned output be a dichotomized version (0/1) of the RCA? Defaults to FALSE (the full values of the RCA will be returned), but can be set to TRUE (RCA above 1 will be set to 1 & RCA values below 1 will be set to 0)
#' @keywords specialization
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' rca(mat)
#' rca(mat, binary = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}
#' @references Balassa, B. (1965) Trade Liberalization and Revealed Comparative Advantage, \emph{The Manchester School} \strong{33}: 99-123.

rca <- function(mat, binary = FALSE) {
  mat <- as.matrix(mat)
  share_tech_city <- mat / rowSums(mat)
  share_tech_total <- colSums(mat) / sum(mat)

  lq <- t(t(share_tech_city) / share_tech_total)
  lq[is.na(lq)] <- 0

  if (binary) {
    lq[lq < 1] <- 0
    lq[lq > 1] <- 1
  }
  lq <- round(lq, digits = 2)
  return(lq)
}
