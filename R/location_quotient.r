#' Compute location quotients from regions - industries matrices
#'
#' This function computes location quotients from (incidence) regions - industries matrices. The numerator is the share of a given industry in a given region. The denominator is the share of a this industry in a larger economy (overall country for instance). This index is also refered to as the index of Revealed Comparative Advantage (RCA) following Ballasa (1965), or the Hoover-Balassa index.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param binary Logical; shall the returned output be a dichotomized version (0/1) of the location quotient? Defaults to FALSE (the full values of the location quotient will be returned), but can be set to TRUE (location quotient values above 1 will be set to 1 & location quotient values below 1 will be set to 0)
#' @return A matrix of location quotients computed from the regions - industries matrix. If the 'binary' parameter is set to TRUE, the returned matrix will contain binary values (0/1) representing the location quotient. If 'binary' is set to FALSE, the full values of the location quotient will be returned.
#' @keywords specialization concentration
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
#' location_quotient(mat)
#' location_quotient(mat, binary = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{rca}}
#' @references Balassa, B. (1965) Trade Liberalization and Revealed Comparative Advantage, \emph{The Manchester School} \strong{33}: 99-123.

location_quotient <- function(mat, binary = FALSE) {
  share_tech_city <- mat / rowSums(mat)
  share_tech_total <- colSums(mat) / sum(mat)
  if (binary) {
    lq <- t(t(share_tech_city) / share_tech_total)
    lq[is.na(lq)] <- 0
    lq[lq < 1] <- 0
    lq[lq > 1] <- 1
  } else {
    lq <- t(t(share_tech_city) / share_tech_total)
    lq[is.na(lq)] <- 0
  }
  return(lq)
}
