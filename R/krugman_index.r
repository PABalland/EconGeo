#' Compute the Krugman index from regions - industries matrices
#'
#' This function computes the Krugman index from regions - industries matrices. The higher the coefficient, the greater the regional specialization. This index is often referred to as the Krugman specialisation index and measures the distance between the distributions of industry shares in a region and at a more aggregated level (country for instance).
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @return A vector representing the Krugman index of regional specialization computed from the regions - industries matrix.
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
#' krugman_index(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient_avg}}
#' @references Krugman P. (1991) \emph{Geography and Trade}, MIT Press, Cambridge


krugman_index <- function(mat) {
  share_tech_city <- mat / rowSums(mat)
  share_tech_total <- colSums(mat) / sum(mat)
  x <- matrix(share_tech_total,
    nrow = nrow(share_tech_city),
    ncol = length(share_tech_total), byrow = TRUE
  )
  k_i <- rowSums(abs(share_tech_city - x))
  return(k_i)
}
