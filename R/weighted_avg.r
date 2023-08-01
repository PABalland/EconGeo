#' Compute a weighted average of regions or industries from regions - industries matrices
#'
#' This function computes a weighted average of regions or industries from (incidence) regions - industries matrices.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param vec A vector that will be used to compute the weighted average for each industry/region
#' @param reg Logical; Shall the weighted average for regions be returned? Default to TRUE (requires a vector of industry value) but can be set to FALSE (requires a vector of region value) if the weighted average for industries should be returned
#' @return A numeric vector representing the weighted average of regions or industries, depending on the value of the `reg` argument. If `reg = TRUE`, the weighted average for regions is returned; if `reg = FALSE`, the weighted average for industries is returned.
#' @keywords diversity specialization
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## a vector for regions will be used to computed the weighted average of industries
#' vec <- c(5, 10, 15, 25, 50)
#' ## run the function
#' weighted_avg(mat, vec, reg = FALSE)
#'
#' ## a vector for industries will be used to computed the weighted average of regions
#' vec <- c(5, 10, 15, 25)
#' ## run the function
#' weighted_avg(mat, vec, reg = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}

weighted_avg <- function(mat, vec, reg = TRUE) {
  if (reg) {
    tt <- t(mat / rowSums(mat))
    w <- vec %*% tt
    return(w)
  }

  if (!reg) {
    tt <- t(mat) / (rowSums(t(mat)))
    w <- vec %*% t(tt)
    return(w)
  }
}
