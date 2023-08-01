#' Compute a measure of complexity by normalizing ubiquity of industries
#'
#' This function computes a measure of complexity by normalizing ubiquity of industries. We divide the share of the total count (employment, number of firms, number of patents, ...) in an industry by its share of ubiquity. Ubiquity is given by the number of regions in which an industry can be found (location quotient > 1) from regions - industries (incidence) matrices
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @keywords ubiquity complexity
#' @export
#' @examples
#' ## generate a region - industry matrix with full count
#' set.seed(31)
#' mat <- matrix(sample(0:10, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' norm_ubiquity(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{diversity}}, \code{\link{location_quotient}}, \code{\link{ubiquity}}, \code{\link{tci}}, \code{\link{mort}}
#' @references Balland, P.A. and Rigby, D. (2017) The Geography of Complex Knowledge, \emph{Economic Geography} \strong{93} (1): 1-23.


norm_ubiquity <- function(mat) {
  inv <- (colSums(mat) /
    sum(mat)) /
    (colSums(rca(mat, binary = TRUE) /
      sum(rca(mat, binary = TRUE))))
  inv <- round(inv, 2)
  return(inv)
}
