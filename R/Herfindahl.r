#' Compute the Herfindahl index from regions - industries matrices
#'
#' This function computes the Herfindahl index from regions - industries matrices from (incidence) regions - industries matrices. This index is also known as the  Herfindahl-Hirschman index.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @return A vector of Herfindahl index values indicating the concentration of industries within regions
#' @keywords specialization concentration
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' herfindahl(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{krugman_index}}
#' @references Herfindahl, O.C. (1959) \emph{Copper Costs and Prices: 1870-1957}. Baltimore: The Johns Hopkins Press. \cr
#' \cr
#' Hirschman, A.O. (1945) \emph{National Power and the Structure of Foreign Trade}, Berkeley and Los Angeles: University of California Press.

herfindahl <- function(mat) {
  herfindahl_v <- rowSums(mat * mat) / (rowSums(mat) * rowSums(mat))
  return(herfindahl_v)
}
