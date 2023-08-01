#' Compute the prody index of industries from regions - industries matrices
#'
#' This function computes the prody index of industries from (incidence) regions - industries matrices, as proposed by Hausmann, Hwang & Rodrik (2007). The index gives an associated income level for each industry. It represents a weighted average of per-capita GDPs (but GDP can be replaced by R&D, education...), where the weights correspond to the revealed comparative advantage of each region in a given industry (or sector, technology, ...).
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param vec A vector that gives GDP, R&D, education or any other relevant regional attribute that will be used to compute the weighted average for each industry
#' @return A numeric vector representing the prody index of industries. Each value in the vector corresponds to the associated income level for an industry.
#' @keywords complexity concentration
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
#' prody(mat, vec)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}
#' @references Balassa, B. (1965) Trade Liberalization and Revealed Comparative Advantage, \emph{The Manchester School} \strong{33}: 99-123 \cr
#' \cr
#' Hausmann, R., Hwang, J. & Rodrik, D. (2007) What you export matters, \emph{Journal of economic growth} \strong{12}: 1-25.

prody <- function(mat, vec) {
  p <- (vec %*% rca(mat, binary = FALSE)) / colSums(rca(mat, binary = FALSE))

  return(p)
}
