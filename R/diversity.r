#' Compute a simple measure of diversity of regions
#'
#' This function computes a simple measure of diversity of regions by counting the number of industries in which a region has a relative comparative advantage (location quotient > 1) from regions - industries (incidence) matrices
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param rca Logical; should the index of relative comparative advantage (RCA - also refered to as location quotient) first be computed? Defaults to FALSE (a binary matrix - 0/1 - is expected as an input), but can be set to TRUE if the index of relative comparative advantage first needs to be computed
#' @keywords diversity
#' @export
#' @examples
#' ## generate a region - industry matrix with full count
#' set.seed(31)
#' mat <- matrix(sample(0:10, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' diversity(mat, rca = TRUE)
#'
#' ## generate a region - industry matrix in which cells represent the presence/absence of a RCA
#' set.seed(31)
#' mat <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' diversity(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{ubiquity}}, \code{\link{location_quotient}}
#' @references Balland, P.A. and Rigby, D. (2017) The Geography of Complex Knowledge, \emph{Economic Geography} \strong{93} (1): 1-23.


diversity <- function(mat, rca = FALSE) {
  if (rca) {
    share_tech_city <- mat / rowSums(mat)
    share_tech_total <- colSums(mat) / sum(mat)
    # compute the location quotient
    lq <- t(t(share_tech_city) / share_tech_total)
    lq[is.na(lq)] <- 0
    lq[lq < 1] <- 0
    lq[lq > 1] <- 1
    mat <- lq

    # compute the share of a tech in a city's portfolio
    # markov chain - row stochastic
    sh <- rowSums(mat)
  } else {
    # compute the share of a tech in a city's portfolio
    # markov chain - row stochastic
    sh <- rowSums(mat)
  }
  return(sh)
}
