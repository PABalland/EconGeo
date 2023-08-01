#' Compute the Shannon entropy index from regions - industries matrices
#'
#' This function computes the Shannon entropy index from regions - industries matrices from (incidence) regions - industries matrices
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @return A numeric vector representing the Shannon entropy index computed from the regions - industries matrix
#' @keywords diversity
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' entropy(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{diversity}}
#' @references Shannon, C.E., Weaver, W. (1949) \emph{The Mathematical Theory of Communication}. Univ of Illinois Press. \cr
#' \cr
#' Frenken, K., Van Oort, F. and Verburg, T. (2007) Related variety, unrelated variety and regional economic growth, \emph{Regional studies} \strong{41} (5): 685-697.

entropy <- function(mat) {
  freqs <- mat / rowSums(mat)
  entropy <- -rowSums(freqs * log2(freqs + 0.000000001))
  entropy <- round(entropy, digits = 3)
  return(entropy)
}
