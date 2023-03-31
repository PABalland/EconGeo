#' Compute the Hoover coefficient of specialization from regions - industries matrices
#'
#' This function computes the Hoover coefficient of specialization from regions - industries matrices. The higher the coefficient, the greater the regional specialization. This index is closely related to the Krugman specialisation index.
#' @param mat An incidence matrix with regions in rows and industries in columns
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
#' spec_coeff(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{krugman_index}}
#' @references Hoover, E.M. and Giarratani, F. (1985) \emph{An Introduction to Regional Economics}. 3rd edition. New York: Alfred A. Knopf (see table 9-4 in particular)

spec_coeff <- function(mat) {
  share_tech_city <- mat / rowSums(mat)
  share_tech_total <- colSums(mat) / sum(mat)
  x <- matrix(share_tech_total,
    nrow = nrow(share_tech_city),
    ncol = length(share_tech_total), byrow = TRUE
  )
  k <- rowSums(abs(share_tech_city - x))
  k <- k / 2
  return(k)
}
