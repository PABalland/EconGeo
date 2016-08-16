#' Compute average location quotients of regions from regions - industries matrices
#'
#' This function computes the average location quotients of regions from (incidence) regions - industries matrices. This index is also referred to as the \emph{coefficient of specialization} (Hoover and Giarratani, 1985).
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @keywords specialization concentration
#' @export
#' @examples
#' ## generate a region - industry matrix
#' mat = matrix (
#' c (100, 0, 0, 0, 0,
#' 0, 15, 5, 70, 10,
#' 0, 20, 10, 20, 50,
#' 0, 25, 30, 5, 40,
#' 0, 40, 55, 5, 0), ncol = 5, byrow = T)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4", "I5")
#'
#' ## run the function
#' location.quotient.avg (mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location.quotient}}, \code{\link{Hachman}}
#' @references Hoover, E.M. and Giarratani, F. (1985) \emph{An Introduction to Regional Economics}. 3rd edition. New York: Alfred A. Knopf \cr
#' \cr
#' Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250

location.quotient.avg <- function(mat) {
  share_tech_city <- mat / rowSums (mat)
  share_tech_total <- colSums (mat) / sum (mat)
  LQ <- t(t(share_tech_city)/ share_tech_total)
  LQ[is.na(LQ)] <- 0
  meanLQ <- rowSums (LQ*share_tech_city)
  return (meanLQ)
}



