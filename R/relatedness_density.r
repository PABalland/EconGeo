#' Compute the relatedness density between regions and industries from regions - industries matrices and industries - industries matrices
#'
#' This function computes the relatedness density between regions and industries from regions - industries (incidence) matrices and industries - industries (adjacency) matrices
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param relatedness An adjacency industry - industry matrix indicating the degree of relatedness between industries
#' @return A matrix representing the relatedness density between regions and industries. The values in the matrix indicate the share of industries related to each industry in each region, scaled from 0 to 100. Rows represent regions and columns represent industries.
#' @keywords relatedness
#' @export
#' @examples
#' ## generate a region - industry matrix in which cells represent the presence/absence of a RCA
#' set.seed(31)
#' mat <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## generate an industry - industry matrix in which cells indicate if two industries are
#' ## related (1) or not (0)
#' relatedness <- matrix(sample(0:1, 16, replace = TRUE), ncol = 4)
#' relatedness[lower.tri(relatedness, diag = TRUE)] <- t(relatedness)[lower.tri(t(relatedness),
#'   diag = TRUE
#' )]
#' rownames(relatedness) <- c("I1", "I2", "I3", "I4")
#' colnames(relatedness) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' relatedness_density(mat, relatedness)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{relatedness}}, \code{\link{co_occurrence}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

relatedness_density <- function(mat, relatedness) {
  # nb of tech related to i in each cities
  rel <- mat %*% relatedness

  # absolute nb of techs related to i (anywhere)
  reltot <- colSums(relatedness)

  # share of tech related to i in each cities
  reldens <- t(rel) / reltot

  # scale 0 to 100 - cities in rows / techs in columns
  reldens <- t(reldens) * 100

  # round
  reldens <- round(reldens, digits = 2)

  if (sum(mat > 1) > 0) {
    warning("some entries in the region - industry matrix are > 1")
  }

  return(reldens)
}
