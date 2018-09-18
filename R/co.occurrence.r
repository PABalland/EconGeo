#' Compute the number of co-occurrences between industry pairs from an incidence (industry - event) matrix
#'
#' This function computes the number of co-occurrences between industry pairs from an incidence (industry - event) matrix
#' @param mat An incidence matrix with industries in rows and events in columns
#' @param diagonal Logical; shall the values in the diagonal of the co-occurrence matrix be included in the output? Defaults to FALSE (values in the diagonal are set to 0), but can be set to TRUE (values in the diagonal reflects in how many events a single industry can be found)
#' @param list Logical; is the input a list? Defaults to FALSE (input = adjacency matrix), but can be set to TRUE if the input is an edge list
#' @keywords relatedness
#' @export
#' @examples
#' ## generate a region - events matrix
#' set.seed(31)
#' mat <- matrix(sample(0:1,20,replace=T), ncol = 5)
#' rownames(mat) <- c ("I1", "I2", "I3", "I4")
#' colnames(mat) <- c("US1", "US2", "US3", "US4", "US5")
#'
#' ## run the function
#' co.occurrence (mat)
#' co.occurrence (mat, diagonal = TRUE)
#'
#' ## generate a regular data frame (list)
#' list <- get.list (mat)
#'
#' ## run the function
#' co.occurrence (list, list = TRUE)
#' co.occurrence (list, list = TRUE, diagonal = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{relatedness}}, \code{\link{relatedness.density}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250


co.occurrence <- function(mat, diagonal = FALSE, list = FALSE) {

  library (Matrix)

  mat = as.matrix (mat)

  if (list) {
    mat <- get.matrix (mat, sparse = TRUE)
  } else {
    mat <- Matrix(mat, sparse=TRUE)
  }

  cooc = mat %*% Matrix::t(mat)
  cooc = as.matrix (cooc)

  if (!diagonal) {
    diag(cooc) <- 0
  }
  
  return (cooc)

}
