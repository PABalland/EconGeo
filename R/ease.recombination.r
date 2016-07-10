#' Compute the ease of recombination of a given technological class
#'
#' This function computes the ease of recombination of a given technological class from technological classes - patents (incidence) matrices 
#' @param mat A bipartite adjacency matrix (can be a sparse matrix)
#' @param sparse Logical; is the input matrix a sparse matrix? Defaults to FALSE, but can be set to TRUE if the input matrix is a sparse matrix
#' @keywords modular complexity interdependencies recombination
#' @export
#' @examples
#' ## generate a region - industry matrix 
#' set.seed(31)
#' mat <- matrix(sample(0:1,30,replace=T), ncol = 5)
#' rownames(mat) <- c ("T1", "T2", "T3", "T4", "T5", "T6")
#' colnames(mat) <- c ("US1", "US2", "US3", "US4", "US5")
#' 
#' ## generate a region - industry sparse matrix 
#' library (Matrix)
#' smat <- Matrix(mat,sparse=TRUE)
#'
#' ## run the function
#' modular.complexity (mat)
#' modular.complexity (smat, sparse = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @references Fleming, L. and Sorenson, O. (2001) Technology as a complex adaptive system: evidence from patent data, \emph{Research Policy} \strong{30}: 1019-1039
#' @seealso \code{\link{modular.complexity}}, \code{\link{TCI}}, \code{\link{MORt}}

ease.recombination <- function(mat, sparse = FALSE) {

  library (Matrix)

  if (!sparse) {

  mat <- Matrix(mat,sparse=TRUE)
  cooc =   mat %*% Matrix::t(mat)
  diag(cooc) <- 0
  cooc[cooc > 1] <- 1

  Ease <- Matrix::rowSums(cooc)/Matrix::rowSums(mat)

   } else {

  cooc =   mat %*% Matrix::t(mat)
  diag(cooc) <- 0
  cooc[cooc > 1] <- 1

  Ease <- Matrix::rowSums(cooc)/Matrix::rowSums(mat)

   } 

  return(Ease)
}



