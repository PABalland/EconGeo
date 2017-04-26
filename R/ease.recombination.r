#' Compute the ease of recombination of a given technological class
#'
#' This function computes the ease of recombination of a given technological class from technological classes - patents (incidence) matrices
#' @param mat A bipartite adjacency matrix (can be a sparse matrix)
#' @param sparse Logical; is the input matrix a sparse matrix? Defaults to FALSE, but can be set to TRUE if the input matrix is a sparse matrix
#' @keywords complexity
#' @export
#' @examples
#' ## generate a technology - patent matrix
#' set.seed(31)
#' mat <- matrix(sample(0:1,30,replace=T), ncol = 5)
#' rownames(mat) <- c ("T1", "T2", "T3", "T4", "T5", "T6")
#' colnames(mat) <- c ("US1", "US2", "US3", "US4", "US5")
#'
#' ## generate a technology - patent sparse matrix
#' library (Matrix)
#' smat <- Matrix(mat,sparse=TRUE)
#'
#' ## run the function
#' ease.recombination (mat)
#' ease.recombination (smat, sparse = TRUE)
#'
#' ## generate a regular data frame (list)
#' list <- get.list (mat)
#'
#' ## run the function
#' ease.recombination (list, list = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @references Fleming, L. and Sorenson, O. (2001) Technology as a complex adaptive system: evidence from patent data, \emph{Research Policy} \strong{30}: 1019-1039
#' @seealso \code{\link{modular.complexity}}, \code{\link{TCI}}, \code{\link{MORt}}

ease.recombination <- function(mat, sparse = FALSE, list = FALSE) {

  library (Matrix)

  if (!list) {

    if (!sparse) {

      mat <- Matrix(mat,sparse=TRUE)
      cooc =   mat %*% Matrix::t(mat)
      diag(cooc) <- 0
      cooc[cooc > 1] <- 1

      Ease <- Matrix::rowSums(cooc)/Matrix::rowSums(mat)
      IntPat2 <- data.frame (tech = rownames (mat),
                             eor = round (as.numeric (Ease),5))


    } else {

      cooc =   mat %*% Matrix::t(mat)
      diag(cooc) <- 0
      cooc[cooc > 1] <- 1

      Ease <- Matrix::rowSums(cooc)/Matrix::rowSums(mat)

      IntPat2 <- data.frame (tech = rownames (mat),
                             eor = round (as.numeric (Ease),5))


    }

  } else {

    mat <- get.matrix(mat, sparse = TRUE)
    cooc = mat %*% Matrix::t(mat)
    diag(cooc) <- 0
    summ <- Matrix::summary(cooc)
    summ$x[summ$x>1] = 1
    x = get.matrix(summ, sparse = T)
    colnames (x) = colnames (cooc)
    rownames (x) = rownames (cooc)
    cooc = x
    Ease <- Matrix::rowSums(cooc)/Matrix::rowSums(mat)
    IntPat2 <- data.frame (tech = rownames (cooc),
                           eor = round (as.numeric (Ease),5))

  }

  return(IntPat2)
}



