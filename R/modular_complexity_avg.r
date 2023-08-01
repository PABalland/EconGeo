#' Compute a measure of average modular complexity of technologies
#'
#' This function computes a measure of average modular complexity of technologies (average complexity of patent documents in a given technological class) from technological classes - patents (incidence) matrices
#' @param mat A bipartite adjacency matrix (can be a sparse matrix)
#' @param sparse Logical; is the input matrix a sparse matrix? Defaults to FALSE, but can be set to TRUE if the input matrix is a sparse matrix
#' @param list Logical; is the input a list? Defaults to FALSE (input = adjacency matrix), but can be set to TRUE if the input is an edge list
#' @return A data frame with columns "tech" and "avg.mod.comp" representing the technologies and their corresponding average modular complexity values.
#' @keywords complexity
#' @export
#' @examples
#' ## generate a technology - patent matrix
#' set.seed(31)
#' mat <- matrix(sample(0:1, 30, replace = TRUE), ncol = 5)
#' rownames(mat) <- c("T1", "T2", "T3", "T4", "T5", "T6")
#' colnames(mat) <- c("US1", "US2", "US3", "US4", "US5")
#'
#' ## run the function
#' modular_complexity_avg(mat)
#'
#' ## generate a technology - patent sparse matrix
#' library(Matrix)
#'
#' ## run the function
#' smat <- Matrix(mat, sparse = TRUE)
#'
#' modular_complexity_avg(smat, sparse = TRUE)
#' ## generate a regular data frame (list)
#' my_list <- get_list(mat)
#'
#' ## run the function
#' modular_complexity_avg(my_list, list = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @references Fleming, L. and Sorenson, O. (2001) Technology as a complex adaptive system: evidence from patent data, \emph{Research Policy} \strong{30}: 1019-1039
#' @seealso \code{\link{ease_recombination}}, \code{\link{tci}}, \code{\link{mort}}

modular_complexity_avg <- function(mat, sparse = FALSE, list = FALSE) {
  # library (Matrix)

  if (!list) {
    if (!sparse) {
      mat <- Matrix(mat, sparse = TRUE)
      cooc <- mat %*% Matrix::t(mat)
      diag(cooc) <- 0
      cooc[cooc > 1] <- 1

      ease <- Matrix::rowSums(cooc, na.rm = TRUE) /
        Matrix::rowSums(mat, na.rm = TRUE)

      intpat <- Matrix::colSums(mat, na.rm = TRUE) / (Matrix::t(mat) %*% ease)
      intpat[is.infinite(intpat)] <- 0

      avgintpat <- (mat %*% intpat) / Matrix::rowSums(mat, na.rm = TRUE)

      avgintpat <- data.frame(
        tech = rownames(mat),
        avg.mod.comp = round(as.numeric(avgintpat), 2)
      )
    } else {
      cooc <- mat %*% Matrix::t(mat)
      diag(cooc) <- 0
      cooc[cooc > 1] <- 1

      ease <- Matrix::rowSums(cooc, na.rm = TRUE) /
        Matrix::rowSums(mat, na.rm = TRUE)

      intpat <- Matrix::colSums(mat, na.rm = TRUE) / (Matrix::t(mat) %*% ease)
      intpat[is.infinite(intpat)] <- 0

      avgintpat <- (mat %*% intpat) / Matrix::rowSums(mat, na.rm = TRUE)

      avgintpat <- data.frame(
        tech = rownames(mat),
        avg.mod.comp = round(as.numeric(avgintpat), 2)
      )
    }
  } else {
    mat <- get_matrix(mat, sparse = TRUE)
    cooc <- mat %*% Matrix::t(mat)
    diag(cooc) <- 0
    summ <- Matrix::summary(cooc)
    summ$x[summ$x > 1] <- 1
    x <- get_matrix(summ, sparse = TRUE)
    colnames(x) <- colnames(cooc)
    rownames(x) <- rownames(cooc)
    cooc <- x
    ease <- Matrix::rowSums(cooc) / Matrix::rowSums(mat)
    intpat <- Matrix::colSums(mat) / (Matrix::t(mat) %*% ease)
    intpat[is.infinite(intpat)] <- 0

    avgintpat <- (mat %*% intpat) / Matrix::rowSums(mat, na.rm = TRUE)

    avgintpat <- data.frame(
      tech = rownames(mat),
      avg.mod.comp = round(as.numeric(avgintpat), 2)
    )
  }

  return(avgintpat)
}
