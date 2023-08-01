#' Re-arrange the dimension of a matrix based on the dimension of another matrix
#'
#' This function e-arranges the dimension of a matrix based on the dimension of another matrix
#' @param fill A matrix that will be used to populate the matrix output
#' @param dim A matrix that will be used to determine the dimensions of the matrix output
#' @param missing Logical; Shall the cells of the non matching rows/columns set to NA? Default to TRUE but can be set to FALSE to set the cells of the non matching rows/columns to 0 instead.
#' @return The matrix output with the dimensions rearranged based on the input `dim` matrix.
#' @keywords data.management
#' @export
#' @examples
#' ## generate a first region - industry matrix
#' set.seed(31)
#' mat1 <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
#' rownames(mat1) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c("I1", "I2", "I3", "I4")
#'
#' ## generate a second region - industry matrix
#' set.seed(31)
#' mat2 <- matrix(sample(0:1, 16, replace = TRUE), ncol = 4)
#' rownames(mat2) <- c("R1", "R2", "R3", "R5")
#' colnames(mat2) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' match_mat(fill = mat1, dim = mat2)
#' match_mat(fill = mat2, dim = mat1)
#' match_mat(fill = mat2, dim = mat1, missing = FALSE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}

match_mat <- function(fill, dim, missing = TRUE) {
  if (missing) {
    dim[dim != 0.123456789] <- NA
    cols <- colnames(dim)[colnames(dim) %in% colnames(fill)]
    rows <- rownames(dim)[rownames(dim) %in% rownames(fill)]
    dim[rows, cols] <- fill[rows, cols]
    return(dim)
  } else {
    dim[dim != 0] <- 0
    cols <- colnames(dim)[colnames(dim) %in% colnames(fill)]
    rows <- rownames(dim)[rownames(dim) %in% rownames(fill)]
    dim[rows, cols] <- fill[rows, cols]
    return(dim)
  }
}
