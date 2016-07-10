#' Re-arrange the dimension of a matrix based on the dimension of another matrix
#'
#' This function e-arranges the dimension of a matrix based on the dimension of another matrix
#' @param fill A matrix that will be used to populate the matrix output
#' @param dim A matrix that will be used to determine the dimensions of the matrix output
#' @param missing Logical; Shall the cells of the non matching rows/columns set to NA? Default to TRUE but can be set to FALSE to set the cells of the non matching rows/columns to 0 instead. 
#' @keywords 
#' @export
#' @examples
#' ## generate a first region - industry matrix 
#' set.seed(31)
#' mat1 <- matrix(sample(0:1,20,replace=T), ncol = 4) 
#' rownames(mat1) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c ("I1", "I2", "I3", "I4")
#' 
#' ## generate a second region - industry matrix 
#' set.seed(31)
#' mat2 <- matrix(sample(0:1,16,replace=T), ncol = 4) 
#' rownames(mat2) <- c ("R1", "R2", "R3", "R5")
#' colnames(mat2) <- c ("I1", "I2", "I3", "I4")
#' 
#' ## run the function
#' matchmat (fill = mat1, dim = mat2)
#' matchmat (fill = mat2, dim = mat1)
#' matchmat (fill = mat2, dim = mat1, missing = F)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location.quotient}}

matchmat <- function(fill = mat1, dim = mat2, missing = T) {

if (missing) {
dim[dim != 0.123456789] = NA
cols <- colnames(dim)[colnames(dim) %in% colnames(fill)]
rows <- rownames(dim)[rownames(dim) %in% rownames(fill)]
dim[rows, cols] <- fill[rows, cols]
return (dim)
}

else {
dim[dim != 0] = 0
cols <- colnames(dim)[colnames(dim) %in% colnames(fill)]
rows <- rownames(dim)[rownames(dim) %in% rownames(fill)]
dim[rows, cols] <- fill[rows, cols]
return (dim)
}

}



