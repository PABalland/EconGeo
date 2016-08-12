#' Create regular data frames from regions - industries matrices
#'
#' This function creates regular data frames with three columns (regions, industries, count) from (incidence) matrices (wide to long format) using the reshape2 package
#' @param mat An incidence matrix with regions in rows and industries in columns (or the other way around)
#' @keywords data.management
#' @usage get.list (data)
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100,20,replace=T), ncol = 4)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' get.list (mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get.matrix}}


get.list = function (mat){
  library(Matrix)
  list = Matrix(mat, sparse = T)
  list = Matrix::summary(list)
  list = as.data.frame(as.matrix(list))
  colnames(list) <- c("Region", "Industry", "Count")
  return(list)
}


