#' Create regular data frames from regions - industries matrices
#'
#' This function creates regular data frames with three columns (regions, industries, count) from (incidence) matrices (wide to long format) using the reshape2 package
#' @param mat An incidence matrix with regions in rows and industries in columns (or the other way around)
#' @param sparse Logical; is the input a sparse matrix? Defaults to FALSE
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
#'
#' ## generate a sparse matrix
#' library (Matrix)
#' set.seed(31)
#' mat = Matrix (matrix(sample(0:100,20,replace=T), ncol = 4))
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' get.list (mat, sparse = T)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get.matrix}}


get.list = function (mat, sparse = F){
  if (sparse) {

  r = data.frame (rowid = as.integer (as.factor (rownames (mat))), region = rownames (mat))
  c = data.frame (colid = as.integer (as.factor (colnames (mat))), industry = colnames (mat))

  library(Matrix)
  id = Matrix(mat, sparse = T)
  id = Matrix::summary(id)
  id = as.data.frame(as.matrix(id))
  colnames(id) <- c("Reg", "Ind", "Cnt")

  id$Region <- r[,2][match(id[,1], r[,1])]
  id$Industry <- c[,2][match(id[,2], c[,1])]
  id$Count <- id$Cnt

  list = id[, 4:6]


  } else {

    r = data.frame (rowid = as.integer (as.factor (rownames (mat))), region = rownames (mat))
    c = data.frame (colid = as.integer (as.factor (colnames (mat))), industry = colnames (mat))

    id <- cbind(rowid = as.vector(t(row(mat))),
                rowid = as.vector(t(col(mat))),
                colid = as.vector(t(mat)))

    id = as.data.frame (id)

    id$Region <- r[,2][match(id[,1], r[,1])]
    id$Industry <- c[,2][match(id[,2], c[,1])]
    id$Count <- id$colid

    list = id[, 4:6]

  }

  return(list)
}


