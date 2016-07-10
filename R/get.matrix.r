#' Create regions - industries matrices from regular data frames 
#'
#' This function creates regions - industries (incidence) matrices from regular data frames (long to wide format) using the reshape2 package or the Matrix package
#' @param data is a data frame with three columns (regions, industries, count)
#' @param sparse Logical; shall the returned output be a sparse matrix? Defaults to FALSE, but can be set to TRUE if the dataset is very large
#' @keywords adjacency matrix, sparse matrix, edge lists, reshape package, Matrix package, economics, geography, economic geography
#' @export
#' @examples
#' ## generate a region - industry data frame
#' set.seed(31)
#' region <- c("R1", "R1", "R1", "R1", "R2", "R2", "R3", "R4", "R5")
#' industry <- c("I1", "I2", "I3", "I4", "I1", "I2", "I1", "I1", "I3")
#' data <- data.frame (region, industry)
#' data$count <- 1
#' 
#' ## run the function
#' get.matrix (data)
#' get.matrix (data, sparse = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get.list}}


get.matrix <- function(data, sparse = FALSE) {
  if (sparse) {
  library (Matrix)

  data[,1] <- as.factor(data[,1])
  data[,2] <- as.factor(data[,2])
  adj <- sparseMatrix(as.integer(data[,1]), as.integer(data[,2]), x = data[,3])

  rownames(adj) = levels(data[,1])
  colnames(adj) = levels(data[,2])

   } else {

  library (reshape2)
  adj <- acast (data, data[,1] ~ data[,2], sum)
  adj[is.na(adj)] <- 0
  adj <- as.matrix (adj)
  }
  return (adj)
  
}
