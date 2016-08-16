#' Create regions - industries matrices from regular data frames
#'
#' This function creates regions - industries (incidence) matrices from regular data frames (long to wide format) using the reshape2 package or the Matrix package
#' @param data is a data frame with three columns (regions, industries, count)
#' @param sparse Logical; shall the returned output be a sparse matrix? Defaults to FALSE, but can be set to TRUE if the dataset is very large
#' @keywords data.management
#' @usage get.matrix (data)
#' @export
#' @examples
#' ## generate a region - industry data frame
#' set.seed(31)
#' region <- c("R1", "R1", "R1", "R1", "R2", "R2", "R3", "R4", "R5", "R5")
#' industry <- c("I1", "I2", "I3", "I4", "I1", "I2", "I1", "I1", "I3", "I3")
#' data <- data.frame (region, industry)
#' data$count <- 1
#'
#' ## run the function
#' get.matrix (data)
#' get.matrix (data, sparse = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get.list}}


get.matrix <- function(data, sparse = FALSE) {

  data$x <- paste(data[,1],data[,2], sep = ".")
  data$y <- ave(data[,3], data[,4], FUN=sum)
  data <- data[!duplicated(data), ]
  data[,3:4] <- NULL

  if (sparse) {
  library (Matrix)

  data[,1] <- factor(data[,1])
  data[,2] <- factor(data[,2])
  adj <- sparseMatrix(as.integer(data[,1]), as.integer(data[,2]), x = data[,3])

  rownames(adj) = levels(data[,1])
  colnames(adj) = levels(data[,2])

   } else {

  library (Matrix)

  data[,1] <- factor(data[,1])
  data[,2] <- factor(data[,2])
  adj <- sparseMatrix(as.integer(data[,1]), as.integer(data[,2]), x = data[,3])

  rownames(adj) = levels(data[,1])
  colnames(adj) = levels(data[,2])

  adj = as.matrix (adj)

  }
  return (adj)

}
