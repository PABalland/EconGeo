#' Create regular data frames from regions - industries matrices 
#'
#' This function creates regular data frames with three columns (regions, industries, count) from (incidence) matrices (wide to long format) using the reshape2 package
#' @param mat An incidence matrix with regions in rows and industries in columns (or the other way around)
#' @keywords adjacency matrix, edge lists, reshape package, economics, geography, economic geography
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
#' get.list (mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get.matrix}}

get.list <- function(mat) {
  library (reshape2)
  list <- melt(mat)
  colnames (list) <- c ("Region", "Industry", "Count")
  return (list)
}




