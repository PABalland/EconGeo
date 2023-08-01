#' Create regular data frames from regions - industries matrices
#'
#' This function creates regular data frames with three columns (regions, industries, count) from (incidence) matrices (wide to long format) using the reshape2 package
#' @param mat An incidence matrix with regions in rows and industries in columns (or the other way around)
#' @return A data frame with three columns: "Region" (representing the region), "Industry" (representing the industry), and "Count" (representing the count of occurrences)
#' @keywords data.management
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' get_list(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get_matrix}}

get_list <- function(mat) {
  dn <- dimnames(mat)
  char <- sapply(dn, is.character)
  meltnew <- reshape::melt.matrix
  body(meltnew)[8][[1]] <-
    dn[char] <-
    lapply(dn[char], type.convert, as.is = TRUE)
  my_list <- meltnew(mat)
  colnames(my_list) <- c("Region", "Industry", "Count")
  return(my_list)
}
