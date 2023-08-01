#' Create regions - industries matrices from regular data frames
#'
#' This function creates regions - industries (incidence) matrices from regular data frames (long to wide format) using the reshape2 package or the Matrix package
#' @param my_data is a data frame with three columns (regions, industries, count)
#' @param sparse Logical; shall the returned output be a sparse matrix? Defaults to FALSE, but can be set to TRUE if the dataset is very large
#' @return A regions - industries matrix in either dense or sparse format, depending on the value of the "sparse" parameter
#' @keywords data.management
#' @usage get_matrix (my_data, sparse = FALSE)
#' @export
#' @examples
#' ## generate a region - industry data frame
#' set.seed(31)
#' region <- c("R1", "R1", "R1", "R1", "R2", "R2", "R3", "R4", "R5", "R5")
#' industry <- c("I1", "I2", "I3", "I4", "I1", "I2", "I1", "I1", "I3", "I3")
#' my_data <- data.frame(region, industry)
#' my_data$count <- 1
#'
#' ## run the function
#' get_matrix(my_data)
#' get_matrix(my_data, sparse = TRUE)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{get_list}}


get_matrix <- function(my_data, sparse = FALSE) {
  my_data$azertyuiop <- paste(my_data[, 1], my_data[, 2], sep = ".")
  my_data$qsdfghjklm <- ave(my_data[, 3], my_data[, 4], FUN = sum)
  my_data[, 3] <- NULL
  my_data <- my_data[!duplicated(my_data), ]
  my_data[, 3] <- NULL

  if (sparse) {
    # library (Matrix)

    my_data[, 1] <- factor(my_data[, 1])
    my_data[, 2] <- factor(my_data[, 2])
    adj <- sparseMatrix(as.integer(my_data[, 1]),
      as.integer(my_data[, 2]),
      x = my_data[, 3]
    )

    rownames(adj) <- levels(my_data[, 1])
    colnames(adj) <- levels(my_data[, 2])
  } else {
    # library (Matrix)

    my_data[, 1] <- factor(my_data[, 1])
    my_data[, 2] <- factor(my_data[, 2])
    adj <- sparseMatrix(as.integer(my_data[, 1]),
      as.integer(my_data[, 2]),
      x = my_data[, 3]
    )

    rownames(adj) <- levels(my_data[, 1])
    colnames(adj) <- levels(my_data[, 2])

    adj <- as.matrix(adj)
  }
  return(adj)
}
