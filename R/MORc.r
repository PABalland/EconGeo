#' Compute an index of knowledge complexity of regions using the method of reflection
#'
#' This function computes an index of knowledge complexity of regions using the method of reflection from regions - industries (incidence) matrices. The index has been developed by Hidalgo and Hausmann (2009) for country - product matrices and adapted by Balland and Rigby (2016) to city - technology matrices.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param rca Logical; should the index of relative comparative advantage (RCA - also refered to as location quotient) first be computed? Defaults to FALSE (a binary matrix - 0/1 - is expected as an input), but can be set to TRUE if the index of relative comparative advantage first needs to be computed
#' @param steps Number of iteration steps. Defaults to 20, but can be set to 0 to give diversity (number of industry in which a region has a RCA), to 1 to give  the average ubiquity of the industries in which a region has a RCA, to 2 to give the average diversity of regions that have similar industrial structures, or to any other number of steps < or = to 22. Note that above steps = 2 the index will be rescaled from 0 (minimum relative complexity) to 100 (maximum relative complexity).
#' @keywords complexity
#' @export
#' @examples
#' ## generate a region - industry matrix with full count
#' set.seed(31)
#' mat <- matrix(sample(0:10, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' morc(mat, rca = TRUE)
#' morc(mat, rca = TRUE, steps = 0)
#' morc(mat, rca = TRUE, steps = 1)
#' morc(mat, rca = TRUE, steps = 2)
#'
#' ## generate a region - industry matrix in which cells represent the presence/absence of an RCA
#' set.seed(32)
#' mat <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' morc(mat)
#' morc(mat, steps = 0)
#' morc(mat, steps = 1)
#' morc(mat, steps = 2)
#'
#' ## generate the simple network of Hidalgo and Hausmann (2009) presented p.11 (Fig. S4)
#' countries <- c("C1", "C1", "C1", "C1", "C2", "C3", "C3", "C4")
#' products <- c("P1", "P2", "P3", "P4", "P2", "P3", "P4", "P4")
#' my_data <- data.frame(countries, products)
#' my_data$freq <- 1
#' mat <- get_matrix(my_data)
#'
#' ## run the function
#' morc(mat)
#' morc(mat, steps = 0)
#' morc(mat, steps = 1)
#' morc(mat, steps = 2)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}, \code{\link{ubiquity}}, \code{\link{diversity}}, \code{\link{kci}}, \code{\link{tci}}, \code{\link{mort}}
#' @references Hidalgo, C. and Hausmann, R. (2009) The building blocks of economic complexity, \emph{Proceedings of the National Academy of Sciences} \strong{106}: 10570 - 10575. \cr
#' \cr
#' Balland, P.A. and Rigby, D. (2017) The Geography of Complex Knowledge, \emph{Economic Geography} \strong{93} (1): 1-23.

morc <- function(mat, rca = FALSE, steps = 20) {
  # Remove null observations
  mat <- mat[rowSums(mat) > 0, ]
  mat <- mat[, colSums(mat) > 0]

  rescale <- function(x) {
    (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
  }

  if (rca) {
    share_tech_city <- mat / rowSums(mat)
    share_tech_total <- colSums(mat) / sum(mat)
    lq <- t(t(share_tech_city) / share_tech_total)
    lq[is.na(lq)] <- 0
    lq[lq < 1] <- 0
    lq[lq > 1] <- 1
    mat <- lq
  }

  diversification <- rowSums(mat)
  ubiquity <- colSums(mat)

  city_order <- matrix(0, nrow(mat), steps)
  rownames(city_order) <- rownames(mat) # Add row names to city_order
  tech_order <- matrix(0, ncol(mat), steps)

  if (steps != 0) {
    for (i in 1:steps) {
      if (i == 1) {
        city_order[, i] <- (1 / diversification) * rowSums(t(t(mat) * ubiquity))
        tech_order[, i] <- (1 / ubiquity) * colSums(mat * diversification)
      } else if (i == 2) {
        city_order[, i] <-
          (1 / diversification) * rowSums(t(t(mat) * tech_order[, i - 1]))
        tech_order[, i] <-
          (1 / ubiquity) * colSums(mat * city_order[, i - 1])
      } else {
        city_order[, i] <- rescale((1 / diversification) *
          rowSums(t(t(mat) * tech_order[, i - 1]),
            na.rm = TRUE
          ))
        tech_order[, i] <- rescale((1 / ubiquity) *
          colSums(mat * city_order[, i - 1],
            na.rm = TRUE
          ))
      }
    }
  }

  if (steps == 0) {
    return(diversification)
  }

  return(setNames(city_order[, steps], rownames(city_order)))
}
