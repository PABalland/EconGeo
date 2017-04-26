#' Compute an index of knowledge complexity of regions using the method of reflection
#'
#' This function computes an index of knowledge complexity of regions using the method of reflection from regions - industries (incidence) matrices. The index has been developed by Hidalgo and Hausmann (2009) for country - product matrices and adapted by Balland and Rigby (2016) to city - technology matrices.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param RCA Logical; should the index of relative comparative advantage (RCA - also refered to as location quotient) first be computed? Defaults to FALSE (a binary matrix - 0/1 - is expected as an input), but can be set to TRUE if the index of relative comparative advantage first needs to be computed
#' @param steps Number of iteration steps. Defaults to 20, but can be set to 0 to give diversity (number of industry in which a region has a RCA), to 1 to give  the average ubiquity of the industries in which a region has a RCA, to 2 to give the average diversity of regions that have similar industrial structures, or to any other number of steps < or = to 22. Note that above steps = 2 the index will be rescaled from 0 (minimum relative complexity) to 100 (maximum relative complexity).
#' @keywords complexity
#' @export
#' @examples
#' ## generate a region - industry matrix with full count
#' set.seed(31)
#' mat <- matrix(sample(0:10,20,replace=T), ncol = 4)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' MORc (mat, RCA = TRUE)
#' MORc (mat, RCA = TRUE, steps = 0)
#' MORc (mat, RCA = TRUE, steps = 1)
#' MORc (mat, RCA = TRUE, steps = 2)
#'
#' ## generate a region - industry matrix in which cells represent the presence/absence of a RCA
#' set.seed(32)
#' mat <- matrix(sample(0:1,20,replace=T), ncol = 4)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' MORc (mat)
#' MORc (mat, steps = 0)
#' MORc (mat, steps = 1)
#' MORc (mat, steps = 2)
#'
#' ## generate the simple network of Hidalgo and Hausmann (2009) presented p.11 (Fig. S4)
#' countries <- c("C1", "C1", "C1", "C1", "C2", "C3", "C3", "C4")
#' products <- c("P1","P2", "P3", "P4", "P2", "P3", "P4", "P4")
#' data <- data.frame(countries, products)
#' data$freq <- 1
#' mat <- get.matrix (data)
#'
#' ## run the function
#' MORc (mat)
#' MORc (mat, steps = 0)
#' MORc (mat, steps = 1)
#' MORc (mat, steps = 2)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location.quotient}}, \code{\link{ubiquity}}, \code{\link{diversity}}, \code{\link{KCI}}, \code{\link{TCI}}, \code{\link{MORt}}
#' @references Hidalgo, C. and Hausmann, R. (2009) The building blocks of economic complexity, \emph{Proceedings of the National Academy of Sciences} \strong{106}: 10570 - 10575. \cr
#' \cr
#' Balland, P.A. and Rigby, D. (2017) The Geography of Complex Knowledge, \emph{Economic Geography} \strong{93} (1): 1-23.



"MORc" <- function (mat, RCA = FALSE, steps = 20) {

  # remove null observations
  mat <- mat[rowSums(mat) > 0, ]
  mat <- mat[, colSums(mat) > 0]
  mat

rescale <- function(x) {
   ((x)-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x,na.rm=TRUE)) * 100
   }

  if (RCA) {
    share_tech_city <- mat / rowSums (mat)
    share_tech_total <- colSums (mat) / sum (mat)
    LQ <- t(t(share_tech_city)/ share_tech_total)
    LQ[is.na(LQ)] <- 0
    LQ[LQ < 1] <- 0
    LQ[LQ > 1] <- 1
    mat <- LQ

diversification = rowSums (mat)
ubiquity = colSums (mat)

city_1st_order = (1/diversification) * rowSums (t(t(mat)*ubiquity))
tech_1st_order = (1/ubiquity) * colSums ((mat)*diversification)

city_2nd_order = (1/diversification) * rowSums (t(t(mat)*tech_1st_order))
tech_2nd_order = (1/ubiquity) * colSums ((mat)*city_1st_order)

city_3rd_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_2nd_order)))
tech_3rd_order = rescale((1/ubiquity) * colSums ((mat)*city_2nd_order))

city_4th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_3rd_order), na.rm = TRUE))
tech_4th_order = rescale((1/ubiquity) * colSums ((mat)*city_3rd_order, na.rm = TRUE))

city_5th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_4th_order), na.rm = TRUE))
tech_5th_order = rescale((1/ubiquity) * colSums ((mat)*city_4th_order, na.rm = TRUE))

city_6th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_5th_order), na.rm = TRUE))
tech_6th_order = rescale((1/ubiquity) * colSums ((mat)*city_5th_order, na.rm = TRUE))

city_7th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_6th_order), na.rm = TRUE))
tech_7th_order = rescale((1/ubiquity) * colSums ((mat)*city_6th_order, na.rm = TRUE))

city_8th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_7th_order), na.rm = TRUE))
tech_8th_order = rescale((1/ubiquity) * colSums ((mat)*city_7th_order, na.rm = TRUE))

city_9th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_8th_order), na.rm = TRUE))
tech_9th_order = rescale((1/ubiquity) * colSums ((mat)*city_8th_order, na.rm = TRUE))

city_10th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_9th_order), na.rm = TRUE))
tech_10th_order = rescale((1/ubiquity) * colSums ((mat)*city_9th_order, na.rm = TRUE))

city_11th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_10th_order), na.rm = TRUE))
tech_11th_order = rescale((1/ubiquity) * colSums ((mat)*city_10th_order, na.rm = TRUE))

city_12th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_11th_order), na.rm = TRUE))
tech_12th_order = rescale((1/ubiquity) * colSums ((mat)*city_11th_order, na.rm = TRUE))

city_13th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_12th_order), na.rm = TRUE))
tech_13th_order = rescale((1/ubiquity) * colSums ((mat)*city_12th_order, na.rm = TRUE))

city_14th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_13th_order), na.rm = TRUE))
tech_14th_order = rescale((1/ubiquity) * colSums ((mat)*city_13th_order, na.rm = TRUE))

city_15th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_14th_order), na.rm = TRUE))
tech_15th_order = rescale((1/ubiquity) * colSums ((mat)*city_14th_order, na.rm = TRUE))

city_16th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_15th_order), na.rm = TRUE))
tech_16th_order = rescale((1/ubiquity) * colSums ((mat)*city_15th_order, na.rm = TRUE))

city_17th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_16th_order), na.rm = TRUE))
tech_17th_order = rescale((1/ubiquity) * colSums ((mat)*city_16th_order, na.rm = TRUE))

city_18th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_17th_order), na.rm = TRUE))
tech_18th_order = rescale((1/ubiquity) * colSums ((mat)*city_17th_order, na.rm = TRUE))

city_19th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_18th_order), na.rm = TRUE))
tech_19th_order = rescale((1/ubiquity) * colSums ((mat)*city_18th_order, na.rm = TRUE))

city_20th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_19th_order), na.rm = TRUE))
tech_20th_order = rescale((1/ubiquity) * colSums ((mat)*city_19th_order, na.rm = TRUE))

city_21th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_20th_order), na.rm = TRUE))
tech_21th_order = rescale((1/ubiquity) * colSums ((mat)*city_20th_order, na.rm = TRUE))

city_22th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_21th_order), na.rm = TRUE))
tech_22th_order = rescale((1/ubiquity) * colSums ((mat)*city_21th_order, na.rm = TRUE))


  } else {

diversification = rowSums (mat)
ubiquity = colSums (mat)

city_1st_order = (1/diversification) * rowSums (t(t(mat)*ubiquity))
tech_1st_order = (1/ubiquity) * colSums ((mat)*diversification)

city_2nd_order = (1/diversification) * rowSums (t(t(mat)*tech_1st_order))
tech_2nd_order = (1/ubiquity) * colSums ((mat)*city_1st_order)

city_3rd_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_2nd_order)))
tech_3rd_order = rescale((1/ubiquity) * colSums ((mat)*city_2nd_order))

city_4th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_3rd_order), na.rm = TRUE))
tech_4th_order = rescale((1/ubiquity) * colSums ((mat)*city_3rd_order, na.rm = TRUE))

city_5th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_4th_order), na.rm = TRUE))
tech_5th_order = rescale((1/ubiquity) * colSums ((mat)*city_4th_order, na.rm = TRUE))

city_6th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_5th_order), na.rm = TRUE))
tech_6th_order = rescale((1/ubiquity) * colSums ((mat)*city_5th_order, na.rm = TRUE))

city_7th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_6th_order), na.rm = TRUE))
tech_7th_order = rescale((1/ubiquity) * colSums ((mat)*city_6th_order, na.rm = TRUE))

city_8th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_7th_order), na.rm = TRUE))
tech_8th_order = rescale((1/ubiquity) * colSums ((mat)*city_7th_order, na.rm = TRUE))

city_9th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_8th_order), na.rm = TRUE))
tech_9th_order = rescale((1/ubiquity) * colSums ((mat)*city_8th_order, na.rm = TRUE))

city_10th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_9th_order), na.rm = TRUE))
tech_10th_order = rescale((1/ubiquity) * colSums ((mat)*city_9th_order, na.rm = TRUE))

city_11th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_10th_order), na.rm = TRUE))
tech_11th_order = rescale((1/ubiquity) * colSums ((mat)*city_10th_order, na.rm = TRUE))

city_12th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_11th_order), na.rm = TRUE))
tech_12th_order = rescale((1/ubiquity) * colSums ((mat)*city_11th_order, na.rm = TRUE))

city_13th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_12th_order), na.rm = TRUE))
tech_13th_order = rescale((1/ubiquity) * colSums ((mat)*city_12th_order, na.rm = TRUE))

city_14th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_13th_order), na.rm = TRUE))
tech_14th_order = rescale((1/ubiquity) * colSums ((mat)*city_13th_order, na.rm = TRUE))

city_15th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_14th_order), na.rm = TRUE))
tech_15th_order = rescale((1/ubiquity) * colSums ((mat)*city_14th_order, na.rm = TRUE))

city_16th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_15th_order), na.rm = TRUE))
tech_16th_order = rescale((1/ubiquity) * colSums ((mat)*city_15th_order, na.rm = TRUE))

city_17th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_16th_order), na.rm = TRUE))
tech_17th_order = rescale((1/ubiquity) * colSums ((mat)*city_16th_order, na.rm = TRUE))

city_18th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_17th_order), na.rm = TRUE))
tech_18th_order = rescale((1/ubiquity) * colSums ((mat)*city_17th_order, na.rm = TRUE))

city_19th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_18th_order), na.rm = TRUE))
tech_19th_order = rescale((1/ubiquity) * colSums ((mat)*city_18th_order, na.rm = TRUE))

city_20th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_19th_order), na.rm = TRUE))
tech_20th_order = rescale((1/ubiquity) * colSums ((mat)*city_19th_order, na.rm = TRUE))

city_21th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_20th_order), na.rm = TRUE))
tech_21th_order = rescale((1/ubiquity) * colSums ((mat)*city_20th_order, na.rm = TRUE))

city_22th_order = rescale((1/diversification) * rowSums (t(t(mat)*tech_21th_order), na.rm = TRUE))
tech_22th_order = rescale((1/ubiquity) * colSums ((mat)*city_21th_order, na.rm = TRUE))


}

 if (steps == 0) {
return (diversification)
}

if (steps == 1) {
return (city_1st_order)
}

if (steps == 2) {
return (city_2nd_order)
}

if (steps == 3) {
return (city_3rd_order)
}

if (steps == 4) {
return (city_4th_order)
}

if (steps == 5) {
return (city_5th_order)
}

if (steps == 6) {
return (city_6th_order)
}

if (steps == 7) {
return (city_7th_order)
}

if (steps == 8) {
return (city_8th_order)
}

if (steps == 9) {
return (city_9th_order)
}

if (steps == 10) {
return (city_10th_order)
}

if (steps == 11) {
return (city_11th_order)
}

if (steps == 12) {
return (city_12th_order)
}

if (steps == 13) {
return (city_13th_order)
}

if (steps == 14) {
return (city_14th_order)
}

if (steps == 15) {
return (city_15th_order)
}

if (steps == 16) {
return (city_16th_order)
}

if (steps == 17) {
return (city_17th_order)
}

if (steps == 18) {
return (city_18th_order)
}

if (steps == 19) {
return (city_19th_order)
}

if (steps == 20) {
return (city_20th_order)
}

if (steps == 21) {
return (city_21th_order)
}

 if (steps == 22) {
return (city_22th_order)
}

}


