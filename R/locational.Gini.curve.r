#' Plot a locational Gini curve from regions - industries matrices
#'
#' This function plots a locational Gini curve following Krugman from regions - industries matrices.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100,20,replace=T), ncol = 4)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' locational.Gini.curve (mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{locational.Gini}}
#' @references Krugman P. (1991) \emph{Geography and Trade}, MIT Press, Cambridge (chapter 2 - p.56)


locational.Gini.curve <- function (mat, col = 1) {

  share_city_total = rowSums (mat) / sum (mat)
  ind <- mat[,col]
  pop <- share_city_total
  o = ind/pop
  o[is.na(o)] = 0
  oind <- order(o)
  ind <- ind[oind]
  pop <- pop[oind]
  ind <- c(0, ind)
  pop <- c(0, pop)
  cind <- cumsum(ind)/max(cumsum(ind))
  cpop <- cumsum(pop)/max(cumsum(pop))
  plot(cpop, cind, type = "l", main = "Locational Gini curve", xlab = "Cumulative distribution of total industrial shares",
       ylab = "Cumulative distribution shares in the focal industry",
       xlim = c(0, 1), ylim = c(0, 1))
  return(abline(0, 1, col = "red"))
}


