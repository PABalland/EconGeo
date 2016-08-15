#' Compute the Hoover curve from regional counts in population and industry
#'
#' This function computes the Hoover curve from regional shares in population and industry
#' @param mat An incidence matrix with regions in rows and industries in columns or a vector of industrial regional count
#' @param pop A vector of population regional count
#' @param pdf Logical; shall a pdf be saved to your current working directory? Defaults to FALSE
#' @param sum Logical; shall the function use the sums of the rows or instead of plotting a curve for each industry? Defaults to FALSE
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(5, 10, 10, 25, 30)
#'
#' ## run the function
#' Hoover.curve (ind, pop)
#' Hoover.curve (ind, pop, pdf = TRUE)
#'
#' ## generate a region - industry matrix
#' set.seed(31)
#' mat <- matrix(sample(0:100,20,replace=T), ncol = 4)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' Hoover.curve (mat, pop)
#' Hoover.curve (mat, pop, pdf = TRUE)
#'
#' ## run the function by aggregating all industries
#' Hoover.curve (mat, pop, sum = TRUE)
#' Hoover.curve (mat, pop, sum = TRUE, pdf = TRUE)
#'
#' ## run the function for industry #2 only
#' Hoover.curve (mat[,2], pop)
#' Hoover.curve (mat[,2], pop, pdf = TRUE)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location.quotient}}, \code{\link{locational.Gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171

Hoover.curve <- function(mat, pop, pdf = FALSE, sum = FALSE) {

  mat = as.matrix (mat)

  if (sum) {
    mat = as.matrix (mat)
    mat <- rowSums (mat)
    mat = as.matrix (mat)
  }

  HC <- function(mat, pop, col = 1) {

      ind <- c(0, mat[,col])
      pop <- c(0, pop)
      o = ind/pop
      o[is.na(o)] = 0
      oind <- order(o)
      ind <- ind[oind]
      pop <- pop[oind]
      cind <- cumsum(ind)/max(cumsum(ind))
      cpop <- cumsum(pop)/max(cumsum(pop))
      plot (cpop, cind, type = "l", main = paste0("Hoover curve ", colnames(mat)[col]),
            xlab="Cumulative distribution of population shares", ylab="Cumulative distribution of industry shares",
            xlim=c(0, 1), ylim=c(0, 1))
      return(abline (0,1, col = "red"))

  }

  if (!pdf) {

    for (i in unique(1:ncol(mat)))
    {
      HC(mat, pop, i)
    }


  } else {

    pdf("Hoover.curve.pdf")
    for (i in unique(1:ncol(mat)))
    {
      HC(mat, pop, i)
    }
    dev.off()
    print ("Hoover.curve.pdf has been saved to your current working directory")

  }

}


