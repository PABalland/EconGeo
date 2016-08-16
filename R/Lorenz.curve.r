#' Plot a Lorenz curve from regional industrial counts
#'
#' This function plots a Lorenz curve from regional industrial counts. This curve gives an indication of the unequal distribution of an industry accross regions.
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pdf Logical; shall a pdf be saved to your current working directory? Defaults to FALSE. If set to TRUE, a pdf with all Lorenz curves will be compiled and saved to your current working directory.
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial count
#' ind <- c(0, 10, 10, 30, 50)
#'
#' ## run the function
#' Lorenz.curve (ind)
#' Lorenz.curve (ind, pdf = TRUE)
#'
#' ## generate a region - industry matrix
#' mat = matrix (
#' c (0, 1, 0, 0,
#' 0, 1, 0, 0,
#' 0, 1, 0, 0,
#' 0, 1, 0, 1,
#' 0, 1, 1, 1), ncol = 4, byrow = T)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' Lorenz.curve (mat)
#' Lorenz.curve (mat, pdf = TRUE)
#'
#' ## run the function by aggregating all industries
#' Lorenz.curve (rowSums(mat))
#' Lorenz.curve (rowSums(mat))
#'
#' ## run the function for industry #1 only (perfect equality)
#' Lorenz.curve (mat[,1])
#' Lorenz.curve (mat[,1], pdf = TRUE)
#'
#' ## run the function for industry #2 only (perfect equality)
#' Lorenz.curve (mat[,2])
#' Lorenz.curve (mat[,2], pdf = TRUE)
#'
#' ## run the function for industry #3 only (perfect unequality)
#' Lorenz.curve (mat[,3])
#' Lorenz.curve (mat[,3], pdf = TRUE)
#'
#' ## run the function for industry #4 only (top 40% produces 100% of the output)
#' Lorenz.curve (mat[,4])
#' Lorenz.curve (mat[,4], pdf = TRUE)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{Hoover.Gini}}, \code{\link{locational.Gini}}, \code{\link{locational.Gini.curve}}, \code{\link{Hoover.curve}}, \code{\link{Gini}}
#' @references Lorenz, M. O. (1905) Methods of measuring the concentration of wealth, \emph{Publications of the American Statistical Association} \strong{9}: 209–219

Lorenz.curve <- function(mat, pdf = FALSE) {

  mat = ind[complete.cases (mat)]
  mat = as.matrix (mat)

  HC <- function(mat, col = 1) {

    x <- mat[,col]
    weights = rep(1, length = length(x))
    ox <- order(x)
    x <- x[ox]
    weights <- weights[ox]/sum(weights)
    p <- cumsum(weights)
    nu <- cumsum(weights * x)
    n <- length(nu)
    nu <- nu/nu[n]
    p <- c(0,p)
    nu <- c(0,nu)
    plot (p, nu, type = "l", main = paste0("Lorenz curve ", colnames(mat)[col]),
          xlab="Cumulative proportion of regions", ylab="Cumulative proportion of industrial output",
          xlim=c(0, 1), ylim=c(0, 1))
    return(abline (0,1, col = "red"))

  }

  if (!pdf) {

    for (i in unique(1:ncol(mat)))
    {
      HC(mat, i)
    }


  } else {

    pdf("Lorenz.curve.pdf")
    for (i in unique(1:ncol(mat)))
    {
      HC(mat, i)
    }
    dev.off()
    print ("Lorenz.curve.pdf has been saved to your current working directory")

  }

}


