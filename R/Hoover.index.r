#' Compute the Hoover index
#'
#' This function computes the Hoover index, named after Hedgar Hoover. The Hoover index is a measure of spatial inequality. It ranges from 0 (perfect equality) to 100 (perfect inequality) and is calculated from the Lorenz curve associated with a given distribution of population, industries or technologies. In this sense, it is closely related to the Gini coefficient. The Hoover index represents the maximum vertical distance between the Lorenz curve and the 45 degree line of perfect spatial equality. It indicates the proportion of industries, jobs, or population needed to be transferred from the top to the bottom of the distribution to achieve perfect spatial equality. The Hoover index is also known as the Robin Hood index in studies of income inequality.\cr
#' \cr
#' Computation of the Hoover index: \eqn{H=1/2\sum _{ i=1 }^{ N }{ \left| \frac { { E }_{ i } }{ { E }_{ total } } -\frac { { A }_{ i } }{ { A }_{ total } }  \right|  }  }
#'
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pop A vector of population regional count
#' @param pdf Logical; shall a pdf be saved to your current working directory? Defaults to FALSE. If set to TRUE, a pdf with all Hoover indices will be compiled and saved to your current working directory.
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(10, 15, 20, 25, 30)
#'
#' ## run the function (30% of the population produces 50% of the industrial output)
#' Hoover.index (ind, pop)
#'
#' ## generate a region - industry matrix
#' mat = matrix (
#' c (0, 10, 0, 0,
#' 0, 15, 0, 0,
#' 0, 20, 0, 0,
#' 0, 25, 0, 1,
#' 0, 30, 1, 1), ncol = 4, byrow = T)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' Hoover.index (mat, pop)
#'
#' ## run the function by aggregating all industries
#' Hoover.index (rowSums(mat), pop)
#'
#' ## run the function for industry #1 only
#' Hoover.index (mat[,1], pop)
#'
#' ## run the function for industry #2 only (perfectly proportional to population)
#' Hoover.index (mat[,2], pop)
#'
#' ## run the function for industry #3 only (30% of the pop. produces 100% of the output)
#' Hoover.index (mat[,3], pop)
#'
#' ## run the function for industry #4 only (55% of the pop. produces 100% of the output)
#' Hoover.index (mat[,4], pop)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{Hoover.curve}}, \code{\link{Hoover.Gini}}, \code{\link{locational.Gini}}, \code{\link{locational.Gini.curve}}, \code{\link{Lorenz.curve}}, \code{\link{Gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171


Hoover.index <- function (mat, pop) {

  mat = as.matrix (mat)

  HG <- function(mat, pop, col = 1) {

    ind <- mat[,col]
    pop <- pop
    c = data.frame (ind, pop)
    c = c[complete.cases(c),]
    ind = c$ind
    pop = c$pop
    ind = ind/sum(ind)
    pop = pop/sum(pop)
    s = abs (ind - pop)
    s = sum (s)
    s = round ((s/2), digits = 2)
    s = s*100

  }

  if (ncol(mat) == 1) {
    x = HG (mat, pop)
  } else {

  y  <- NULL;
  for (i in unique(1:ncol(mat)))
  {
    tmp <- HG (mat, pop, i)
    y <- rbind(y, tmp)
  }

  x = data.frame (colnames (mat),  y[,1])
  colnames (x) = c ("Industry", "Hoover.index")

  }

  return (x)

}



