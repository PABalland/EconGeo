#' Generate a matrix of industrial growth in regions from two regions - industries matrices (same matrix composition from two different periods)
#'
#' This function generates a matrix of industrial growth in regions from two regions - industries matrices (same matrix composition from two different periods)
#' @param mat1 An incidence matrix with regions in rows and industries in columns (period 1)
#' @param mat2 An incidence matrix with regions in rows and industries in columns (period 2)
#' @keywords growth
#' @export
#' @examples
#' ## generate a first region - industry matrix with full count (period 1)
#' set.seed(31)
#' mat1 <- matrix(sample(0:10,20,replace=T), ncol = 4)
#' rownames(mat1) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c ("I1", "I2", "I3", "I4")
#'
#' ## generate a second region - industry matrix with full count (period 2)
#' mat2 <- mat1
#' mat2[3,1] <- 8
#'
#'
#' ## run the function
#' growth.mat (mat1, mat2)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{exit}}, \code{\link{entry.list}}, \code{\link{exit.list}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

"growth.mat" <- function(mat1, mat2) {

  growth = ((mat2 - mat1)/mat1)*100
  growth[is.nan(growth)] <- 0
  growth = round (growth, digits = 2)

return (growth)
}



