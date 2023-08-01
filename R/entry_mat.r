#' Generate a matrix of entry events from two regions - industries matrices (same matrix composition from two different periods)
#'
#' This function generates a matrix of entry events from two regions - industries matrices (different matrix compositions are allowed)
#' @param mat1 An incidence matrix with regions in rows and industries in columns (period 1)
#' @param mat2 An incidence matrix with regions in rows and industries in columns (period 2)
#' @return A matrix representing the entry events from two regions - industries matrices, with rows representing regions and columns representing industries
#' @keywords diversification
#' @export
#' @examples
#' ## generate a first region - industry matrix in which cells represent the presence/absence
#' ## of a RCA (period 1)
#' set.seed(31)
#' mat1 <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
#' rownames(mat1) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c("I1", "I2", "I3", "I4")
#'
#' ## generate a second region - industry matrix in which cells represent the presence/absence
#' ## of a RCA (period 2)
#' mat2 <- mat1
#' mat2[3, 1] <- 1
#'
#'
#' ## run the function
#' entry_mat(mat1, mat2)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl} \cr
#' Wolf-Hendrik Uhlbach \email{w.p.uhlbach@students.uu.nl}
#' @seealso \code{\link{growth_list}}, \code{\link{entry_list}}, \code{\link{exit_list}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

entry_mat <- function(mat1, mat2) {
  mat1a <- mat1
  mat1a[mat1a == 1] <- NA
  a <- list(mat1a, mat2)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  entry <- array(0,
                 dim = c(length(rows),
                         length(cols)),
                 dimnames = list(rows, cols))
  for (M in a) {
    entry[rownames(M), colnames(M)] <- entry[rownames(M), colnames(M)] + M
  }
  return(entry)
}
