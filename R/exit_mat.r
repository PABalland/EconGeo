#' Generate a matrix of exit events from two regions - industries matrices (same matrix composition from two different periods)
#'
#' This function generates a matrix of exit events from two regions - industries matrices (different matrix compositions are allowed)
#' @param mat1 An incidence matrix with regions in rows and industries in columns (period 1)
#' @param mat2 An incidence matrix with regions in rows and industries in columns (period 2)
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
#' mat2[2, 1] <- 0
#'
#'
#' ## run the function
#' exit_mat(mat1, mat2)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl} \cr
#' Wolf-Hendrik Uhlbach \email{w.p.uhlbach@students.uu.nl}
#' @seealso \code{\link{growth_list}}, \code{\link{exit_list}}, \code{\link{entry_list}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

exit_mat <- function(mat1, mat2) {
  a <- list(mat1, mat2)
  cols <- sort(unique(unlist(lapply(a, colnames))))
  rows <- sort(unique(unlist(lapply(a, rownames))))
  unimat <- array(0,
    dim = c(
      length(rows),
      length(cols)
    ),
    dimnames = list(rows, cols)
  )

  unimat1 <- unimat
  col <- colnames(unimat1)[colnames(unimat1) %in% colnames(mat1)]
  row <- rownames(unimat1)[rownames(unimat1) %in% rownames(mat1)]
  unimat1[row, col] <- mat1[row, col]
  unimat1

  unimat2 <- unimat
  col <- colnames(unimat2)[colnames(unimat2) %in% colnames(mat2)]
  row <- rownames(unimat2)[rownames(unimat2) %in% rownames(mat2)]
  unimat2[row, col] <- mat2[row, col]
  unimat2

  unimat1[unimat1 == 0] <- NA
  exit <- unimat1 + unimat2
  exit[exit == 2] <- 0
  return(exit)
}
