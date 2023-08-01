#' Generate a data frame of exit events from multiple regions - industries matrices (same matrix composition for the different periods)
#'
#' This function generates a data frame of exit events from multiple regions - industries matrices (different matrix compositions are allowed). In this function, the maximum number of periods is limited to 20.
#' @param ... Incidence matrices with regions in rows and industries in columns (period ... - optional)
#' @return A data frame representing the exit events from multiple regions - industries matrices, with columns "region" (representing the region), "industry" (representing the industry), "exit" (representing the exit event), and "period" (representing the period)
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
#' ## run the function
#' exit_list(mat1, mat2)
#'
#' ## generate a third region - industry matrix in which cells represent the presence/absence
#' ## of a RCA (period 3)
#' mat3 <- mat2
#' mat3[5, 1] <- 0
#'
#' ## run the function
#' exit_list(mat1, mat2, mat3)
#'
#' ## generate a fourth region - industry matrix in which cells represent the presence/absence
#' ## of a RCA (period 4)
#' mat4 <- mat3
#' mat4[5, 3] <- 0
#'
#' ## run the function
#' exit_list(mat1, mat2, mat3, mat4)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl} \cr
#' Wolf-Hendrik Uhlbach \email{w.p.uhlbach@students.uu.nl}
#' @seealso \code{\link{entry_list}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

exit_list <- function(...) {
  mats <- list(...)
  num_mats <- length(mats)

  cols <- sort(unique(unlist(lapply(mats, colnames))))
  rows <- sort(unique(unlist(lapply(mats, rownames))))
  unimat <- array(0,
                  dim = c(length(rows),
                          length(cols)),
                  dimnames = list(rows, cols))

  create_exit <- function(bim1, bim2, period) {
    bim1[bim1 == 0] <- NA
    exit <- bim2 + bim1
    exit[exit == 2] <- 0
    exit <- get_list(exit)
    colnames(exit) <- c("region", "industry", "exit")
    exit$period <- period
    return(exit)
  }

  exits <- list()
  period <- 2

  for (i in seq(2, num_mats, by = 1)) {
    bim1 <- mats[[i - 1]]
    bim2 <- mats[[i]]

    unimat1 <- unimat
    col <- colnames(unimat1)[colnames(unimat1) %in% colnames(bim1)]
    row <- rownames(unimat1)[rownames(unimat1) %in% rownames(bim1)]
    unimat1[row, col] <- bim1[row, col]
    bim1 <- unimat1

    unimat2 <- unimat
    col <- colnames(unimat2)[colnames(unimat2) %in% colnames(bim2)]
    row <- rownames(unimat2)[rownames(unimat2) %in% rownames(bim2)]
    unimat2[row, col] <- bim2[row, col]
    bim2 <- unimat2

    exit <- create_exit(bim1, bim2, period)
    exits[[period]] <- exit

    period <- period + 1
  }

  return(do.call(rbind, exits))
}
