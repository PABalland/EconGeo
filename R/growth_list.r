#' Generate a data frame of industrial growth in regions from multiple regions - industries matrices (same matrix composition for the different periods)
#'
#' This function generates a data frame of industrial growth in regions from multiple regions - industries matrices (same matrix composition for the different periods). In this function, the maximum number of periods is limited to 20.
#' @param ... Incidence matrices with regions in rows and industries in columns (period ... - optional)
#' @keywords growth
#' @export
#' @examples
#' ## generate a first region - industry matrix with full count (period 1)
#' set.seed(31)
#' mat1 <- matrix(sample(0:10, 20, replace = TRUE), ncol = 4)
#' rownames(mat1) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c("I1", "I2", "I3", "I4")
#'
#' ## generate a second region - industry matrix with full count (period 2)
#' mat2 <- mat1
#' mat2[3, 1] <- 8
#'
#' ## run the function
#' growth_list(mat1, mat2)
#'
#' ## generate a third region - industry matrix with full count (period 3)
#' mat3 <- mat2
#' mat3[5, 2] <- 1
#'
#' ## run the function
#' growth_list(mat1, mat2, mat3)
#'
#' ## generate a fourth region - industry matrix with full count (period 4)
#' mat4 <- mat3
#' mat4[5, 4] <- 1
#'
#' ## run the function
#' growth_list(mat1, mat2, mat3, mat4)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{exit_list}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

growth_list <- function(...) {
  mats <- list(...)
  num_mats <- length(mats)

  cols <- sort(unique(unlist(lapply(mats, colnames))))
  rows <- sort(unique(unlist(lapply(mats, rownames))))
  unimat <- array(0,
    dim = c(
      length(rows),
      length(cols)
    ),
    dimnames = list(rows, cols)
  )

  create_growth <- function(bim1, bim2, period) {
    growth <- ((bim2 - bim1) / bim1) * 100
    growth[is.nan(growth)] <- 0
    growth <- get_list(growth)
    colnames(growth) <- c("region", "industry", "growth")
    growth$period <- period
    return(growth)
  }

  growths <- list()
  period <- 2

  for (i in seq(2, num_mats, by = 1)) {
    bim1 <- get(paste0("mat", i - 1))
    bim2 <- get(paste0("mat", i))

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

    growth <- create_growth(bim1, bim2, period)
    growths[[period]] <- growth

    period <- period + 1
  }

  return(do.call(rbind, growths))
}
