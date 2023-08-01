#' Plot a Hoover curve from regions - industries matrices
#'
#' This function plots a Hoover curve from regions - industries matrices.
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pop A vector of population regional count
#' @param plot Logical; shall the curve be automatically plotted? Defaults to TRUE. If set to TRUE, the function will return x y coordinates that you can latter use to plot and customize the curve.
#' @param pdf Logical; shall a pdf be saved to your current working directory? Defaults to FALSE. If set to TRUE, a pdf with all Hoover curves will be compiled and saved to your current working directory.
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial and population count
#' ind <- c(0, 10, 10, 30, 50)
#' pop <- c(10, 15, 20, 25, 30)
#'
#' ## run the function (30% of the population produces 50% of the industrial output)
#' hoover_curve (ind, pop)
#' hoover_curve (ind, pop, pdf = FALSE)
#' hoover_curve (ind, pop, plot = FALSE)
#'
#' ## generate a region - industry matrix
#' mat = matrix (
#' c (0, 10, 0, 0,
#' 0, 15, 0, 0,
#' 0, 20, 0, 0,
#' 0, 25, 0, 1,
#' 0, 30, 1, 1), ncol = 4, byrow = TRUE)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' hoover_curve (mat, pop)
#' hoover_curve (mat, pop, pdf = FALSE)
#' hoover_curve (mat, pop, plot = FALSE)
#'
#' ## run the function by aggregating all industries
#' hoover_curve (rowSums(mat), pop)
#' hoover_curve (rowSums(mat), pop, pdf = FALSE)
#' hoover_curve (rowSums(mat), pop, plot = FALSE)
#'
#' ## run the function for industry #1 only
#' hoover_curve (mat[,1], pop)
#' hoover_curve (mat[,1], pop, pdf = FALSE)
#' hoover_curve (mat[,1], pop, plot = FALSE)
#'
#' ## run the function for industry #2 only (perfectly proportional to population)
#' hoover_curve (mat[,2], pop)
#' hoover_curve (mat[,2], pop, pdf = FALSE)
#' hoover_curve (mat[,2], pop, plot = FALSE)
#'
#' ## run the function for industry #3 only (30% of the pop. produces 100% of the output)
#' hoover_curve (mat[,3], pop)
#' hoover_curve (mat[,3], pop, pdf = FALSE)
#' hoover_curve (mat[,3], pop, plot = FALSE)
#'
#' ## run the function for industry #4 only (55% of the pop. produces 100% of the output)
#' hoover_curve (mat[,4], pop)
#' hoover_curve (mat[,4], pop, pdf = FALSE)
#' hoover_curve (mat[,4], pop, plot = FALSE)
#'
#' ## Compare the distribution of the #industries
#' par(mfrow=c(2,2))
#' hoover_curve (mat[,1], pop)
#' hoover_curve (mat[,2], pop)
#' hoover_curve (mat[,3], pop)
#' hoover_curve (mat[,4], pop)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{hoover_gini}}, \code{\link{locational_gini}}, \code{\link{locational_gini_curve}}, \code{\link{lorenz_curve}}, \code{\link{gini}}
#' @references Hoover, E.M. (1936) The Measurement of Industrial Localization, \emph{The Review of Economics and Statistics} \strong{18} (1): 162-171

hoover_curve <- function(mat, pop, plot = TRUE, pdf = FALSE) {
  if (!plot) {
    mat <- as.matrix(mat)
    ind <- c(0, mat[, 1])
    pop <- c(0, pop)
    c <- data.frame(ind, pop)
    c <- c[complete.cases(c), ]
    ind <- c$ind
    pop <- c$pop
    o <- ind / pop
    o[is.na(o)] <- 0
    oind <- order(o)
    ind <- ind[oind]
    pop <- pop[oind]
    cind <- cumsum(ind) / max(cumsum(ind))
    cpop <- cumsum(pop) / max(cumsum(pop))
    return(list(cum.reg = cpop, cum.out = cind))
  }

  if (plot) {
    mat <- as.matrix(mat)

    hc <- function(mat, pop, col = 1) {
      ind <- c(0, mat[, col])
      pop <- c(0, pop)
      c <- data.frame(ind, pop)
      c <- c[complete.cases(c), ]
      ind <- c$ind
      pop <- c$pop
      o <- ind / pop
      o[is.na(o)] <- 0
      oind <- order(o)
      ind <- ind[oind]
      pop <- pop[oind]
      cind <- cumsum(ind) / max(cumsum(ind))
      cpop <- cumsum(pop) / max(cumsum(pop))
      plot(cpop, cind,
        type = "l", main = paste0("Hoover curve ", colnames(mat)[col]),
        xlab = "Cumulative distribution of population shares",
        ylab = "Cumulative distribution of industry shares",
        xlim = c(0, 1), ylim = c(0, 1)
      )
      return(abline(0, 1, col = "red"))
    }

    if (!pdf) {
      for (i in seq_len(ncol(mat))) {
        hc(mat, pop, i)
      }
    } else {
      pdf("hoover_curve.pdf")
      for (i in seq_len(ncol(mat))) {
        hc(mat, pop, i)
      }
      dev.off()
      print("hoover_curve.pdf has been saved to your current working directory")
    }
  }
}
