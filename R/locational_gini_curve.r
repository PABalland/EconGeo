#' Plot a locational Gini curve from regions - industries matrices
#'
#' This function plots a locational Gini curve following Krugman from regions - industries matrices.
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pdf Logical; shall a pdf be saved to your current working directory? Defaults to FALSE. If set to TRUE, a pdf with all locational Gini curves will be compiled and saved to your current working directory.
#' @keywords concentration inequality
#' @export
#' @examples
#'
#' ## generate a region - industry matrix
#' mat <- matrix(
#'   c(
#'     100, 0, 0, 0, 0,
#'     0, 15, 5, 70, 10,
#'     0, 20, 10, 20, 50,
#'     0, 25, 30, 5, 40,
#'     0, 40, 55, 5, 0
#'   ),
#'   ncol = 5, byrow = TRUE
#' )
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4", "I5")
#'
#' ## run the function (shows industry #5)
#' locational_gini_curve(mat, pdf = FALSE)
#' locational_gini_curve(mat, pdf = FALSE)
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{hoover_gini}}, \code{\link{locational_gini}}, \code{\link{hoover_curve}}, \code{\link{lorenz_curve}}, \code{\link{gini}}
#' @references Krugman P. (1991) \emph{Geography and Trade}, MIT Press, Cambridge (chapter 2 - p.56)


locational_gini_curve <- function(mat, pdf = FALSE) {
  mat <- as.matrix(mat)

  lgc <- function(mat, col = 1) {
    share_city_total <- rowSums(mat) / sum(mat)
    ind <- mat[, col]
    pop <- share_city_total
    o <- ind / pop
    o[is.na(o)] <- 0
    oind <- order(o)
    ind <- ind[oind]
    pop <- pop[oind]
    ind <- c(0, ind)
    pop <- c(0, pop)
    cind <- cumsum(ind) / max(cumsum(ind))
    cpop <- cumsum(pop) / max(cumsum(pop))
    plot(cpop, cind,
      type = "l", main = paste0(
        "Locational Gini curve ",
        colnames(mat)[col]
      ),
      xlab = "Cumulative distribution of total industrial shares",
      ylab = "Cumulative distribution shares in the focal industry",
      xlim = c(0, 1), ylim = c(0, 1)
    )
    return(abline(0, 1, col = "red"))
  }

  if (!pdf) {
    for (i in seq_len(ncol(mat))) {
      lgc(mat, i)
    }
  } else {
    pdf("locational_gini_curve.pdf")
    for (i in seq_len(ncol(mat))) {
      lgc(mat, i)
    }
    dev.off()
    print("locational_gini_curve.pdf saved to current working directory")
  }
}
