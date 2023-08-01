#' Plot a Lorenz curve from regional industrial counts
#'
#' This function plots a Lorenz curve from regional industrial counts. This curve gives an indication of the unequal distribution of an industry accross regions.
#' @param mat An incidence matrix with regions in rows and industries in columns. The input can also be a vector of industrial regional count (a matrix with n regions in rows and a single column).
#' @param pdf Logical; shall a pdf be saved?  Defaults to FALSE. If set to TRUE, a pdf with all will be compiled and saved to R's temp dir if no 'pdf_location' is specified.
#' @param pdf_location Output location of pdf file
#' @param plot Logical; shall the curve be automatically plotted? Defaults to TRUE. If set to TRUE, the function will return x y coordinates that you can latter use to plot and customize the curve.
#' @return If `plot = FALSE`, the function returns a list with two components:
#'   - `cum.reg`: A vector of cumulative proportions of regions.
#'   - `cum.out`: A vector of cumulative proportions of industrial output.
#' If `plot = TRUE`, the function generates a plot of the Lorenz curve and does not return a value.
#' @keywords concentration inequality
#' @export
#' @examples
#' ## generate vectors of industrial count
#' ind <- c(0, 10, 10, 30, 50)
#'
#' ## run the function
#' lorenz_curve (ind)
#' lorenz_curve (ind, plot = FALSE)
#'
#' ## generate a region - industry matrix
#' mat = matrix (
#' c (0, 1, 0, 0,
#' 0, 1, 0, 0,
#' 0, 1, 0, 0,
#' 0, 1, 0, 1,
#' 0, 1, 1, 1), ncol = 4, byrow = TRUE)
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' lorenz_curve (mat)
#' lorenz_curve (mat, plot = FALSE)
#'
#' ## run the function by aggregating all industries
#' lorenz_curve (rowSums(mat))
#' lorenz_curve (rowSums(mat), plot = FALSE)
#'
#' ## run the function for industry #1 only (perfect equality)
#' lorenz_curve (mat[,1])
#' lorenz_curve (mat[,1], plot = FALSE)
#'
#' ## run the function for industry #2 only (perfect equality)
#' lorenz_curve (mat[,2])
#' lorenz_curve (mat[,2], plot = FALSE)
#'
#' ## run the function for industry #3 only (perfect unequality)
#' lorenz_curve (mat[,3])
#' lorenz_curve (mat[,3], plot = FALSE)
#'
#' ## run the function for industry #4 only (top 40% produces 100% of the output)
#' lorenz_curve (mat[,4])
#' lorenz_curve (mat[,4], plot = FALSE)
#'
#' ## Compare the distribution of the #industries
#' oldpar <- par(mfrow = c(2, 2))  # Save the current graphical parameter settings
#' lorenz_curve (mat[,1])
#' lorenz_curve (mat[,2])
#' lorenz_curve (mat[,3])
#' lorenz_curve (mat[,4])
#' par(oldpar)  # Reset the graphical parameters to their original values
#'
#' ## Save output as pdf
#' lorenz_curve (mat, pdf = TRUE)
#'
#' ## To specify an output directory for the pdf,
#' ## specify 'pdf_location', for instance as '/Users/jones/lorenz_curve.pdf'
#' ## lorenz_curve(mat, pdf = TRUE, pdf_location = '/Users/jones/lorenz_curve.pdf')
#'
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{hoover_gini}}, \code{\link{locational_gini}}, \code{\link{locational_gini_curve}}, \code{\link{hoover_curve}}, \code{\link{gini}}
#' @references Lorenz, M. O. (1905) Methods of measuring the concentration of wealth, \emph{Publications of the American Statistical Association} \strong{9}: 209–219

lorenz_curve <- function(mat, plot = TRUE, pdf = TRUE, pdf_location = NULL) {
  if (!plot) {
    mat <- as.matrix(mat)

    x <- mat[, 1]
    x <- x[complete.cases(x)]
    weights <- rep(1, length = length(x))
    ox <- order(x)
    x <- x[ox]
    weights <- weights[ox] / sum(weights)
    p <- cumsum(weights)
    nu <- cumsum(weights * x)
    n <- length(nu)
    nu <- nu / nu[n]
    p <- c(0, p)
    nu <- c(0, nu)
    return(list(cum.reg = p, cum.out = nu))
  }

  if (plot) {
    mat <- as.matrix(mat)

    hc <- function(mat, col = 1) {
      x <- mat[, col]
      x <- x[complete.cases(x)]
      weights <- rep(1, length = length(x))
      ox <- order(x)
      x <- x[ox]
      weights <- weights[ox] / sum(weights)
      p <- cumsum(weights)
      nu <- cumsum(weights * x)
      n <- length(nu)
      nu <- nu / nu[n]
      p <- c(0, p)
      nu <- c(0, nu)
      plot(p, nu,
        type = "l", main = paste0("Lorenz curve ", colnames(mat)[col]),
        xlab = "Cumulative proportion of regions",
        ylab = "Cumulative proportion of industrial output",
        xlim = c(0, 1), ylim = c(0, 1)
      )
      return(abline(0, 1, col = "red"))
    }

    if (!pdf) {
      for (i in seq_len(ncol(mat))) {
        hc(mat, i)
      }
    } else {
      if (!is.null(pdf_location)) {
        pdf(pdf_location)
        message("Lorenz curve PDF saved to:", pdf_location)
      } else {
        pdf_location <- file.path(tempdir(), "lorenz_curve.pdf")
        pdf(pdf_location)
        message("No 'pdf_location' specified: lorenz_curve.pdf saved to R's temporary directory.")
      }
      for (i in seq_len(ncol(mat))) {
        hc(mat, i)
      }
      dev.off()
    }
  }
}
