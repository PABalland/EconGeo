#' Compute the z-score between technologies from an incidence matrix
#'
#' This function computes the z-score between pairs of technologies from a patent-technology incidence matrix. The z-score is a measure to analyze the co-occurrence of technologies in patent documents (i.e. knowledge combination). It compares the observed number of co-occurrences to what would be expected under the hypothesis that combination is random. A positive z-score indicates a typical co-occurrence which has occurred multiple times before. In contrast, a negative z-socre indicates an atypical co-occurrence. The z-score has been used to estimate the degree of novelty of patents (Kim 2016), scientific publications (Uzzi et al. 2013) or the relatedness between industries (Teece et al. 1994).
#' @param mat A patent-technology incidence matrix with patents in rows and technologies in columns
#' @keywords relatedness
#' @export
#' @examples
#'
#' ## Generate a toy incidence matrix
#' set.seed(2210)
#' techs <- paste0("T", seq(1, 5))
#' techs <- sample(techs, 50, replace = TRUE)
#' patents <- paste0("P", seq(1, 20))
#' patents <- sort(sample(patents, 50, replace = TRUE))
#' dat <- data.frame(patents, techs)
#' dat <- unique(dat)
#' mat <- as.matrix(table(dat$patents, dat$techs))
#'
#' ## run the function
#' zScore(mat)
#'
#' @author Lars Mewes \email{mewes@wigeo.uni-hannover.de}
#' @seealso \code{\link{relatedness.density}}, \code{\link{co.occurence}}
#' @references Kim, D., Cerigo, D. B., Jeong, H., and Youn, H. (2016). Technological novelty proile and invention's future impact. \emph{EPJ Data Science}, \bold{5} (1):1--15 \cr
#' \cr
#' Teece, D. J., Rumelt, R., Dosi, G., and Winter, S. (1994). Understanding corporate coherence. Theory and evidence. \emph{Journal of Economic Behavior and Organization}, \bold{23} (1):1--30 \cr
#' \cr
#' Uzzi, B., Mukherjee, S., Stringer, M., and Jones, B. (2013). Atypical Combinations and Scientific Impact. \emph{Science}, \bold{342} (6157):468--472

"zScore" <- function(mat) {
  
  # z = (cooc - u) / o
  
  # z = z-score
  # cooc = co-occurrence
  # u = expected co-occurrence
  # o = standard deviation of expected co-occurrence

  # Test conditions
  stopifnot(is.matrix(mat))
  
  # Calculate co-occurrence
  cooc <- co.occurrence(t(mat), diag = FALSE)
  
  # Calculate the expected co-occurrence based on the number of occurrences in tech i and j divided by the number of total patents
  n <- colSums(mat)
  P <- nrow(mat)
  u <- outer(n, n)
  u <- u / P
  
  # Calculate the standard deviation of the expected co-occurrence
  x1 <- 1 - (n / P)
  x2 <- (P - n) / (P - 1)
  o <- outer(x1, x2)
  o <- u * o
  o <- sqrt(o)
  
  # Calculate and return final z-score
  z <- (cooc - u) / o
  z
  
}