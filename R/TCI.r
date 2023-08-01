#' Compute an index of knowledge complexity of industries using the eigenvector method
#'
#' This function computes an index of knowledge complexity of industries using the eigenvector method from regions - industries (incidence) matrices. Technically, the function returns the eigenvector associated with the second largest eigenvalue of the projected industry - industry matrix.
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param rca Logical; should the index of relative comparative advantage (RCA - also refered to as location quotient) first be computed? Defaults to FALSE (a binary matrix - 0/1 - is expected as an input), but can be set to TRUE if the index of relative comparative advantage first needs to be computed
#' @keywords complexity
#' @export
#' @examples
#' ## generate a region - industry matrix with full count
#' set.seed(31)
#' mat <- matrix(sample(0:10, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' tci(mat, rca = TRUE)
#'
#' ## generate a region - industry matrix in which cells represent the presence/absence of a rca
#' set.seed(31)
#' mat <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
#' rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c("I1", "I2", "I3", "I4")
#'
#' ## run the function
#' tci(mat)
#'
#' ## generate the simple network of Hidalgo and Hausmann (2009) presented p.11 (Fig. S4)
#' countries <- c("C1", "C1", "C1", "C1", "C2", "C3", "C3", "C4")
#' products <- c("P1", "P2", "P3", "P4", "P2", "P3", "P4", "P4")
#' my_data <- data.frame(countries, products)
#' my_data$freq <- 1
#' mat <- get_matrix(my_data)
#'
#' ## run the function
#' tci(mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{location_quotient}}, \code{\link{ubiquity}}, \code{\link{diversity}}, \code{\link{morc}}, \code{\link{kci}}, \code{\link{mort}}
#' @references Hidalgo, C. and Hausmann, R. (2009) The building blocks of economic complexity, \emph{Proceedings of the National Academy of Sciences} \strong{106}: 10570 - 10575. \cr
#' \cr
#' Balland, P.A. and Rigby, D. (2017) The Geography of Complex Knowledge, \emph{Economic Geography} \strong{93} (1): 1-23.


tci <- function(mat, rca = FALSE) {
  # remove null observations
  mat <- mat[rowSums(mat) > 0, ]
  mat <- mat[, colSums(mat) > 0]
  mat

  if (rca) {
    share_tech_city <- mat / rowSums(mat)
    share_tech_total <- colSums(mat) / sum(mat)
    lq <- t(t(share_tech_city) / share_tech_total)
    lq[is.na(lq)] <- 0
    lq[lq < 1] <- 0
    lq[lq > 1] <- 1
    mat <- lq

    # compute the share of a tech in a city's portfolio
    # markov chain - row stochastic
    c <- mat / rowSums(mat)
    c

    # sum of the rows = 1
    rowSums(c)

    # compute the share of a city in the overall produmation of a tech
    # markov chain - row stochastic
    t <- t(mat) / colSums(mat)
    t

    # sum of the rows = 1
    rowSums(t)

    # multiplying t by c gives a tech-tech Markov chain (row stochastic)
    tt <- round(t %*% c, 4)
    tt

    # sum of the rows = 1
    rowSums(tt)

    #  calculate the eigenvalues and eigenvemators
    e <- eigen(tt)
    e

    # the dominant eigenvalue of a stochastic matrix is 1
    # the second eigenvalue is important here
    # it governs the rate at which the random process given by
    # the stochastic matrix converges to its stationary distribution

    v <- e$vec[, 2]
    v

    tci <- as.numeric(v) / sum(as.numeric(v))
    tci

    # eigenvectors do not have a sign
    # we make sure to choose the eigen that correlates with diversity
    if (cor(tci, ubiquity(mat),
      use = "pairwise.complete.obs",
      method = "spearman"
    ) > 0) {
      tci <- tci * (-1)
    }
  } else {
    # compute the share of a tech in a city's portfolio
    # markov chain - row stochastic
    c <- mat / rowSums(mat)
    c

    # sum of the rows = 1
    rowSums(c)

    # compute the share of a city in the overall produmation of a tech
    # markov chain - row stochastic
    t <- t(mat) / colSums(mat)
    t

    # sum of the rows = 1
    rowSums(t)

    # multiplying t by c gives a tech-tech Markov chain (row stochastic)
    tt <- round(t %*% c, 4)
    tt

    # sum of the rows = 1
    rowSums(tt)

    #  calculate the eigenvalues and eigenvemators
    e <- eigen(tt)
    e

    # the dominant eigenvalue of a stochastic matrix is 1
    # the second eigenvalue is important here
    # it governs the rate at which the random process given by
    # the stochastic matrix converges to its stationary distribution

    v <- e$vec[, 2]
    v

    tci <- as.numeric(v) / sum(as.numeric(v))
    tci

    # eigenvectors do not have a sign
    # we make sure to choose the eigen that correlates with diversity
    if (cor(tci, ubiquity(mat),
      use = "pairwise.complete.obs",
      method = "spearman"
    ) > 0) {
      tci <- tci * (-1)
    }
  }
  return(tci)
}
