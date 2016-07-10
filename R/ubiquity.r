#' Compute a simple measure of ubiquity of industries
#'
#' This function computes a simple measure of ubiquity of industries by counting the number of regions in which an industry can be found (location quotient > 1) from regions - industries (incidence) matrices 
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @param RCA Logical; should the index of relative comparative advantage (RCA - also refered to as location quotient) first be computed? Defaults to FALSE (a binary matrix - 0/1 - is expected as an input), but can be set to TRUE if the index of relative comparative advantage first needs to be computed 
#' @keywords diversity ubiquity
#' @export
#' @examples
#' ## generate a region - industry matrix with full count
#' set.seed(31)
#' mat <- matrix(sample(0:10,20,replace=T), ncol = 4) 
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#' 
#' ## run the function
#' ubiquity (mat, RCA = TRUE)
#' 
#' ## generate a region - industry matrix in which cells represent the presence/absence of a RCA
#' set.seed(31)
#' mat <- matrix(sample(0:1,20,replace=T), ncol = 4) 
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#' 
#' ## run the function
#' ubiquity (mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{diversity}} \code{\link{location.quotient}} 
#' @references Balland, P.A. and Rigby, D. (2016) The geography of complex knowledge, \emph{Economic Geography, forthcoming}


"ubiquity"<- function(mat, RCA = FALSE) {
    
  if (RCA) {
    share_tech_city <- mat / rowSums (mat)
    share_tech_total <- colSums (mat) / sum (mat)
    LQ <- t(t(share_tech_city)/ share_tech_total)
    LQ[is.na(LQ)] <- 0 
    LQ[LQ < 1] <- 0
    LQ[LQ > 1] <- 1
    mat <- LQ

  # compute the share of a tech in a city's portfolio
  # markov chain - row stochastic 
  C = colSums(mat)
  
  
  } else {
  # compute the share of a tech in a city's portfolio
  # markov chain - row stochastic 
  C = colSums(mat)
  
  }
  return (C)
  
}



