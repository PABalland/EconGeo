#' Compute the Hachman index from regions - industries matrices 
#'
#' This function computes the Hachman index from regions - industries matrices. The Hachman index indicates how closely the industrial distribution of a region resembles the one of a more global economy (nation, world). The index varies between 0 (extreme dissimilarity between the region and the more global economy) and 1 (extreme similarity between the region and the more global economy) 
#' @param mat An incidence matrix with regions in rows and industries in columns
#' @keywords location quotient, relative comparative advantage Hachman
#' @export
#' @examples
#' ## generate a region - industry matrix 
#' set.seed(31)
#' mat <- matrix(sample(0:100,20,replace=T), ncol = 4) 
#' rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4")
#' 
#' ## run the function
#' Hachman (mat)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{average.location.quotient}}


Hachman <- function(mat) {
  share_tech_city <- mat / rowSums (mat)
  share_tech_total <- colSums (mat) / sum (mat)
  LQ <- t(t(share_tech_city)/ share_tech_total)
  LQ[is.na(LQ)] <- 0 
  meanLQ <- rowSums (LQ*share_tech_city)
  Hachman <- 1/meanLQ
  return (Hachman)
}


