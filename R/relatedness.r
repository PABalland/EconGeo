#' Compute the relatedness between entities (industries, technologies, ...) from their co-occurence matrix
#'
#' This function computes the relatedness between entities (industries, technologies, ...) from their co-occurence (adjacency) matrix. Different normalization procedures are proposed following van Eck and Waltman (2009): association strength, cosine, Jaccard, and an adapted version of the association strength that we refer to as probability index.
#' @param mat An adjacency matrix of co-occurences between entities (industries, technologies, cities...)
#' @param method Which normalization method should be used to compute relatedness? Defaults to "prob", but it can be "association", "cosine" or "Jaccard"
#' @keywords relatedness
#' @export
#' @examples
#' ## generate an industry - industry matrix in which cells give the number of co-occurences
#' ## between two industries
#' set.seed(31)
#' mat <- matrix(sample(0:10,36,replace=T), ncol = 6)
#' mat[lower.tri(mat, diag = TRUE)] <- t(mat)[lower.tri(t(mat), diag = TRUE)]
#' rownames(mat) <- c ("I1", "I2", "I3", "I4", "I5", "I6")
#' colnames(mat) <- c ("I1", "I2", "I3", "I4", "I5", "I6")
#'
#' ## run the function
#' relatedness (mat)
#' relatedness (mat, method = "association")
#' relatedness (mat, method = "cosine")
#' relatedness (mat, method = "Jaccard")
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl} \cr
#' Joan Crespo \email{J.Crespo@uu.nl} \cr
#' Mathieu Steijn \email{M.P.A.Steijn@uu.nl}
#' @seealso \code{\link{relatedness.density}}, \code{\link{co.occurence}}
#' @references van Eck, N.J. and Waltman, L. (2009) How to normalize cooccurrence data? An analysis of some well-known similarity measures, \emph{Journal of the American Society for Information Science and Technology} \strong{60} (8): 1635-1651 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114 \cr
#' \cr
#' Hidalgo, C.A., Klinger, B., Barabasi, A. and Hausmann, R. (2007) The product space conditions the development of nations, \emph{Science} \strong{317}: 482-487 \cr
#' \cr
#' Balland, P.A. (2016) Relatedness and the Geography of Innovation, in: R. Shearmur, C. Carrincazeaux and D. Doloreux (eds) Handbook on the Geographies of Innovation. Northampton, MA: Edward Elgar \cr
#' \cr
#' Steijn, M.P.A. (2017) Improvement on the association strength: implementing probability measures based on combinations without repetition, \emph{Working Paper, Utrecht University}

"relatedness" <- function(mat, method = "prob") {
  
  method <- tolower(method)

  Cij = mat
  diag(Cij) <- 0
  Si <- colSums(Cij)
  Sj <- colSums(Cij)
  Si <- matrix(Si,nrow=length(Si),ncol=length(Si),byrow=TRUE)
  Sj <- matrix(Sj,nrow=length(Sj),ncol=length(Sj),byrow=FALSE)
  T<-(sum(Cij))
  
  if (method == "prob") {
    
    SM <- Cij/(((Si/T)*(Sj/(T-Si))+(Sj/T)*(Si/(T-Sj)))*(T/2))
    SM[is.na(SM)] <- 0
    diag(SM) <- 0
    return(SM)
    
  } else if (method == "association") {
    
    SA <- (Cij/T)/ ((Si/T) * (Sj/T))
    SA[is.na(SA)] <- 0
    diag(SA) <- 0
    return(SA)
    
  } else if (method == "cosine") {

    SC <- (Cij)/ sqrt (Si * Sj)
    SC[is.na(SC)] <- 0
    diag(SC) <- 0
    return(SC)
    
  } else if (method == "jaccard") {
    
    SJ <- (Cij)/ (Si + Sj - Cij)
    SJ[is.na(SJ)] <- 0
    diag(SJ) <- 0
    return(SJ)

  } else {
    
    stop("Unknown value for argument 'method'")

  }


}

