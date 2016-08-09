#' Generate a data frame of industrial growth in regions from multiple regions - industries matrices (same matrix composition for the different periods)
#'
#' This function generates a data frame of industrial growth in regions from multiple regions - industries matrices (same matrix composition for the different periods). In this function, the maximum number of periods is limited to 20.
#' @param mat1 An incidence matrix with regions in rows and industries in columns (period 1 - mandatory)
#' @param mat2 An incidence matrix with regions in rows and industries in columns (period 2 - mandatory)
#' @param mat... An incidence matrix with regions in rows and industries in columns (period ... - optional)
#' @keywords growth
#' @export
#' @examples
#' ## generate a first region - industry matrix with full count (period 1)
#' set.seed(31)
#' mat1 <- matrix(sample(0:10,20,replace=T), ncol = 4)
#' rownames(mat1) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c ("I1", "I2", "I3", "I4")
#'
#' ## generate a second region - industry matrix with full count (period 2)
#' mat2 <- mat1
#' mat2[3,1] <- 8
#'
#' ## run the function
#' growth.list.ind (mat1, mat2)
#'
#' ## generate a third region - industry matrix with full count (period 3)
#' mat3 <- mat2
#' mat3[5,2] <- 1
#'
#' ## run the function
#' growth.list.ind (mat1, mat2, mat3)
#'
#' ## generate a fourth region - industry matrix with full count (period 4)
#' mat4 <- mat3
#' mat4[5,4] <- 1
#'
#' ## run the function
#' growth.list.ind (mat1, mat2, mat3, mat4)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{growth}}, \code{\link{exit}}, \code{\link{exit.list}}
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

"growth.list.ind" <- function(mat1, mat2, mat3, mat4, mat5, mat6, mat7, mat8, mat9, mat10, mat11, mat12, mat13, mat14, mat15, mat16, mat17, mat18, mat19, mat20) {

n = 2

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth[is.nan(growth)] <- 0
growth = round (growth, digits = 2)

growth = data.frame (industry = colnames (mat1), gr.ind = growth)
growth$period <- n

  if (missing(mat3)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat4)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat5)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat6)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat7)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat8)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat9)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat10)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat11)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat12)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat13)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat14)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat15)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat16)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat17)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat18)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat19)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

}

  if (missing(mat20)) {

return (growth)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

growth2 = ((colSums(bim2) - colSums(bim1))/colSums(bim1))*100
growth2[is.nan(growth2)] <- 0
growth2 = round (growth2, digits = 2)

growth2 = data.frame (industry = colnames (mat1), gr.ind = growth2)
growth2$period <- n

growth <- rbind (growth, growth2)

return (growth)

}


}
