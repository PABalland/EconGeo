#' Generate a data frame of entry events from multiple regions - industries matrices (same matrix composition for the different periods)
#'
#' This function generates a data frame of entry events from multiple regions - industries matrices (same matrix composition for the different periods). In this function, the maximum number of periods is limited to 20.
#' @param mat1 An incidence matrix with regions in rows and industries in columns (period 1 - mandatory)
#' @param mat2 An incidence matrix with regions in rows and industries in columns (period 2 - mandatory)
#' @param mat3 An incidence matrix with regions in rows and industries in columns (period 3 - optional)
#' @param mat4 An incidence matrix with regions in rows and industries in columns (period 4 - optional)
#' @param mat5 An incidence matrix with regions in rows and industries in columns (period 5 - optional)
#' @param mat6 An incidence matrix with regions in rows and industries in columns (period 6 - optional)
#' @param mat7 An incidence matrix with regions in rows and industries in columns (period 7 - optional)
#' @param mat8 An incidence matrix with regions in rows and industries in columns (period 8 - optional)
#' @param mat9 An incidence matrix with regions in rows and industries in columns (period 9 - optional)
#' @param mat10 An incidence matrix with regions in rows and industries in columns (period 10 - optional)
#' @param mat11 An incidence matrix with regions in rows and industries in columns (period 11 - mandatory)
#' @param mat12 An incidence matrix with regions in rows and industries in columns (period 12 - mandatory)
#' @param mat13 An incidence matrix with regions in rows and industries in columns (period 13 - optional)
#' @param mat14 An incidence matrix with regions in rows and industries in columns (period 14 - optional)
#' @param mat15 An incidence matrix with regions in rows and industries in columns (period 15 - optional)
#' @param mat16 An incidence matrix with regions in rows and industries in columns (period 16 - optional)
#' @param mat17 An incidence matrix with regions in rows and industries in columns (period 17 - optional)
#' @param mat18 An incidence matrix with regions in rows and industries in columns (period 18 - optional)
#' @param mat19 An incidence matrix with regions in rows and industries in columns (period 19 - optional)
#' @param mat20 An incidence matrix with regions in rows and industries in columns (period 20 - optional)
#' @keywords relatedness density co-occurences relatedness normalized co-occurences
#' @export
#' @examples
#' ## generate a first region - industry matrix in which cells represent the presence/absence of a RCA (period 1)
#' set.seed(31)
#' mat1 <- matrix(sample(0:1,20,replace=T), ncol = 4) 
#' rownames(mat1) <- c ("R1", "R2", "R3", "R4", "R5")
#' colnames(mat1) <- c ("I1", "I2", "I3", "I4")
#' 
#' ## generate a second region - industry matrix in which cells represent the presence/absence of a RCA (period 2)
#' mat2 <- mat1
#' mat2[3,1] <- 1
#' 
#' ## run the function
#' entry.list (mat1, mat2)
#'
#' ## generate a third region - industry matrix in which cells represent the presence/absence of a RCA (period 3)
#' mat3 <- mat2
#' mat3[5,2] <- 1
#' 
#' ## run the function
#' entry.list (mat1, mat2, mat3)
#' 
#' ## generate a fourth region - industry matrix in which cells represent the presence/absence of a RCA (period 4)
#' mat4 <- mat3
#' mat4[5,4] <- 1
#' 
#' ## run the function
#' entry.list (mat1, mat2, mat3, mat4)
#' @author Pierre-Alexandre Balland \email{p.balland@uu.nl}
#' @seealso \code{\link{entry}}, \code{\link{exit}}, \code{\link{exit.list}}  
#' @references Boschma, R., Balland, P.A. and Kogler, D. (2015) Relatedness and Technological Change in Cities: The rise and fall of technological knowledge in U.S. metropolitan areas from 1981 to 2010, \emph{Industrial and Corporate Change} \strong{24} (1): 223-250 \cr
#' \cr
#' Boschma, R., Heimeriks, G. and Balland, P.A. (2014) Scientific Knowledge Dynamics and Relatedness in Bio-Tech Cities, \emph{Research Policy} \strong{43} (1): 107-114

"entry.list" <- function(mat1, mat2, mat3, mat4, mat5, mat6, mat7, mat8, mat9, mat10, mat11, mat12, mat13, mat14, mat15, mat16, mat17, mat18, mat19, mat20) {

n = 2

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry = bim2 - bim1
entry = get.list (entry)
colnames (entry) <- c("region", "industry", "entry")
entry$period <- n

  if (missing(mat3)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat4)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat5)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat6)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat7)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat8)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat9)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat10)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat11)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat12)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat13)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat14)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat15)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat16)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat17)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat18)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat19)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

}

  if (missing(mat20)) {

return (entry)

       } else {

n = n + 1

nam <- paste("bim", 1, sep = "")
    object = get(paste0("mat", n-1))
    assign(nam, object)

nam2 <- paste("bim", 2, sep = "")
    object = get(paste0("mat", n))
    assign(nam2, object)

bim1[bim1 == 1] <- NA
entry2 = bim2 - bim1
entry2 = get.list (entry2)
colnames (entry2) <- c("region", "industry", "entry")
entry2$period <- n
entry <- rbind (entry, entry2)

return (entry)
 
}
 
 
}
