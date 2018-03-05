#' Matrix & Vector Computation
#' 
#' Will compute (x)(matrix)(x^T)
#' 
#' @param mat A 2-D Matrix
#' @param vec A numerical vector
#' @return Scalar
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' matMult(a, x)
#' 
matMult <- function(mat, vec){
stopifnot(is.numeric(mat))
stopifnot(is.numeric(vec))
stopifnot(ncol(mat) == nrow(mat))
stopifnot(ncol(mat) == length(vec))
stopifnot(is.finite(vec))
stopifnot(is.finite(mat))

inv <- solve(mat)
return(t(vec) %*% inv %*% vec)

}


#' Standardize A 2-D Matrix
#' 
#' Will compute a standardized matrix by column.
#' 
#' @param dat A 2-D Matrix containing numerical values
#' @return A 2-D Matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' stdize(a)
#' 
stdize <- function(dat){
  stopifnot(nrow(dat) > 1)
  stopifnot(is.numeric(dat))
  stopifnot(is.finite(dat))
  
  count = 1
  rows <- nrow(dat)
  cols <- ncol(dat)
  while (count <= cols){
    singleColumn <- dat[,count]
    columnMean <- mean(singleColumn)
    columnSd <- sd(singleColumn)
    dat[,count] = ((singleColumn-columnMean)/columnSd)
    count = count + 1
  }
  return(dat)
}
