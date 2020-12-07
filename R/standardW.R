#' Standardize (scale) data
#'
#' This functions work similar to \code{\link[base]{scale}}, however, it evaluates the entire input and not column-wise (and independeltly as \code{scale} does). 
#' With Standarizing we speak of transforming the data to end up with mean=O and sd=1.
#' Furthermore, in case of 3-dim arrays, this function returns also an object with the same dimensions as the input.
#' 
#' @param mat (matrix, data.frame or array) data that need to get standardized.
#' @param byColumn (logical) if \code{TRUE} the function will be run independently over all columns such as as \code{apply(mat,2,standardW)}  
#' @param na.rm (logical) if \code{NA}s in the data don't get ignored via this argument, the output will be all \code{NA} 
#' @return vector of rescaled data (in dimensions as input)
#' @seealso \code{\link[base]{scale}} 
#' @examples
#' dat <- matrix(2*round(runif(100),2), ncol=4)
#' mean(dat); sd(dat)
#' 
#' dat2 <- standardW(dat)
#' apply(dat2, 2, sd)
#' summary(dat2)
#' 
#' dat3 <- standardW(dat, byColumn=TRUE)
#' apply(dat2, 2, sd)
#' summary(dat2)
#' mean(dat2); sd(dat2)
#' 
#' @export
standardW <- function(mat, byColumn=FALSE, na.rm=TRUE) {   
  ## standardize as entire matrix (ie not column-wise, relative differences in row get thus conserved), specific cols may be selected
  ## used to be standEntMatr
  if(is.data.frame(mat)) mat <- as.matrix(mat)
  if(byColumn & length(dim(mat) >1)) { std <- function(x) (mat -mean(x, na.rm=na.rm))/stats::sd(x, na.rm=na.rm) 
    if(length(dim(mat)) ==2) apply(mat, 2, std) else {if(length(dim(mat)) ==3) apply(mat, 2:3, std) }
  } else (mat -mean(mat, na.rm=na.rm)) /stats::sd(mat, na.rm=na.rm)
}
  
