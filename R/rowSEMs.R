#' SEM for each row
#'
#' \code{rowSEMs} speed optimized SEM (standard error of the mean) for each row.
#' The function takes a matrix or data.frame and treats each row as set of data for SEM; NAs are ignored from data.
#' Note: NaN instances will be transformed to NA 
#' @param dat matrix or data.frame
#' @return numeric vector with SEM values
#' @seealso \code{\link{rowSds}},  \code{\link{colSds}}, \code{\link[base]{colSums}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' head(rowSEMs(dat1))
#' @export
rowSEMs <- function(dat) {
  if(is.null(ncol(dat))) stop("data should be matrix or data.frame with multiple columns !") else if(ncol(dat) < 2) {
    stop("data should be matrix or data.frame with at least 2 columns !")}
  out <- sqrt(rowSums(matrix(as.numeric(!is.na(dat)),ncol=ncol(dat))*((dat) - rowMeans(dat,na.rm=TRUE))^2,na.rm=TRUE)/(rowSums(!is.na(dat))-1)) /
    sqrt(rowSums(!is.na(dat)))
  out[is.nan(out)] <- NA      
  out }
  
