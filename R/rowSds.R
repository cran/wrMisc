#' sd for each row (fast execution)
#'
#' \code{rowSds} is speed optimized sd (takes matrix or data.frame and treats each line as set of data for sd {equiv to apply(dat,1,sd)}.
#' NAs are ignored from data unless entire line NA). Speed improvements may be seen at more than 100 lines.
#' Note: NaN instances will be transformed to NA
#' @param dat matrix (or data.frame) with numeric values (may contain NAs)
#' @return numeric vector of sd values
#' @seealso \code{\link[stats]{sd}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' rowSds(dat1)
#' @export
rowSds <- function(dat) {
  msg <- "'dat' should be matrix or data.frame with "
  if(is.null(ncol(dat))) stop(msg,"multiple columns !") else if(ncol(dat) < 2) stop(msg,"at least 2 columns !")
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  allRowsNA <- rowSums(!is.na(dat)) <1
  out <- sqrt(rowSums(matrix(as.numeric(!is.na(dat)),ncol=ncol(dat))*((dat) - rowMeans(dat,na.rm=TRUE))^2,na.rm=TRUE)/(rowSums(!is.na(dat))-1)) 
  chNan <- is.nan(out)
  if(any(chNan)) out[which(chNan)] <- NA      
  if(any(allRowsNA)) out[which(allRowsNA)] <- NA              
  out }
   
