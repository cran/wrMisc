#' rowCVs
#'
#' This function returns CV for values in each row (using speed optimized standard deviation).
#' Note : NaN values get replaced by NA.
#' @param dat (numeric) matix 
#' @param autoconvert (NULL or character) allows converting simple vectors in matrix of 1 row (autoconvert="row")
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a (numeric) vector with CVs for each row of 'dat'
#' @seealso  \code{\link[base]{colSums}}, \code{\link{rowSds}}, \code{\link{rowGrpCV}}, \code{\link{colCVs}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' head(rowCVs(dat1))
#' @export
rowCVs <- function(dat, autoconvert=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="rowCVs")
  msg <- "Data should be matrix or data.frame with at least 2 columns !"
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  if(is.null(ncol(dat))) {
    if(identical(autoconvert,"row")) dat <- matrix(dat, nrow=1)}
  if(is.null(ncol(dat))) stop(msg) else if(ncol(dat) < 2) stop(msg)
  out <- rowSds(dat)/base::rowMeans(dat, na.rm=TRUE)
  out[which(is.nan(out))] <- NA  
  out }
    
