#' rowCVs
#'
#' This function returns CV for values in each column (using speed optimized standard deviation).
#' Note : NaN values get replaced by NA.
#' @param dat (numeric) matix 
#' @param autoconvert (NULL or character) allows converting simple vectors in matrix of 1 row (autoconvert="row")
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a (numeric) vector with CVs for each column of 'dat'
#' @seealso  \code{\link[base]{rowSums}}, \code{\link{rowCVs}}, \code{\link{rowGrpCV}}, \code{\link{colSds}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)),ncol=10)
#' head(colCVs(dat1))
#' @export
colCVs <- function(dat, autoconvert=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="colCVs")
  msg <- "Data should be matrix or data.frame with at least 2 rows !"
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  if(is.null(ncol(dat))) {
    if(identical(autoconvert,"col")) dat <- matrix(dat, ncol=1)}
  if(is.null(nrow(dat))) stop(msg) else if(nrow(dat) < 2) stop(msg)
  out <- colSds(dat)/base::colMeans(dat, na.rm=TRUE)
  out[which(is.nan(out))] <- NA  
  out }
    
