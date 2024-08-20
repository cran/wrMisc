#' sd for each column
#'
#' This function is speed optimized \code{sd} per column of a matrix or data.frame and treats each column as independent set of data for sd (equiv to \code{apply(dat,2,sd)}).
#' NAs are ignored from data.  Speed improvements may be seen at more than 100 columns
#' @param dat matrix (or data.frame) with numeric values (may contain NAs which will be ignored)
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return numeric vector of sd values
#' @seealso \code{\link[stats]{sd}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' colSds(dat1)
#' @export
colSds <- function(dat, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="colSds")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(length(dat) <1) out <- NULL else {  
    msg <- "'dat' should be matrix or data.frame with "
    if(is.null(ncol(dat))) stop(msg,"multiple rows !") else if(nrow(dat) < 2) stop(msg,"at least 2 rows !")
    if(is.data.frame(dat)) dat <- as.matrix(dat)
    out <- sqrt(colSums(matrix(as.numeric(!is.na(t(dat))), ncol=nrow(dat))*(t(dat) - colMeans(dat, na.rm=TRUE))^2, na.rm=TRUE)/(colSums(!is.na(dat)) -1))  # handels NA OK
    out[is.nan(out)] <- NA }        # replace 'NaN' by NA for output
  out }
   
