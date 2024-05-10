#' sd for each column
#'
#' \code{colSds} is a speed optimized \code{sd} for matrix or data.frames. 
#' It and treats each line as an independent set of data for calculating the sd (equiv to \code{apply(dat,1,sd)}).
#' NAs are ignored from data. 
#' @param dat matrix (or data.frame) with numeric values (may contain NAs)
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return numeric vector of sd values
#' @seealso \code{\link[stats]{sd}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),nc=10)
#' colSds(dat1)
#' @export
colSds <- function(dat, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="colSds")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(length(dat) <1) out <- NULL else {  
    msg <- "'dat' should be matrix or data.frame with "
    if(is.null(ncol(dat))) stop(msg,"multiple columns !") else if(ncol(dat) < 2) stop(msg,"at least 2 columns !")
    if(is.data.frame(dat)) dat <- as.matrix(dat)
    out <- sqrt(rowSums(matrix(as.numeric(!is.na(t(dat))),ncol=nrow(dat))*(t(dat) - colMeans(dat,na.rm=TRUE))^2,na.rm=TRUE)/(colSums(!is.na(dat))-1))  # handels NA OK
    out[is.nan(out)] <- NA}        # replace 'NaN' by NA for output
  out }
   
