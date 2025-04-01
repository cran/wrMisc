#' Mean Of 3 Highest Values
#'
#' This function returns the mean of the top3 highest values.
#' 
#' @details
#' Generally, \code{NA} will be excluded, if all values are \code{NA} this finction will return \code{NULL} ;
#' thus, in case of (entirely) unsuitable data (non-numeric ...)  \code{NULL} will be returned.
#' If data has subgroups you may try a tapply -way. 
#' 
#' @param x (numeric vector) main input
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a vector with single numeric value for mean of top3
#'   ( returns \code{NULL} with invalid input or if all input is \code{NA})
#' @seealso \code{\link[base]{mean}}
#' @examples
#' x1 <- c(15:11,NA,16)
#' top3mean(x1)
#' @export
top3mean <- function(x, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ##  mean of the top3 highest values
  ## return vector
  fxNa <- .composeCallName(callFrom, newNa="top3mean")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  datOK <- length(x) >0 
  if(datOK) { if(!is.numeric(x)) x <- try(as.numeric(x), silent=TRUE)
    if(inherits(x, "try-error")) { datOK <- FALSE
      if(debug) message(fxNa,"Main input data are NOT numeric !  (nothing to do)")}
  }
  if(datOK) {
    x <- sort(x, decreasing=TRUE)
    out <- x[1:min(3, length(x))]
    chNA <- is.na(out)
    if(any(chNA) && !silent) message("fxNa","Note : Data contain ",sum(chNA)," NAs")
    out <- if(all(chNA)) NA else mean(out, na.rm=TRUE)
  } else out <- NULL
  out }  
  
 