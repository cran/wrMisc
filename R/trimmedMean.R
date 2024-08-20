#' Trimmed Mean
#'
#' This function allows more flexible options for calculating a trimmed mean compared to \code{mean} (from the base-package).
#' 
#' 
#' @details
#' If the second value of trim is <0.5 it is supposed that this indicates a fraction from the upper end the vector-dat as \code{\link[base]{mean}} does.
#' Otherwise, \code{trim=c(l=0.2,u=0.7)} will be interpreted indication to use the 20th percentile to 70th percentile of \code{dat}.
#' 
#' Please note, that trimmed means - and in particular asymmetric trimmed means - should be used with caution as there is also a risk of introducing bias.
#' 
#' 
#' @param dat (numeric) numeric vector
#' @param trim (numeric, length=2) specifies how data should get trimmed, lower and upper fraction(s) to exclude have to be assigned separately.
#'   The lower and upper fraction may be named 'l' and 'u'.
#'   The value 0 means that all (sorted) data on a given side will be used. 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a (numeric) vector with the trimmed mean
#' @seealso  \code{\link[base]{mean}} (symmetric trimming only)
#' @examples
#' x <- c(17:11,27:28)
#' mean(x); mean(x, trim=0.15)
#' trimmedMean(x, trim=c(l=0, u=0.7))   # asymmetric trim
#' 
#' @export
trimmedMean <- function(dat, trim=c(l=0.2,u=0.2), silent=FALSE, debug=FALSE, callFrom=NULL) {
  ##
  fxNa <- .composeCallName(callFrom, newNa="trimmedMean")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  out <- NULL
  datOK <- is.numeric(dat) && length(dat) >0 
  if(length(trim) <1) trim <- c(0, 0)
  if(datOK) {
    if(length(trim) <2) {trim <- rep(trim, 2); 
      if(debug) message(fxNa,"Argument 'trim' is only of length=1, setting given value as symmetric") }
  } else { if(!silent) message(fxNa,"Invalid 'dat', returning NULL")}
  if(datOK) if(all(is.na(trim)) || all(is.na(dat))) {datOK <- FALSE; out <- NA}
  if(datOK) datOK <- all(trim >= 0 & trim <= 1)

  ## main
  if(datOK) if(sum(duplicated(trim[1:2]))==1) { out <- mean(dat, trim=trim[1], na.rm=TRUE)
    } else {
      if(trim[1]==trim[2]) out <- mean(dat, trim=trim[1], na.rm=TRUE) else {
        if(all(c("l","u") %in% names(trim))) trim <- trim[match(c("l","u"), names(trim))]
        if(trim[2] < 0.5) trim[2] <- 1 - trim[2]
        trim <- round(trim[1:2]*length(dat))
        if(debug) message(fxNa," using indexes ",trim[1]," to ",trim[2],"  out of 1:",length(dat))
        out <- if(trim[2] != trim[1]) mean(sort(dat)[trim[1]:trim[2]], na.rm=TRUE) else dat[trim[1]]}
    }   
  out }
    