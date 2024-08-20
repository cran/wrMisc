#' Trim/Remove Redundant Words
#'
#' This function allows removing shared words, ie triming to non-redundant words.
#' 
#'
#' @details
#' Heading separators will be removed in any case (even if not followed by a 'word').
#'
#' Special characters will be automatically protected.
#' When looking for repeated words, the order of such words does NOT matter, multiple repeats will be removed, too.
#'  
#' #' 
#' @param x (character) main input for making non-redundant
#' @param sep (character) separator(s) to be used
#' @param anySep (logical) if \code{TRUE}, will consider all separators at one time (), thus combinations with different separators won't be distinguished
#' @param newSep (character) new (uniform) separator between words, if \code{NULL} the first value/separator of if \code{sep} will be used
#' @param minLe (integer) minimum length for allowing being recognised as 'word'
#' @param na.omit (logical) if \code{TRUE NA}s will be removed from output 
#' @param fixed (logical) will be transmitted to argument \code{fixed} of \code{strsplit()};  if \code{TRUE} regular expressions are allowed/used
#' @param callFrom (character) allows easier tracking of messages produced
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @return This function returns character vector of same length (unless \code{na.omit=TRUE}), simply with modified text-content
#' @seealso \code{\link{trimRedundText}}
#' @examples
#' x1 <- c("aa_A1 yy_zz.txt", NA, "B2 yy_aa_aa_zz.txt")
#' rmSharedWords(x1)
#'  
#' @export
rmSharedWords <- function(x, sep=c("_"," ","."), anySep=TRUE, newSep=NULL, minLe=2, na.omit=FALSE, fixed=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## function to trim redundant words (@separator) similar to wrMisc::trimRedundText()
  ## remove common/repeated words as occuring in each instance of x; words are separarated by sep (ignoring NAs); separators must be consistent (no mixing of separators allowed)
  ## special characters will be automatically protected, order does NOT matter, multiple repeats will be removed, too
  ## note : anySep=TRUE will consider all separators at one time (), thus combinations with different separators won't be distinguished
  ## note : heading separators will be removed in any case
  ## move to wrMisc ??
  #example#   x1 <- c("aa_A1 yy_zz.txt", NA, "B2 yy_aa_aa_zz.txt"); rmSharedWords(x1)
  fxNa <- .composeCallName(callFrom, newNa="rmSharedWords")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  datOK <- length(x) >0 && length(sep) >0
  if(datOK) {
    chNA <- is.na(x)
    x2 <- if(any(chNA)) x[-which(chNA)] else x
    redSep <- which(colSums(sapply(protectSpecChar(sep), function(y) nchar(x2) > nchar(sub(y,"", x2))))==length(x2))
  } else x2 <- x
  if(datOK && length(redSep) >0) {
    se2 <- sep[redSep]                                  # reduce to separators appearing in all cases
    if(length(newSep) >1) newSep <- newSep[redSep]
    if(isTRUE(anySep)) {    # combine all separators
      iP <- paste(protectSpecChar(sep), collapse="|")
      se2 <- paste(sep, collapse="|")
      fixed <- FALSE
    } else  { se2 <- sep
      iP <- protectSpecChar(sep) }
    for(i in 1:length(se2)) {
      i2 <- iP[i]
      chSep <- if(i==1) TRUE else all(nchar(x2) > nchar(sub(i2,"", x2)))            # check current separator 'se2' if occuring in each instance
      if(chSep) {
        spl <- strsplit(x2, split=if(isTRUE(fixed)) se2[i] else iP[i], fixed=fixed)        
        rmW <- Reduce(intersect, spl)                              # get words common in all instances
        if(length(rmW) >0) { 
          nCh <- nchar(rmW)
          chL <- nCh >= minLe | nCh ==0
          if(any(chL)) {
            spl <- lapply(spl, function(y) y[-which(y %in% unique(c(rmW[which(chL)], if(TRUE) "")))])       # remove repeated/common 'words'
            newSe <- if(length(newSep) >0) {if(length(newSep) < length(sep)) rep(newSep,i)[i] else newSep[i]} else sep[1]
            x2 <- sapply(spl, paste, collapse=newSe) } }                      # paste collapse
      }
    } }
  if(!any(chNA) || isTRUE(na.omit)) x2 else {out <- x; out[-which(chNA)] <- x2; out}
}
   
