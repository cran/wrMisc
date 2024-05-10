#' Protect Special Characters
#'
#' Some characters do have a special meaning when used with regular expressions.
#' This concerns characters like a point, parinthesis, backslash etc.
#' Thus, when using \code{grep} or any related command, shuch special characters must get protected in order to get considered as they are.  
#' 
#' 
#' 
#' 
#' 
#' @param x character vector to be prepared for use in regular expressions
#' @param prot (character) collection of characters that need to be protected 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a modified character vector
#' @examples
#' aa <- c("abc","abcde","ab.c","ab.c.e","ab*c","ab\\d")
#' grepl("b.", aa)             # all TRUE
#' grepl("b\\.", aa)           # manual prootection
#' grepl(protectSpecChar("b."), aa)
#' @export
protectSpecChar <- function(x, prot=c(".","\\","|","(",")","[","{","^","$","*","+","?"), silent=TRUE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="protectSpecChar")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(length(x) >0 && length(prot) >0) {
    chSp <- sapply(paste0("\\",prot), function(y) grepl(y, x))
    if(length(dim(chSp)) <2) chSp <- matrix(chSp, ncol=length(prot))
    
    ## special treatment for '\\'
    if("\\" %in% prot & any(grepl("\\\\",x))) {x <- gsub("\\\\","\\\\\\\\",x); prot <- prot[-which(prot %in% "\\")]}
    ## main protecting of sec chars
    if(any(chSp)) {
      z <- if(length(x) >1) which(colSums(chSp) >0) else which(chSp)
      if(debug) {message(fxNa," pSC1"); pSC1 <- list(x=x,prot=prot,chSp=chSp,z=z)}
      for(i in z) {
        if(debug) message(fxNa," i=",i," at ",pasteC(which(chSp[,i])),"  to ",pasteC(x[which(chSp[,i])]) )   
        x[which(chSp[,i])] <- gsub(paste0("\\",prot[i]), paste0("\\\\",prot[i]), x[which(chSp[,i])])
      }
    }
  } else if(debug) message(fxNa," 'x' and 'prot' may not be empty, nothing to do ..")    
  x
}
  
