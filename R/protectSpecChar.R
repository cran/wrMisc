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
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a modified character vector
#' @examples
#' aa <- c("abc","abcde","ab.c","ab.c.e","ab*c","ab\\d")
#' grepl("b.", aa)             # all TRUE
#' grepl("b\\.", aa)           # manual prootection
#' grepl(protectSpecChar("b."), aa)
#' @export
protectSpecChar <- function(x, prot=c(".","\\","|","(",")","[","{","^","$","*","+","?"), silent=TRUE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="protectSpecChar")
  if(!isTRUE(silent)) silent <- FALSE
  chSp <- matrix(sapply(prot, function(x) grepl(paste0("\\",x),x)), ncol=length(prot))
  ## special treatment for '\\'
  if("\\" %in% prot & any(grepl("\\\\",x))) {x <- gsub("\\\\","\\\\\\\\",x); prot <- prot[-which(prot %in% "\\")]}
  ## main protecting of sec chars
  if(any(chSp)) {
    z <- if(length(x) >1) which(colSums(chSp) >0) else which(chSp)
    for(i in z) {
      if(!silent) message(fxNa," i=",i," at ",pasteC(which(chSp[,i])),"  to ",pasteC(x[which(chSp[,i])]) )   
      x[which(chSp[,i])] <- gsub(paste0("\\",prot[i]), paste0("\\\\",prot[i]), x[which(chSp[,i])])
    }
  }
  x
}
  
