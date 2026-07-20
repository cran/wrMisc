#' Truncating Text Vector To Specific Length
#'
#' This function allows truncating a text vector (or matrix of text) at nMax number of characters with optional adding of truncation/ending sign and trimming after last separator.
#' 
#' convenient access to results produced using the functions \code{\link[base]{substr}} or \code{\link{moderTest2grp}}.
#' The user can define the threshold and which type of multiple testing correction should be used
#'  (as long as the multiple testing correction method cited was actually performed as part of testing).
#'  
#' @details
#' This function builds on \code{\link[base]{substr}} but offers additional options for trimming at right side of text:
#' In contrast to \code{\link[base]{substr}} where the absolute position number has to be given, this function takes 
#' the max desired length of the resulting character vector (argument \code{nMax}).
#' The argument \code{appendChar} allows adding extra characters (default '...') indicating that trimmming has been performed.
#' When doing so, the text will be trimmed further inside to ensure the final length of the resulting text, as defined by argument \code{nMax}.
#' 
#' Furthermore, using the argument \code{sep} it is possible to respect entities between separators and only cut at/before such separators
#' (the resulting text vecor may thus be shorter than specified globally with argument \code{nMax}).
#' 
#' @param x (character or matrix of text) main input, text to be truncated
#' @param nMax (integer, length=1) max length in number of characters to be returned
#' @param sep (character or \code{NULL}) optional separator to be considered : Truncation will happen before the separator; set to  \code{NULL} for ignoring
#' @param appendChar (character, length=1) optional set of characters to add at end when trucation has happened (default '...'); set to  \code{NULL} or  \code{""} for not appending anything
#' @param startFrom (integer, length=1) designes the first character to be extracted (see also \code{\link[base]{substr}} ) 
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a data.frame or matrix (if no annotation added) with values (and annotation) conform to fiter criteria
#' @seealso \code{\link[base]{substr}} 
#' @examples
#' txt <- c("abc", "abcdefgh", "abc/def/gh")
#' truncateVect(txt, nMax=7, appendChar="..")
#' truncateVect(txt, nMax=7, sep="/", appendChar="..")
#' 
#' @export
truncateVect <- function(x, nMax=20, sep=NULL, appendChar="...", startFrom=1, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## truncate text vector (or matrix of text) at nMax number of characters, optional adding of truncation/ending sign and trimming after last separator
  fxNa <- .composeCallName(callFrom, newNa="truncateVect")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) { silent <- FALSE } else { debug <- FALSE }
    
  datOK <- length(x) !=0 && !all(is.na(x))
  if(datOK && !is.character(x)) {
    iniDim <- dim(x)
    iniDimNa <- dimnames(x)
    x <- try(as.character(x))
    if(inherits(x, "try-error")) { datOK <- FALSE; x <- NULL
      if(!silent) message(fxNa,"")} 
  } else iniDim <- NULL
  if(inherits(x, "try-error")) datOK <- FALSE
  if(!(length(startFrom) ==1 && !is.na(startFrom) && is.numeric(startFrom) && startFrom >0)) {
    startFrom <- 1
    if(!silent) message(fxNa,"Invalid entry for argument 'startFrom', setting to default  startFrom=1")}
  if(datOK) {
    le <- nchar(x) - startFrom +1 
    chLe <- le > nMax
    if(any(chLe, na.rm=TRUE)) {
      sep <- if(length(sep)==1 && (is.na(sep) || sep=="")) NULL else sep[1]
      if(length(appendChar)==1 && is.na(appendChar)) {
        appendChar <- "" }
      x[which(le > nMax)] <- substr(x[which(le > nMax)], startFrom, max(1, startFrom + nMax - nchar(appendChar) ))   #+ if(length(sep)==1) min(nchar(sep), 3) else 0)
      if(length(sep)==1) { sep <- protectSpecChar(sep)
        x[which(le > nMax)] <- sub(paste0(sep,"[^(",sep,")]*$"),"",  x[which(le > nMax)]) }
      if(length(appendChar) ==1 && isTRUE(nchar(appendChar) !=0) ) x[which(le > nMax)] <- paste0(x[which(le > nMax)], appendChar)  
  } }
  if(length(iniDim)==0) x else matrix(x, nrow=iniDim[1], ncol=iniDim[2], dimnames=iniDimNa)    
}              
  
  
