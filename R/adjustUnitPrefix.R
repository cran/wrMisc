#' Adjust Value With Different Decimal Prefixes To Single Prefix Plus Unit
#'
#' This function provides help converting values with with different unit-prefixes to a single prefix-unit type.
#' This can be used to convert a vector of mixed prefixes like 'p' and 'n'.
#' Any text to the right of the unit will be ignored.
#'
#' @details
#' Please note that the current version recognizes and converts only interger values, decimals or scientific writing won't work.
#' The resultant numeric vector expresses all values as lowest prefix unit level.
#' In case of invalid entries \code{NA}s will be returned.
#'
#' Please note the 'u' is used for 'micro'.
#'
#' @param x (character) vector containing digit uunit-prefix and unit terms
#' @param pref (character) multiplicative unit-prefixes, assumes as increasing factors of 1000
#' @param unit (character) unit name, the numeric part may be sepatated by one space-character
#' @param sep (character) separator characters that may appear between integer numeric value and unit description
#' @param headingTxt (character) additional text preceeding the numeric part of 'x' to be ingnored/removed
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a numeric vector with quantities extracted and adjusted to a single type of unit (without the unit description)
#' @seealso \code{\link{convToNum}}
#' @examples
#' adjustUnitPrefix(c("10.psec abc","2 fsec etc"), unit="sec")
#' @export
adjustUnitPrefix <- function(x, pref=c("z","a","f","p","n","u","m"), unit="sec", sep=c("."," ",""), headingTxt="", silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## fix amol/fmol
  ## 'x' (char)  name including units (to be mined)
  ## conVal' (numeric) custom provided numeric value, if NULL all digit content at beginning (following after 'headingTxt') will be extracted (note, this won't extract digits after comma or scientific notation)
  ## 'pref' (char) must be incrementing by factor 1000
  ## 'headingTxt' (char) constant part of text before numeric part of concentration indications
  fxNa <- .composeCallName(callFrom, newNa="adjustUnitPrefix")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  if(length(unit) >1) { if(!silent) message(fxNa,"Argument 'unit' must be of length=1, truncating...")
    unit <- unit[1]}
  pref2 <- paste0(pref, collapse="|")
  sep2 <- if("" %in% sep) sep[-which(sep %in% "")] else sep
  sep3 <- if(length(sep2) <1) protectSpecChar(sep) else paste0(protectSpecChar(sep2),"{0,1}")
  if(length(sep3) >1) sep3 <- paste0("(",paste0(sep3, collapse="|"),")")
  if(length(headingTxt) ==1) { if(is.na(headingTxt)) headingTxt <- ""
    if(nchar(headingTxt) >0) x <- sub(paste0("^",headingTxt,sep3), "", x)}             # remove headingTxt (+sep)
  if(debug) {message(fxNa,"aCP1")}
  conVal <- sub(paste0("^","[[:digit:]]+",sep3), "", x)
  conVal <- try(as.numeric(substr(x, 1, nchar(x) - nchar(conVal))), silent=TRUE)
  if(debug) {message(fxNa,"aCP2"); aCP2 <- list(x=x,pref=pref,unit=unit,headingTxt=headingTxt,sep=sep,sep2=sep2,sep3=sep3,conVal=conVal)}
  if(inherits(conVal, "try-error") || length(x) <1) { if(!silent) message(fxNa,"Invalid entry for x, returning NA")
    rep(NA, length(x))
  } else {
    xUnit <- sub(paste0("^[[:digit:]]+",sep3), "", x)         # unit + ev other
    xExt <- nchar(sub(paste0("^.+",unit),"", xUnit))          # number of char after unit
    if(any(xExt >0)) xUnit[which(xExt >0)] <- substr(xUnit[which(xExt >0)], 1, (nchar(xUnit)- xExt)[which(xExt >0)])      # remove extension
    if(sum(duplicated(xUnit)) >0) {
      molCh <- sapply(pref, function(x) nchar(xUnit) > nchar(sub(paste0("^",x),"", xUnit)))
      molCh <- molCh[,min(which(colSums(molCh) >0)):max(which(colSums(molCh) >0))]     # reduce to actual range of prefixes found
      minPref <- colnames(molCh)[1]

      conVal <- conVal*1000^(0:(ncol(molCh) -1))[apply(molCh,1, which)]
      names(conVal) <- paste0(conVal, minPref, unit)
    } else names(conVal) <- paste0(conVal,xUnit)
    conVal }
 }

