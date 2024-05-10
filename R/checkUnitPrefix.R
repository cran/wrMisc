#' Check For Common Unit-Name in Text
#'
#' This function aims to find a unit abbreviation or name occurring in all elements of a character-vector \code{x}.
#' The unit name may be preceeded by different decimal prefixes (eg 'k','M'), as defined by argument \code{pref} and separators (\code{sep}). 
#' The unit name will be returned (or first of multiple).
#' 
#'
#' @details
#' Basically this function searches the pattern : digit + separator(\code{sep}) + prefix(\code{pref}) + \code{unit} + optional separator2(\code{sep2})
#' and returns the first unit-name/abbreviation found in all elements of \code{x}.
#' 
#' If \code{} if()
#' In case of invalid entries or no common unit-names \code{NULL} will be returned.
#'
#' Please note the 'u' is used for 'micro' since handeling of special characters may not be portal between different operating systems.
#'
#' @param x (character) vector containing digit uunit-prefix and unit terms
#' @param pref (character) multiplicative unit-prefixes, assumes as increasing factors of 1000
#' @param unit (character) unit name, the numeric part may be sepatated by one space-character
#' @param sep (character) separator character(s) that may appear between integer numeric value and unit-prefix
#' @param sep2 (character) separator character(s) after \code{unit}, set to \code{sep2=""} for ignoring characters following \code{unit}
#' @param stringentSearch (logical) if \code{TRUE} only matches with same separators (sep, sep2) pass, otherwise different elements may contain different separators
#' @param na.rm (logical) remove \code{NA} from input
#' @param protSpecChar (logical) protect special characters and use as they are instead of regex-meaning
#' @param inclPat (logical) return list including pattern of successful search
#' @param silent (logical) suppress messages 
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a charcter vector (length=1) with the common unit name, if \code{inclPat=TRUE} it returns a list with $unit and $pattern
#' @seealso \code{\link{convToNum}}; \code{\link{adjustUnitPrefix}}
#' @examples
#' x1 <- c("10fg WW","xx 10fg 3pW"," 1pg 2.0W")
#' checkUnitPrefix(x1)
#' ## different separators between digit and prefix:
#' x2 <- c("10fg WW","xx 8_fg 3pW"," 1 pg-2.0W")
#' checkUnitPrefix(x2, stringentSearch=TRUE)
#' checkUnitPrefix(x2, stringentSearch=FALSE)
#' 
#' x4 <- c("CT_mixture_QY_50_amol_CN_UPS1_CV_Standards_Research_Group",
#'   "CT_mixture_QY_5_fmol_CN_UPS1_CV_Standards_Research_Group")
#'
#' @export
checkUnitPrefix <- function(x, pref=c("a","f","p","n","u","m","","k","M","G","T","P"), unit=c("m","s","sec", "Mol","mol","g","K","cd","A","W","Watt","V","Volt"), sep=c(""," ",";",",","_","."),
  sep2="", stringentSearch=FALSE, na.rm=FALSE, protSpecChar=TRUE, inclPat=FALSE, callFrom=NULL, silent=FALSE, debug=FALSE) {
  ## check if digit + (flexible) prefix + (fixed) unit combination exists in all entries, 
  ## return first 'x' (ie unit) matching (NULL if not any found)
  fxNa <- .composeCallName(callFrom, newNa="checkUnitPrefix")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  out <- NULL
  fx1 <- function(y, pr, se, se2) apply(matrix(apply(cbind("[[:digit:]]", rep(se, length(pr)), rep(pr, each=length(se)*length(se2)),  y, rep(rep(se2, length(pr)), each=length(se))),
    1, paste0, collapse=""), ncol=length(pr)), 1, paste, collapse="|" )     # (stringent : fixed) dig + sep + pref + unit(y) + sep2  
  fx3 <- function(y, pr, se, se2) paste0(unique(apply(cbind( "[[:digit:]]", if("" %in% se) { paste0("(",paste(se[-which(se=="")], collapse="|"),"){0,1}")
    } else paste(se, collapse="|"), pr, y, if("" %in% se2) NULL else paste(se2, collapse="|")), 1, paste0, collapse="")), collapse="|")  # pattern less stringent, (ie variable) sep & sep2  

  ## check input & prepare
  if(!isFALSE(na.rm) && length(x) >0) x <- naOmit(unique(x))
  if(length(pref) >0) pref <- naOmit(unique(pref))
  if(length(sep) >0) sep <- naOmit(unique(sep))
  datOK <- all(c(length(x), length(pref), length(unit), length(sep)) >0)
  if(!datOK && !silent) message(fxNa,"Invalid entries: 'x','pref','unit' and/or 'sep' may not be all NA or NULL")
  if(datOK) {     
    if("." %in% c(pref, unit)) { datOK <- FALSE
      if(!silent) message(fxNa,"'pref' or 'unit' may NOT contain '.' !") } }
  if(debug) { message(fxNa,"cUP0")}
  if(datOK) {   
    if(protSpecChar) sep <- protectSpecChar(sep, silent=!debug, callFrom=fxNa) 
    if(length(sep2) <1) sep2 <- sep else {
      sep2 <- if("" %in% sep2) "" else {
        if(protSpecChar) protectSpecChar(unique(sep2), silent=!debug, callFrom=fxNa) else unique(sep2)} } 

    ## MAIN :check pattern per unit
    chPat <- lapply(unit, fx1, pr=pref, se=sep, se2=sep2)
    names(chPat) <- unit
    chPa2 <- lapply(chPat, sapply, grepl, x)
    if(debug) {message(fxNa,"cPU1"); cUP1 <- list()}
    fxCh <- function(z) if(length(dim(z)) >1) any(colSums(z)==nrow(z)) else {if(length(x) >1) all(z) else any(z)}
    chUnit <- unlist(lapply(chPa2, fxCh))
    if(debug) { message(fxNa,"colSu : ", paste(if(length(dim(chUnit)) >1) colSums(chUnit) else sum(chUnit), collapse=" "), "  cUP2")
      cUP2 <- list(x=x, pref=pref,unit=unit,sep=sep,sep2=sep2,stringentSearch=stringentSearch,na.rm=na.rm,protSpecChar=protSpecChar, chUnit=chUnit,fx1=fx1,fx3=fx3, out=out) }
    if(any(chUnit, na.rm=TRUE)) {
       ii <- which.max(nchar(names(chUnit))[which(chUnit)])    # in case of multiple hits choose (1st of) longest (supposedly entire unit-name)
       out <- if(isTRUE(inclPat)) list(unit=names(chUnit)[which(chUnit)][ii], 
       pattern=colnames(chPa2[[which(chUnit)]])[which(colSums(chPa2[[which(chUnit)]])==length(x))]) else names(chUnit)[which(chUnit)][ii]} 
    if(length(out) <1 && isFALSE(stringentSearch)) {
      ## now check less stringent, ie any combination of separators
      if(debug) message(fxNa,"Entering less stringent search ..  cUP3")
      chPat <- sapply(unit, fx3, pr=pref,se=sep,se2=sep2)
      chUnit <- colSums(sapply(chPat, grepl, x)) ==length(x)
      if(debug) message(fxNa,"Checking less stringent (variable sep & sep2) : ", any(chUnit))
      if(any(chUnit, na.rm=TRUE)) { out <- names(chPat)[which(chUnit)][1]
        if(isTRUE(inclPat)) out <- list(unit=out, pattern=chPat[which(chUnit)])} 
    }
  } else if(!silent) message(fxNa,"Invalid entries - nothing to do")
  out } 
     
