#' Adjust Value With Different Decimal Prefixes To Single Prefix Plus Unit
#'
#' This function provides help converting values with with different unit-prefixes to a single prefix-unit type.
#' This can be used to convert a vector of mixed prefixes like 'p' and 'n'.
#' Any text to the right of the unit will be ignored.
#'
#' @details
#' The aim of this function if to allow adjusting a vector containing '100pMol' and '1nMol' to '100pMol' and '1000pMol' for better downstream analysis.
#' Please note that the current version recognizes and converts only interger values; decimals or scientific writing won't be recognized properly.
#' The resultant numeric vector expresses all values as lowest prefix unit level.
#' In case of invalid entries \code{NA}s will be returned.
#'
#' Please note that decimal/comma digits will not be recognized properly, since the function will consider (by default) the decimal sign as just another separator.
#'
#' To avoid special characters (which may not work on all operating-systems) the letter 'u' is used for 'micro'.
#'
#' @param x (character) vector containing digit uunit-prefix and unit terms
#' @param pref (character) multiplicative unit-prefixes, assumes as increasing factors of 1000
#' @param unit (character) unit name, the numeric part may be sepatated by one space-character
#' @param sep (character) separator characters that may appear between integer numeric value and unit description
#' @param minTrimNChar (integer) min number of text characters when trimming adjacent text on left and right of main numeric+prefix+unit
#' @param returnType (character) set options for retuning results : 'NAifInvalid' .. return NA for invalid parts,'allText' .. return initial text if problem, 'trim'
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a character vector (same length as input) with adjusted unified decimal prefix and adjusted numeric content, the numeric content only is also giben in the names of the output
#' @seealso \code{\link{convToNum}}; \code{\link{checkUnitPrefix}}; \code{\link{trimRedundText}}
#' @examples
#' adjustUnitPrefix(c("2.psec abc","20 fsec etc"), unit="sec")
#' 
#' x1 <- c("50_amol", "5_fmol","250_amol","100_amol", NA, "500_amol", "500_amol", "1_fmol")
#' adjustUnitPrefix(x1, unit="mol")                   
#'
#' x2 <- c("abCc 500_nmol ABC", "abEe5_umol", "", "abFF_100_nmol_G", "abGg 2_mol", "abH.1 mmol")
#' rbind( adjustUnitPrefix(x2, unit="mol", returnType="allText") , 
#'   adjustUnitPrefix(x2, unit="mol", returnType="trim"),
#'   adjustUnitPrefix(x2, unit="mol", returnType=""))
#' @export
adjustUnitPrefix <- function(x, pref=c("z","a","f","p","n","u","m","","k","M","G"), unit="sec", sep=c("_","."," ",""), minTrimNChar=0, returnType=c("NAifInvalid","allText"), silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## fix amol/fmol labels
  fxNa <- .composeCallName(callFrom, newNa="adjustUnitPrefix")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  datOK <- all(c(length(x), length(pref), length(unit)) >0)
  if(length(returnType) >0 && is.character(returnType)) returnType <- naOmit(returnType)
  if(length(returnType) <1 || !is.character(returnType)) {returnType <- c("NAifInvalid","allText"); if(!silent) message(fxNa,"Invalid entry for 'returnType', setting ti default ('NAifInvalid','allText')")}
  out <- if("NAifInvalid" %in% returnType) rep(NA, length(x)) else x
  if(datOK) {  
    if(length(unit) >1) { if(!silent) message(fxNa,"Argument 'unit' must be of length=1, truncating...")
       unit <- unit[1]}
     chUnit <- grepl(unit, x)
     if(all(!chUnit)) { datOK <- FALSE
       if(!silent) message(fxNa,"Data do NOT contain '",unit,"' anywhere (nothing to do)") 
     } else if(any(!chUnit)) { x[which(!chUnit)] <- NA                        # set entries wo unit to NA
       if(!silent) message(fxNa,"Ignoring ",sum(!chUnit)," entries not containing '",unit,"' ")} 
  }    
  if(datOK) {  
    pref2 <- paste0(pref, collapse="|")
    sep2 <- if("" %in% sep) sep[-which(sep %in% "")] else sep   
    sep3 <- if(length(sep2) <1) protectSpecChar(sep, callFrom=fxNa) else paste0(protectSpecChar(sep2, callFrom=fxNa),"{0,1}")
    if(length(sep3) >1) sep3 <- paste0(sep3, collapse="|")  #,")")
    if(debug) {message(fxNa,"aUP1"); aUP1 <- list(x=x,pref=pref,unit=unit,sep=sep,sep2=sep2,sep3=sep3,pref2=pref2,minTrimNChar=minTrimNChar,returnType=returnType, sep2=sep2,sep3=sep3)}
    txRi <- sub( paste0(".*[[:digit:]]+(", sep3,")(",pref2,"){0,1}", unit), "", x)    # OK   (keep all AFTER digit+sep+pref +unit) 
    pref3 <- paste0("(",paste(unique(pref2), collapse="|"), ")")       # prefixes
     if(debug) {message(fxNa,"aUP1b"); aUP1b <- list()}      #headingTxt=headingTxt,chPref=chPref,txLe=txLe,
    lePat <- paste0("[[:digit:]]+(",sep3,")",pref3,"{0,1}(",sep3,")",unit,".*") 
    chLe <- grepl(lePat, x)                  # see if pattern (digit+sep +pref+unit) exists
    if(any(isFALSE(chLe))) {
      if(!silent) message(fxNa,"Ignoring ",sum(isFALSE(chLe))," instances for not-recognized pattern (eg no numeric content, or not-recognized unit or separator)")
      x[which(isFALSE(chLe))] <- NA }           # correcting/ignoring for not-recognized pattern
    chUnit <- !is.na(x)
    datOK <- sum(chUnit) >0
  }    
  if(datOK) {        
    txLe <- gsub(lePat,"", x)                   #   (keep all heading=toLeft to digits+sep+pref+unit+etc) 
    chPref <- nchar(x)==nchar(txLe)
     if(debug) {message(fxNa,"aUP1d"); aUP1d <- list(x=x,pref=pref,unit=unit,sep=sep,sep2=sep2,sep3=sep3,pref2=pref2,chPref=chPref,txLe=txLe,txRi=txRi)}      #headingTxt=headingTxt,
    xIni <- x 
    x <- mapply(sub, txRi, "", mapply(sub, txLe, "", x))                   # reduce to num +sep+unit
    num <- substr(x, 1, nchar(x)- nchar(sub("[[:digit:]]+","",x)))         # numeric content/part     
    sepPrefUnit <- mapply(sub, num, "", x)
    prefUnit <- sub(paste0("^",sep3),"", sepPrefUnit)
    allSep <- mapply(sub, prefUnit, "", sepPrefUnit)
    allPref <- sub(unit,"", prefUnit)
    dig <- try(as.numeric(num), silent=TRUE)
    if(debug) {message(fxNa,"-- aUP2"); aUP2 <- list()}     # conVal=conVal,
    if(inherits(dig, "try-error")) { if(!silent) message(fxNa,"Invalid entry for x (can't extract numeric part), returning NA")
    } else {
      freqSep <- names(sort(table(allSep), decreasing=TRUE))[1]     # most frequent separator
      ## now check which prefix used, if all prefix same 
      if(length(unique(naOmit(allPref))) >1) {
        if(debug) {message(fxNa, "aUP3, converting ",length(unique(naOmit(allPref)))," different prefixes")}
        molCh <- match(allPref, pref)
        if(any(is.na(molCh) & !is.na(dig))) warning("Non-recognized prefix encountered :",paste(unique(pref2[which(is.na(molCh))])), "Result might be WRONG !")
        minPref <- pref[min(molCh, na.rm=TRUE)]
        ## need to find most frequent separator : if txRi & txLe -> need to remove
        if(debug) {message(fxNa,"-- aUP3b"); aUP3b <- list(x=x,pref=pref,unit=unit,sep=sep,sep2=sep2,sep3=sep3,pref2=pref2,chPref=chPref,dig=dig,molCh=molCh,txLe=txLe,txRi=txRi,freqSep=freqSep)}
        
        ## convert (main)
        decFact <- 1e3                           # assume factor 1000             
        conVa3 <- dig* (decFact^(molCh - min(molCh, na.rm=TRUE)))    # adjust
        out <- if("allText" %in% returnType) {          ## add remaining text
          paste0(if(any(nchar(txLe) >0, na.rm=TRUE)) paste0(txLe, freqSep), conVa3, freqSep, minPref, unit, if(any(nchar(txRi) >0, na.rm=TRUE)) paste0(freqSep, txRi))
        } else { if("trim" %in% returnType)  paste0(trimRedundText(txLe, minNchar=minTrimNChar, callFrom=fxNa), freqSep, conVa3, freqSep, minPref, unit, 
          freqSep, trimRedundText(txRi, minNchar=minTrimNChar, callFrom=fxNa)) else paste0(conVa3, freqSep, minPref, unit) }
        if(any(!chUnit)) out[which(!chUnit)] <- NA
        names(out) <- conVa3                       # numeric value in name
        if(debug) {message(fxNa,"-- aUP4 .."); aUP4 <- list()}                  
      } else {
        out <- if("trim" %in% returnType) {
          paste0(trimRedundText(txLe, minNchar=minTrimNChar, callFrom=fxNa), dig, freqSep, naOmit(allPref)[1], unit, trimRedundText(txRi, minNchar=minTrimNChar, callFrom=fxNa))
        } else x
        names(out) <- dig    
      }
    } 
  }
  out }      
          
