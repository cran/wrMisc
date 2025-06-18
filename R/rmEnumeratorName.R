#' Remove or rename enumerator tag/name (or remove entire enumerator) from tailing enumerators
#'
#' @description
#' This function allows indentifying, removing or renaming enumerator tag/name (or remove entire enumerator) from tailing enumerators (eg 'abc_No1' to 'abc_1').
#' A panel of potential candidates as combination of separator-symbols and separtor text/words will be tested to find if one matches all data.
#' In case the main input is a matrix, all columns will be tested independently to find the first column where one specific combination of separator-symbols and separtor text/words is found.
#' Several options exist for the output, the combination of separator-symbols and separtor text/words may be included, too.
#'
#' @details
#' 
#' In case only digit-enumerators are present (ie, without repetitive text), one has to use \code{incl="rmEnum"} to remove terminal enumerators. 
#' This will work, only when all items do contain terminal digits.
#' 
#' Please note, that checking a variety of different separator text-word and separator-symbols may give an important number of combinations to check.
#' In particular, when automatic trimming of separator text-words is added (eg \code{incl="trim2"}), the complexity of associated searches increases quickly.
#' Thus, with large data-sets restricting the content of the arguments \code{nameEnum}, \code{sepEnum} and (in particular) \code{newSep} to the most probable terms/options
#' is suggested to help reducing demands on memory and CPU.
#'
#' In case the input \code{dat} is a matrix and multiple different numerator-types are found, only the first colum (from the left) will be treated.
#' If you which to remove/subsitute mutiple types of enumerators the function \code{rmEnumeratorName} must be run independently, see last example below.
#'
#' @param dat (character vecor or matrix) main input
#' @param nameEnum (character) potential enumerator-names
#' @param sepEnum (character)  potential separators for enumerator-names
#' @param newSep (character) potential enumerator-names
#' @param incl (character) options to include further variants of the enumerator-names, 
#'   use \code{"rmEnum"} for completely removing enumerator tag/name and digits for different options of trimming names/tags from \code{nameEnum};
#'   or one may use \code{anyCase}, 
#'   \code{trim3} (trimming down to max 3 letters),
#'   \code{trim2} (trimming to max 2 letters) or  \code{trim1} (trimming down to single letter); 
#'   \code{trim0} works like \code{trim1} but also includes ' ', ie no enumerator tag/name in front of the digit(s)  
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a corrected vector (or matrix), or a list if \code{incl="rmEnumL"} containing $dat (corrected data),
#'   $pattern (the combination of separator-symbols and separtor text/words found), and if input is matrix $column (which column of the input was identified and treated)
#' @seealso when the exact pattern is known \code{\link[base]{grep}} and \code{sub} may allow direct manipulations much faster
#' @examples
#' xv <- c("hg_1","hjRe2_2","hk-33")
#' rmEnumeratorName(xv)
#' rmEnumeratorName(xv, incl="rmEnum")
#' 
#' xx <- c("hg_Re1","hjRe2_Re2","hk-Re3_Re33")
#' rmEnumeratorName(xx)
#' rmEnumeratorName(xx, newSep="--")
#' rmEnumeratorName(xx, incl="anyCase")
#'
#' xy <- cbind(a=11:13, b=c("11#11","2_No2","333_samp333"), c=xx)
#' rmEnumeratorName(xy)
#' rmEnumeratorName(xy,incl=c("anyCase","trim2","rmEnumL"))
#'
#' xz <- cbind(a=11:13, b=c("23#11","4#2","567#333"), c=xx)
#' apply(xz, 2, rmEnumeratorName, sepEnum=c("","_"), newSep="_", silent=TRUE)
#'
#' @export
rmEnumeratorName <- function(dat, nameEnum=c("Number","No","#","Replicate","Sample"), sepEnum=c(" ","-","_","/"), newSep="", incl=c("anyCase","trim2"), silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## remove or rename enumerator tag/name (or remove entire enumerator) from tailing enumerators (eg 'abc_No1' to 'abc_1'), only if found present in all instances
  ## dat (character voector or matrix)
  ## return character vector of same length as initial
  ## return-options : 1) repl/no EnumName 2) wo any enum, no digits 3) both

  fxNa <- .composeCallName(callFrom, newNa="rmEnumeratorName")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE

  out <- dat
  datOK <- length(dat) >0
  if(datOK) {
    ch1 <- if(length(dim(dat)) >0) colSums(!matrix(grepl("..[[:digit:]]+$", as.matrix(dat)), ncol=ncol(dat))) <1  else all(grepl("..[[:digit:]]+$", as.character(dat)))
    datOK <- any(ch1, na.rm=TRUE)
    useCol <- if(length(dim(dat)) >0) which(ch1) else NULL }

  if(length(nameEnum) >0) { nameEnum <- naOmit(nameEnum)}
  if(length(nameEnum) <1) { nameEnum <- c("Number","Replicate")
    if(!silent) message(fxNa,"Empty or Invalid entry for 'sepEnum', setting to default") }
  if(length(sepEnum) >0) { sepEnum <- naOmit(sepEnum)
    chDu <- duplicated(nameEnum)
    if(any(chDu)) nameEnum <- nameEnum[which(!chDu)]
  }
  if(length(sepEnum) <1) { sepEnum <- c(" ","-","_")
    if(!silent) message(fxNa,"Empty or Invalid entry for 'sepEnum', setting to default") }
  if(length(incl) <1)  incl <- NA
  if(length(newSep) >0) { newSep <- naOmit(newSep)[1]}
  if(length(newSep) <1) { newSep <- ""
    if(!silent) message(fxNa,"Empty or Invalid entry for 'newSep', setting to default") }


  if(datOK) {
    ## prepare enumerator-patterns to test
    chDu <- duplicated(nameEnum)
    if(any(chDu)) nameEnum <- nameEnum[which(!chDu)]
    if("anyCase" %in% incl) nameEnum <- unique(c(nameEnum, tolower(nameEnum), toupper(nameEnum)))
    if("trim3" %in% incl) {tmp <- 3:max(nchar(nameEnum)); nameEnum <- unique(substring(rep(nameEnum, each=length(tmp)), 1, rep(3:max(nchar(nameEnum)), length(nameEnum)))) } else {
      if("trim2" %in% incl) {tmp <- 2:max(nchar(nameEnum)); nameEnum <- unique(substring(rep(nameEnum, each=length(tmp)), 1, rep(2:max(nchar(nameEnum)), length(nameEnum)))) } else {
        if("trim1" %in% incl || "trim0" %in% incl) {tmp <- 1:max(nchar(nameEnum)); nameEnum <- unique(substring(rep(nameEnum, each=length(tmp)), 1, rep(1:max(nchar(nameEnum)), length(nameEnum)))) }}}
    if("trim0" %in% incl) {nameEnum <- unique(c(nameEnum, ""))
      sepEnum <- unique(c(sepEnum, ""))}

    chDu <- duplicated(sepEnum)
    if(any(chDu)) sepEnum <- sepEnum[which(!chDu)]
    nameEnum <- paste0(rep(sepEnum, length(nameEnum)), rep(nameEnum, each=length(sepEnum)),"[[:digit:]]+$")
    if(debug) message(fxNa,"Ready to test ",length(nameEnum)," types of enumerators")


    ## main
    if(length(nameEnum) >1) nameEnum <- nameEnum[order(nchar(nameEnum), decreasing=TRUE)]   # sort to prefer using longest version

    chEnum <- if(length(dim(dat)) ==2) apply(dat[,useCol], 2, function(y) sapply(nameEnum, function(x) all(grepl(x, y)))) else {
      sapply(nameEnum, function(x) all(grepl(x, dat)))}
    if(debug) {message(fxNa,"rEN0 .."); rEN0 <- list(dat=dat,out=out,nameEnum=nameEnum,newSep=newSep,sepEnum=sepEnum,chEnum=chEnum,newSep=newSep)}

    if(any(chEnum)) { nameEnumInd <- which(chEnum, arr.ind=length(dim(dat)) ==2)                 # each hit in new line
      if(debug) {message(fxNa,"rEN1 .."); rEN1 <- list(dat=dat,out=out,nameEnum=nameEnum,nameEnumInd=nameEnumInd,chEnum=chEnum,newSep=newSep)}
      if(length(dim(dat)) >0) {      ## input is matrix
        usePat <- rownames(nameEnumInd)[1]               # the (1st) pattern matching all input
        nameEnumInd <- nameEnumInd[1,]
        enu2 <- sub(usePat,"", dat[,useCol[nameEnumInd[2]]])         # wo nameEnumerator
        curSep <-  sepEnum[1+ ((nameEnumInd[1] -1) %/% length(sepEnum))]
        if(debug) {message(fxNa,"rEN2"); rEN2 <- list()}
        out[,useCol[nameEnumInd[2]]] <- if(length(grep("^rmEnum", incl)) >0) enu2 else {
          paste0(enu2, if(length(newSep)==1) newSep else curSep, substr(dat[,useCol[nameEnumInd[2]]], nchar(enu2) +nchar(usePat) -12, max(nchar(dat[,useCol[nameEnumInd[2]]]))))}
        if(debug && any(grepl(".L$", incl))) message("Matched matrix via column '",useCol[nameEnumInd[2]],"'")
        if(any(grepl(".L$", incl))) out <- list(dat=out, column=useCol[nameEnumInd[2]], pattern=substr(usePat, 1, nchar(usePat) -13))                  # optinal return as list including info which col was modified
      } else {                               ## input is vector
        if(length(nameEnumInd) >1) nameEnumInd <- nameEnumInd[1]
        usePat <- names(nameEnumInd)
        maxNch <- max(nchar(dat), na.rm=TRUE)
        enu2 <- sub(names(nameEnumInd),"", dat)     # wo nameEnumerator
        curSep <- sepEnum[1+ ((nameEnumInd -1) %/% length(sepEnum))]
        if(length(grep("^rmEnum", incl)) >0)  out <- enu2 else {
          out <- if(length(grep("^rmEnum", incl)) >0) enu2 else {
            paste0(enu2, if(length(newSep)==1) newSep else curSep, substr(dat, nchar(enu2) +nchar(usePat) -12, maxNch)) }
        }
        if("all" %in% incl) out <- cbind(ini=dat, new=out)
        if(debug && any(grepl(".L$", incl))) message(fxNa,"Matched vector")
        if(any(grepl(".L$", incl))) out <- list(dat=out, pattern=substr(usePat, 1, nchar(usePat) -13))                  # optinal return as list including info which col was modified
      }

    } else {
      ## check for removing enumerators (absence of add'l text)
      if(nchar(newSep) <1 && length(grep("^rmEnum", incl)) >0) {
        if(debug) {message(fxNa,"rEN4 .."); rEN4 <- list(dat=dat,out=out,nameEnum=nameEnum,newSep=newSep,sepEnum=sepEnum,chEnum=chEnum,newSep=newSep)}
        rmTailEnu <- sub(paste0("(",paste(protectSpecChar(sepEnum),collapse="|"),")", "[[:digit:]]+$"), "", dat)  # allows all combin of enumerators
        ch3 <- nchar(dat) > nchar(rmTailEnu)
        ## option for 'strict' , ie enumerator at all instances ?
        
        if(length(dim(dat)) <2 ) out <- rmTailEnu else {       
          ch4 <- if(length(dim(dat)) >1) colSums(ch3)==nrow(dat) 
          if(any(ch4)) out <- rmTailEnu[,which.min(ch4)]
        } 
      } else if(!silent && identical(out, dat)) message(fxNa,"No conistent enumerator+digit combination found; nothing to do ..")
    } 
       
  } else if(debug) message(fxNa,"Invalid or empty input; nothing to do ..")
  out }
  
