#' Convert to numeric
#'
#' \code{convToNum} checks if input vector/character string contains numbers (with or without comma) and attempts converting to numeric.
#' This functions was designed for extracting the numeric part of character-vectors (or matrix) containing both numbers and character-elements.
#' Depending on the parameters \code{convert} and \code{remove} text-entries can be converted to NA (in resulting numeric objects) or removed (the number of elements/lines gets reduced, in consequece). 
#' Note: if 'x' is a matrix, its matrix-dimensions & -names will be preseved.
#' Note: so far Inf  and -Inf do not get recognized as numeric.
#'
#' @param x vector to be converted
#' @param spaceRemove (logical) to remove all heading and trailing (white) space (until first non-space character)
#' @param convert (character) define which type of non-conform entries to convert to NAs. Note, if \code{remove} is selected to eliminate character-entries they cannot be converted any more. Use 'allChar' for all character-entries; 'sparseChar' sparse (ie rare) character entries; \code{NA} for converting 'Na' or 'na' to \code{NA}; if 'none' or \code{NULL} no conversions at all. 
#' @param remove (character) define which type of non-conform entries to remove, removed items cannot converted to \code{NA} any more. Use 'allChar' for removing all character entries; \code{NA} for removing all instances of \code{NA} (execept thise created by converting text); all elements will be kept if 'none' or \code{NULL}. 
#' @param euroStyle (logical) if \code{TRUE} will convert all ',' (eg used as European decimal-separator) to '.' (as internally used by R as decimal-separator), thus allowing converting the European decimal format. 
#' @param sciIncl (logical) include recognizing scientific notation (eg 2e-4)
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param silent (logical) suppress messages
#' @return numeric vector (or matrix (if 'x' is matrix))
#' @seealso \code{\link[base]{numeric}}
#' @examples
#' x1 <- c("+4"," + 5","6","bb","Na","-7") 
#' convToNum(x1,convert=c("allChar"))
#' convToNum(x1)        # too many non-numeric instances for 'sparseChar'
#' 
#' x2 <- c("+4"," + 5","6","-7"," - 8","1e6","+ 2.3e4","-3E4","- 4E5") 
#' convToNum(x2,convert=NA,remove=c("allChar",NA))
#' convToNum(x2,convert=NA,remove=c("allChar",NA),sciIncl=FALSE)
#' @export 
convToNum <- function(x,spaceRemove=TRUE,convert=c(NA,"sparseChar"),remove=NULL,euroStyle=TRUE,sciIncl=TRUE,callFrom=NULL,silent=TRUE) {
  fxNa <- .composeCallName(callFrom,newNa="convToNum")
  if(!is.numeric(x)) {
    if(is.factor(x)) x <- as.character(x)
    if(spaceRemove) x <- sub("^ +","",sub(" +$","",x))
    if(euroStyle) x <- sub(",",".",x)
    if("none" %in% convert) convert <- NULL
    if("none" %in% remove) remove <- NULL
    ## convert NA-like text (if specified via 'convert')
    if(length(convert) >0) if("na" %in% tolower(convert)) convert[which(tolower(convert) %in% "na")] <- NA
    if(length(remove) >0) if("na" %in% tolower(remove)) convert[which(tolower(remove) %in% "na")] <- NA
    #if(any(is.na(convert))) { TRUE}
    if(any(is.na(convert))) { naCh <- tolower(x) %in% "na" | tolower(x) %in% "nan"
      if(any(naCh)) x[which(naCh)] <- NA}
    if("NA" %in% remove | any(is.na(remove))) {chNa <- is.na(x); if(any(chNa)) x <- x[-1*which(chNa)]}
    aa <- c("[[:digit:]]+\\.[[:digit:]]+$","[[:digit:]]+$")
    ab <- paste("^",rep(c("","\\+ *","\\- *"),each=2),rep(aa,3),sep="")
    check <- grep(paste(ab,collapse="|"),x)
    if(sciIncl) {
      ac <- c("[[:digit:]]+","[[:digit:]]+\\.[[:digit:]]+")
      ac <- matrix(c(rep("^",24),rep(c("","\\+ *","\\- *"),8),rep(ac,12),rep(c("E","e"),each=12),rep(c("","","-","-"),6),rep(ac[1],24),rep("$",24)),nrow=24)
      ac <- apply(ac,1,paste,collapse="")
      check2 <- unlist(sapply(ac,grep,x))
      check <- sort(unique(c(check,check2))) }
    ## include NAs to check (unless specified to remove)
    chNa <- is.na(x)
    if(!any(is.na(remove)) & any(chNa)) check <- sort(unique(c(check,which(chNa))))    
    ## option remove all character entries
    if(length(remove) >0) {
      msg <- NULL
      if(any(is.na(remove))) { chNa <- is.na(x)
        if(any(chNa)) {x <- x[-1*which(chNa)]
        msg <- c("  remove ",sum(chNa)," NAs !")
        check <- check[-1*which(check %in% which(chNa))]} }
      if("allChar" %in% remove) { x <- if(length(check) >0) x[check] else NULL
        if(length(x) -length(check) >0) msg <- c(if(length(msg) >0) c(msg," ;"),"  remove 'allChar': ",
          length(x) -length(check)," text-elements removed allowing conversion to numeric !")
        check <- if(length(x) >0) 1:length(x) else NULL }
      if(!silent & length(msg) >0) if(paste(nchar(msg),collapse="") >3) message(fxNa,msg)
    }
    ## option: convert (sparse) character entries to NA
    if(length(x) >0) {if(any(c("allChar","sparseChar") %in% convert) & !"allChar" %in% remove) { 
      msg <- conv <- NULL
      if("allChar" %in% convert) { conv <- if(length(check) >0) (1:length(x))[-1*check] else 1:length(x)
        if(length(x) >length(check)) msg <- c(" option convert='allChar': replacing ",length(x)-length(check)," character-entries by NA !")
      } else if("sparseChar" %in% convert & length(check) > ceiling(length(x)*0.7)) {
        conv <- (1:length(x))[-1*check] }
      if(length(conv) >0) {
        msg <- c(" replacing ",length(conv)," 'sparse' character-entries by NA !")
        x[conv] <- NA; check <- sort(unique(c(check,conv))) }
      if(length(msg) >0 & !silent) if(paste(nchar(msg),collapse="") >3) message(fxNa,msg)  
    }
    if(length(check) >0) x[check] <- sub(" ","",x[check])
    chNa <- is.na(x) 
    x <- if(length(x) <1) NULL else {if(identical(1:length(x),check)) as.numeric(x) else {
      if(identical(1:length(x),sort(unique(check,which(chNa))))) as.numeric(x) else x}}}      
  }
  x }     
  
