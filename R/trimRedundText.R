#' Trim redundant text
#'
#' This function allows trimming/removing redundant text-fragments (redundant from head or tail) out of chracter vector 'txt'.
#' 
#' 
#' 
#' @param txt character vector to be treated
#' @param minNchar (integer) minumin number of characters that must remain
#' @param side (character) may be be either 'both', 'left' or 'right'
#' @param spaceElim (logical) optional removal of any heading or tailing white space 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a modified character vector
#' @examples
#' x <- c("abcd","abcde","abcdefg","abcdE",NA,"abcdEF")
#' trimRedundText(x)
#' 
#' @export
trimRedundText <- function(txt, minNchar=1, side="both", spaceElim=FALSE, silent=TRUE, callFrom=NULL, debug=FALSE) {
  ##
  fxNa <- .composeCallName(callFrom, newNa=".trimFromStart")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  doTrim <- TRUE
  if(length(txt) <1) message(fxNa,"Problem : 'txt' appears empty, nothing to do") else {
    if(!is.character(txt)) { txt <- try(as.character(txt))
      if(inherits(txt, "try-error")) {txt <- NULL; doTrim <- FALSE
        warning(fxNa,"Unable to convert 'txt' into text; nothing to do")}}
  } 
  ## trimming of heading and tailing white space
  if(length(txt) >0 & spaceElim) { 
    txt <- gsub(" $","",gsub("^ ","",txt)) 
  }
  if(doTrim) { chLe <- nchar(txt)
    if(any(naOmit(chLe) ==0)) { doTrim <- FALSE
      if(sum(chLe ==0, na.rm=TRUE) + sum(is.na(chLe))==length(txt)) { message(fxNa,"NOTE : all elements appear empty ! Nothing to do ..")
  	  } else if(!silent) message(fxNa,"NOTE : ",sum(chLe==0)," elements appear empty ! Nothing to do ..") }
  	}    
  ## finish checking arguments    
  if(doTrim) {
    msg2 <- "Argument 'minNchar' should be positive integer; setting to default=1"
    if(length(minNchar) !=1 | !is.numeric(minNchar)) { minNchar <- 1
      message(fxNa,msg2)} else minNchar <- abs(as.integer(minNchar)) 
    msg1 <- "Argument 'side' should be either 'both', 'left' or 'right'; setting to default='both'" 
    if(length(side) <1) {side <- "both"; message(fxNa, msg1)}
    if(!is.character(side)) {side <- "both"; message(fxNa, msg1)} 
    ## main
    nChar <- nchar(txt)
    if(debug) {message(fxNa," ready tm trim ",pasteC(nChar)," characters"); trL <- list(txt=txt,minNchar=minNchar,nChar=nChar)}
    if(any(c("both","left") %in% side)) {      
      txt <- .trimLeft(txt, minNchar=minNchar, silent=TRUE, callFrom=fxNa)
      if(debug) {message(fxNa," .trimLeft reduced to ",pasteC(nchar(txt))," characters")}
    }
    if(any(c("both","right") %in% side)) {      
      txt <- .trimRight(txt, minNchar=minNchar, silent=TRUE, callFrom=fxNa)
      if(debug){ message(fxNa," .trimRight reduced to ",pasteC(nchar(txt))," characters")}
    }
    txt } }

#' @export
.trimLeft <- function(x, minNchar=1, silent=TRUE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa=".trimLeft")
  nChar <- nchar(x)
  msg <- c(fxNa,"Some entries are too short for trimming to min ",minNchar," characters, nothing to do")
  if(all(naOmit(nChar > minNchar))) {
    ch1 <- min(nChar, na.rm=TRUE)
    if(ch1 -1 > minNchar) {
      ch1 <- (ch1 -minNchar) :1
      ch1 <- paste0("^",substr(rep(x[which.min(nChar)], length(ch1)), 1, ch1))
      ch3 <- lapply(gsub(".","\\.",ch1), grep, x)
      x <- substring(x, nchar(ch1[which.max(sapply(ch3, length))]))
    } else {
      if(!silent) message(msg) }
  } else if(!silent) message(msg)
  x
}

#' @export
.trimRight <- function(x, minNchar=1, silent=TRUE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa=".trimRight")
  nChar <- nchar(x)
  msg <- c(fxNa,"Some entries are too short for trimming to min ",minNchar," characters, nothing to do")
  if(all(naOmit(nChar > minNchar))) {
    ch1 <- min(nChar, na.rm=TRUE)
    if(ch1 -1 > minNchar) {
      ch1 <- 1: (ch1 -minNchar) 
      ch2 <- x[which.min(nChar)]
      ch1 <- paste0(substr(paste0(rep(ch2, length(ch1))), ch1, nchar(ch2)),"$")
      ch3 <- lapply(gsub(".","\\.",ch1), grep, x)
       x <- substring(x, 1, nchar(ch1[which.max(sapply(ch3, length))]))
    } else {
      if(!silent) message(msg) }
  } else if(!silent) message(msg) 
  x
}
        
#' @export
.trimFromStart <- function(x,..., minNchar=1, silent=TRUE, callFrom=NULL) {
  ## trim, ie remove redundant characters from beginning
  ## 'minNchar' min number of characters that should remain
  y <- list(...)
  fxNa <- .composeCallName(callFrom, newNa=".trimFromStart")
  if(length(x) <  1) message(fxNa," Problem : 'x' appears empty") else {
    exclLiNa <- c("minNchar","silent","callFrom")
    exclLiNa2 <- c(sapply(nchar(exclLiNa[1]):2, function(z) substr(exclLiNa[1],1,z)),
      sapply(nchar(exclLiNa[2]):2,function(z) substr(exclLiNa[2],1,z)))
    te <- which(names(y) %in% exclLiNa2)
    if(length(naOmit(te)) > 0) y <- y[-1*which(names(y) %in% exclLiNa2)]
    if(sum(sapply(y, length) >0)) {
      x <- c(x, unlist(y)) }
    if(!silent) message(fxNa," initial no of characters  ", paste(nchar(x), collapse=" "))
    while(length(unique(substr(x, 1, 1))) <2 & min(nchar(x),na.rm=TRUE) > minNchar) x <- substr(x, 2, nchar(x)) }
  x }

#' @export
.trimFromEnd <- function(x,..., callFrom=NULL, silent=TRUE) {
  ## trim, ie remove redundant characters from beginning
  ## note: since aruguments collected by
  ## less elaborated than .trimFromStart()
  fxNa <- .composeCallName(callFrom,newNa=".trimFromEnd")
  y <- list(...)
  if(length(y) >0) {if(any(c("callFrom","silent") %in% names(y))) {
    y <- y[-1*which(names(y) %in% c("callFrom","callFr","sil","silent"))]}}
  if(sum(sapply(y,length) >0)) {   # '...' argument will be added to x
    x <- c(x,unlist(y)) }
  if(!silent) message(fxNa," initial no of characters  ",paste(nchar(x),collapse=" "))
  while(length(unique(substr(x,nchar(x),nchar(x)))) <2) x <- substr(x,1,nchar(x)-1)
  x }
  
