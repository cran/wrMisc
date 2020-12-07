#' Extract specific text
#'
#' \code{extrSpcText} extracts/cuts text-fragments out of 'txt' following specific anchors 'cutFrom' and 'cutTo'.
#' In case 'cutFrom' not found 'missingAs' will be returned.
#' In case 'cutTo' not found text gets extracted with 'chaMaxEl' characters.
#' @param txt character vector to be treated
#' @param cutFrom (character) text where to start cutting
#' @param cutTo (character) text where to stop cutting
#' @param missingAs (character) specific content of output at line/location of 'exclLi' 
#' @param exclFromTag (logical) to exclude text given in 'cutFrom' from result
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return modified character vector
#' @examples
#' extrSpcText(c(" ghjg GN=thisText PE=001"," GN=_ PE=",NA,"abcd"))
#' extrSpcText(c("ABCDEF.3-6","05g","bc.4-5"),cutFr="\\.",cutT="-")
#' @export
extrSpcText <- function(txt,cutFrom=" GN=",cutTo=" PE=",missingAs=NA,exclFromTag=TRUE,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="extrSpcText")
  seFr <- gregexpr(cutFrom,txt)
  seTo <- gregexpr(cutTo,txt)
  chOc <- cbind(fr=sapply(seFr,length),to=sapply(seTo,length))
  chaFr <- if(all(chOc[,1]==1)) unlist(seFr) else sapply(seFr,function(x) x[1])   # if search-term found mult times, take 1st
  chaTo <- if(all(chOc[,2]==1)) unlist(seTo) else sapply(seTo,function(x) x[1])   # if search-term found mult times, take 1st
  exclLi <- which(chaFr <0 | is.na(txt))
  if(exclFromTag) chaFr[which(chaFr >0)] <- chaFr[which(chaFr >0)]+nchar(sub("\\\\","",cutFrom)) 
  if(any(chaTo <0)) {                                                             # gregexpr returns -1 if term not found -> not usable index
    if(!silent) message(fxNa,sum(chaTo <0,na.rm=TRUE)," 'cutTo' tags not found !")
    }
  out <- substr(txt,chaFr,chaTo-1)
  if(length(exclLi) >0) out[exclLi] <- missingAs
  out }

#' @export
.cutAtSearch <- function(x,searchChar,after=TRUE,silent=TRUE,callFrom=NULL) {
  ## search 'searchChar' within x (character vector) and cut either before or after values of 'x'
  ## for simple cutting before searchCharacter use .retain1stPart()
  ## 'after' =TRUE means that all text BEFORE 'searchChar' will be returned
  searArg <- if(!silent) deparse(substitute(searchChar)) else "searchChar"
  fxNa <- .composeCallName(callFrom,newNa=".cutAtSearch")
  pos <- gregexpr(searchChar,x)
  ch <- pos <0
  if(any(ch)) pos[which(ch)] <- -1*nchar(searchChar)
  if(max(sapply(pos,length),na.rm=TRUE) >1) {
    if(!silent) message(fxNa," multiple occurances of '",searArg,"' found")
    pos <- sapply(pos,function(z) z[1])
  } else pos <- unlist(pos)
  out <- if(after) substr(x,pos+nchar(searchChar),nchar(x)) else substr(x,1,pos-1)
  if(!after) {ch <- out=="" & pos <0; if(any(ch)) out[which(ch)] <- x[which(ch)]}
  names(out) <- x
  out }

#' @export
.trimFromStart <- function(x,...,minNchar=1,silent=TRUE,callFrom=NULL) {
  ## trim, ie remove redundant characters from beginning
  ## 'minNchar' min number of characters that should remain
  y <- list(...)
  fxNa <- .composeCallName(callFrom,newNa=".trimFromStart")
  if(length(x) <1) message(fxNa," Problem : 'x' appears empty") else {
    exclLiNa <- c("minNchar","silent","callFrom")
    exclLiNa2 <- c(sapply(nchar(exclLiNa[1]):2,function(z) substr(exclLiNa[1],1,z)),
      sapply(nchar(exclLiNa[2]):2,function(z) substr(exclLiNa[2],1,z)))
    te <- which(names(y) %in% exclLiNa2)
    if(length(naOmit(te)) > 0) y <- y[-1*which(names(y) %in% exclLiNa2)]
    if(sum(sapply(y,length) >0)) {
      x <- c(x,unlist(y)) }
    if(!silent) message(fxNa," initial no of characters  ",paste(nchar(x),collapse=" "))
    while(length(unique(substr(x,1,1))) <2 & min(nchar(x),na.rm=TRUE) > minNchar) x <- substr(x,2,nchar(x)) }
  x }

#' @export
.trimFromEnd <- function(x,...,callFrom=NULL,silent=TRUE) {
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

#' @export
.retain1stPart <- function(chr,sep=" = ",offSet=1){
  ## trim character string: keep only text before 'sep' (length=1 !)
  ## offSet allows to cut n characteres before occurance of 'sep' (character of length=1 or neg value for cut after)
  ## error if all 'chr' NA !
  ## attention special characters as sep ('+' or '.') need to be protected !! eg # if(sep %in% c("+",".")) sep <- paste("\\",sep,sep="")
  ## see also volcDat  .cutAtSearch() for cutting after ...
  chrCh <- regexpr(sep[1],chr)
  if(any(naOmit(chrCh >0))) { trimNo <- which(chrCh >0)
    chr[trimNo] <- substr(chr[trimNo],1,chrCh[trimNo]-offSet)}
  chr }
    
