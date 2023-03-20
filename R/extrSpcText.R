#' Extract specific text
#'
#' This function extracts/cuts text-fragments out of \code{txt} following specific anchors defined by arguments \code{cutFrom} and \code{cutTo}.
#'
#' @details
#' In case \code{cutFrom} is not found \code{missingAs} will be returned.
#' In case \code{cutTo} is not found, text gets extracted with \code{chaMaxEl} characters.
#'
#' @param txt character vector to be treated
#' @param cutFrom (character) text where to start cutting
#' @param cutTo (character) text where to stop cutting
#' @param missingAs (character) specific content of output at line/location of 'exclLi' 
#' @param exclFromTag (logical) to exclude text given in 'cutFrom' from result
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a modified character vector
#' @seealso  \code{\link[base]{substr}} 
#' @examples
#' extrSpcText(c(" ghjg GN=thisText PE=001"," GN=_ PE=", NA, "abcd"))
#' extrSpcText(c("ABCDEF.3-6","05g","bc.4-5"), cutFr="\\.", cutT="-")
#' @export
extrSpcText <- function(txt, cutFrom=" GN=", cutTo=" PE=", missingAs=NA, exclFromTag=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="extrSpcText")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  seFr <- gregexpr(cutFrom, txt)
  seTo <- gregexpr(cutTo, txt)
  chOc <- cbind(fr=sapply(seFr, length), to=sapply(seTo, length))
  chaFr <- if(all(chOc[,1]==1)) unlist(seFr) else sapply(seFr, function(x) x[1])   # if search-term found mult times, take 1st
  chaTo <- if(all(chOc[,2]==1)) unlist(seTo) else sapply(seTo, function(x) x[1])   # if search-term found mult times, take 1st
  exclLi <- which(chaFr <0 | is.na(txt))
  if(exclFromTag) chaFr[which(chaFr >0)] <- chaFr[which(chaFr >0)] +nchar(sub("\\\\","",cutFrom)) 
  if(any(chaTo <0)) {                                                             # gregexpr returns -1 if term not found -> not usable index
    if(!silent) message(fxNa,sum(chaTo <0,na.rm=TRUE)," 'cutTo' tags not found !")
    }
  out <- substr(txt, chaFr, chaTo-1)
  if(length(exclLi) >0) out[exclLi] <- missingAs
  out }


#' Search character-string and cut either before or after 
#'
#' This function extracts/cuts text-fragments out of \code{txt} following specific anchors defined by arguments \code{cutFrom} and \code{cutTo}.
#'
#'
#' @param x character vector to be treated
#' @param searchChar (character) text to look for
#' @param after (logical) 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a modified character vector
#' @seealso  \code{\link[base]{grep}} 
#' @examples
#' .cutAtSearch("abcdefg","de")
#' @export
.cutAtSearch <- function(x, searchChar, after=TRUE, silent=TRUE, debug=FALSE, callFrom=NULL) {
  ## search 'searchChar' within x (character vector) and cut either before or after values of 'x'
  ## for simple cutting before searchCharacter use .retain1stPart()
  ## 'after' =TRUE means that all text BEFORE 'searchChar' will be returned
  searArg <- if(!silent) deparse(substitute(searchChar)) else "searchChar"
  fxNa <- .composeCallName(callFrom, newNa=".cutAtSearch")
  pos <- gregexpr(searchChar, x)
  ch <- pos <0
  if(any(ch)) pos[which(ch)] <- -1*nchar(searchChar)
  if(max(sapply(pos, length),na.rm=TRUE) >1) {
    if(!silent) message(fxNa," multiple occurances of '",searArg,"' found")
    pos <- sapply(pos,function(z) z[1])
  } else pos <- unlist(pos)
  out <- if(after) substr(x, pos +nchar(searchChar), nchar(x)) else substr(x, 1, pos -1)
  if(!after) {ch <- out=="" & pos <0; if(any(ch)) out[which(ch)] <- x[which(ch)]}
  names(out) <- x
  out }

#' Trim character string: keep only text before 'sep'
#'
#' Trim character string: keep only text before 'sep' (length=1 !)
#' 
#' @param chr character vector to be treated
#' @param sep (character) saparator
#' @param offSet (integer) off-set
#' @return This function returns a modified character vector
#' @seealso  \code{\link[base]{substr}} 
#' @examples
#' .retain1stPart("abc = def")
#' @export
.retain1stPart <- function(chr, sep=" = ", offSet=1){
  ## trim character string: keep only text before 'sep' (length=1 !)
  ## offSet allows to cut n characteres before occurance of 'sep' (character of length=1 or neg value for cut after)
  ## error if all 'chr' NA !
  ## attention special characters as sep ('+' or '.') need to be protected !! eg # if(sep %in% c("+",".")) sep <- paste("\\",sep,sep="")
  ## see also volcDat  .cutAtSearch() for cutting after ...
  chrCh <- regexpr(sep[1], chr)
  if(any(naOmit(chrCh >0))) { trimNo <- which(chrCh >0)
    chr[trimNo] <- substr(chr[trimNo], 1, chrCh[trimNo] -offSet)}
  chr }
    
