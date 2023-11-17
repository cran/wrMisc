#' Combine Redundant Lines In List
#'
#' This function provides help for combining/summarizing lines of numeric data which may be summaried according to reference vector or matrilst (part of the same input-list).
#' Initial data and reference will be aligned based on rownames and the content of reference (or the column specified by \code{refColNa}). 
#' 
#' @details 
#' The first element of \code{listNa} is supposed to be the name of the list-element to be used as reference. 
#' Different order of \code{listNa} is possible when the vector \code{listNa} is named (using 'ref' or 'annot' to specify the reference), see also last example.
#' 
#' The initial list may contain multiple matrixes which will all be summarized by the same procedure as long as they are specified by \code{listNa}. 
#'
#' Please note that list elements from input not specified by \code{listNa} will be lost, ie given as emty list-elements.
#'
#' @param lst (list) main input
#' @param listNa (character)   names of list-elements containing quantitation data (1st position) and protein/line annotation (2nd position)
#' @param refColNa (character) in case the list-element to be used as reference is \code{matrix} or \code{data.frame}, the column to be used must be specified here
#' @param summarizeType (character) the summarization method gets specified here; so far 'sum','av','med','first' and 'last' are implemented
#' @param NA.rm (logical) pass to summarizing functions order to omit \code{NA}s, defaults to \code{TRUE} 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' 
#' @seealso \code{\link[graphics]{image}}
#' @return This function returns a list of same length as input
#' @examples
#' x1 <- list(quant=matrix(11:34, ncol=3, dimnames=list(letters[8:1], LETTERS[11:13])), 
#'   annot=matrix(paste0(LETTERS[c(1:4,6,3:5)],LETTERS[c(1:4,6,3:5)]), ncol=1, 
#'   dimnames=list(paste(letters[1:8]),"xx")) )
#' combineRedundLinesInListAcRef(lst=x1, listNa=c("annot","quant"), refColNa="xx")
#' combineRedundLinesInListAcRef(lst=x1, listNa=c(quant="quant",ref="annot"), refColNa="xx")
  
#' @export
combineRedundLinesInListAcRef <- function(lst, listNa=c("ref","quant"), refColNa="xx", summarizeType="av", NA.rm=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## combine lines of (matrix) list-elements according to (line-) grouping given by reference (also part of list x); 
  ## name of reference should be 1st of listNa or named; in case reference is matrix, a specific column (refColNa) may be chosen 
  fxNa <- .composeCallName(callFrom, newNa="combineRedundLinesInList")
  namesXY <- deparse(substitute(lst))
  out <- NULL
  if(debug) message(fxNa,"cRLL0") 
  ## check listNa
  msg1 <- c(" entry for 'listNa' (must design at least 2 list-elements of '",namesXY,"', 1st sould be annotation, others numeric matrix)")
  if(length(listNa) >1) { 
    if(is.numeric(listNa)) {  listNa <- as.integer(listNa)
      if(any(listNa <1 | listNa > length(lst))) stop(fxNa,"Invalid (numeric)", msg1)
    } else {   
      lstMatch <- match(listNa, names(lst)) 
      chNA <- is.na(lstMatch)
      if(sum(!chNA) <2 || chNA[1]) stop(fxNa, msg1)
      if(any(chNA)) { refColNa <- naOmit(refColNa)
        if(!silent) message(fxNa,"Removing ",sum(chNA)," element(s) not found in 'refColNa'") } }
  } else stop(fxNa,"Insufficient", msg1)              #listNa[1] <- "ref"

  if(length(refColNa) <1) refColNa <- "GeneName"
  if(length(refColNa) >1) {refColNa <- refColNa[1]; if(!silent) message(fxNa,"Argument 'refColNa' should be of length 1, trimming ..")} 
  ## sort listNa to have annot at beginning
  if(length(names(listNa)) ==length(listNa)) { 
    chO <- naOmit(match(c("ref","reference","annot"), tolower(names(listNa)) ))   # look for name indicating reference
    if(length(chO) >0) {if(chO[1] != 1) { if(debug) message(fxNa,"Changing order of 'listNa' based on names of 'listNa'")
      listNa <- listNa[c(chO[1], (1:length(listNa))[-chO[1]])] }}
  }
  if(debug) {message(fxNa,"cRLL1"); cRLL1 <- list(lst=lst,listNa=listNa,refColNa=refColNa,NA.rm=NA.rm,summarizeType=summarizeType ) }

  ## re-order listNa to have annotation as 1st
  nuI <- naOmit(match(listNa[-1], names(lst)))        # numeric elements of lst (to summarize)

  ## check presence of 'refColNa' in lst
  if(length(dim(lst[[listNa[1]]])) >1) {
    ch1 <- refColNa %in% colnames(lst[[listNa[1]]])
    if(!any(ch1)) stop(fxNa,"Missing column '",refColNa,"' in ",namesXY[1],"$",listNa[1])}
   
  chNa <- is.na(nuI)
  if(length(listNa) <2 || all(chNa)) stop("Invalid entry for 'listNa' (must design at least one numeric matrix of 'lst')")
  if(any(chNa)) message(fxNa,"Elements ",pasteC(listNa[which(chNa)]),"missing")
  ## need to adjust $annot to order of numeric matrix(es) (assume same order for num matr !)  
  xAnn <- which(names(lst) ==listNa[1])
  annS <- if(length(dim(lst[[xAnn]])) >1) lst[[xAnn]][match(rownames(lst[[xAnn]]), rownames(lst[[nuI[1]]])),] else lst[[xAnn]][match(names(lst[[xAnn]]), rownames(lst[[nuI[1]]]))]     # adj order of rows for annot
  out <- list()
  newNa <- if(length(dim(annS)) >1) tapply(rownames(annS), annS[,refColNa], paste, collapse=",") else tapply(names(annS), annS, paste, collapse=",")   
  newNFi <- if(length(dim(annS)) >1) tapply(rownames(annS), annS[,refColNa], function(z) z[1]) else tapply(names(annS), annS, function(z) z[1]) 
           
  ## prepare for multiple summarizing options/functions via argument summarizeType
  sumFx <- function(z, meth)  {
    if(length(dim(z)) <2) stop("Invalid entry 'z' (should be matrix)")
    if(any(c("aver","average", "mean") %in% meth)) meth <- "av"
    if(any(c("median") %in% meth)) meth <- "med"
    out <- try(switch(meth, sum=colSums(z, na.rm=NA.rm), av=colMeans(z, na.rm=NA.rm), med=apply(z, 2, stats::median, na.rm=NA.rm), first=z[1,], last=z[nrow(z),]), silent=TRUE)
    if(inherits(out, "try-error")) out <- NULL
    out }
  if(debug) { message(fxNa,"cRLL2")}
  ## prepare $annot
  if(length(dim(annS)) >1) {
    out[[xAnn]] <- annS[which(!duplicated(annS[,refColNa], fromLast=TRUE)),]     # remove replicates
    out[[xAnn]] <- out[[xAnn]][order(out[[xAnn]][,refColNa]),]                   # sort (as output from by)
  } else out[[xAnn]] <- sort(annS[which(!duplicated(annS, fromLast=TRUE))])
  out[[xAnn]] <- cbind(out[[xAnn]], firstIniName=if(length(dim(out[[xAnn]])) >1) rownames(out[[xAnn]]) else names(out[[xAnn]]),
    allIniNames=tapply(if(length(dim(lst[[xAnn]])) >1) rownames(lst[[xAnn]]) else names(lst[[xAnn]]), lst[[xAnn]], paste, collapse=","))
  if(colnames(out[[xAnn]])[1]=="") colnames(out[[xAnn]])[1] <- if(length(refColNa)==1) refColNa else "ref"     # reintroduce initial column name

  ## now summarize
  if(debug) { message(fxNa,"cRLL3; Summarizing ",length(nuI)," elements as ",summarizeType)}
  for(i in nuI) {
    yy <- lst[[i]]        # shortcut - use 1st of data-elements
    yz <- unlist(by(yy, if(length(dim(annS)) >1) annS[,refColNa] else annS, sumFx, summarizeType))  # summarize, not yet matrix
    if(length(yz) >0) out[[i]] <- matrix(yz, byrow=TRUE, ncol=ncol(yy), dimnames=list(sort(unique(if(length(dim(annS)) >1) annS[,refColNa] else annS)), colnames(yy))) }    
  names(out) <- names(lst) 
  ## 
  rownames(out[[xAnn]]) <- rownames(out[[nuI[1]]])    # harmonize rownames  
  ## note : other list-elements will appear empty (remove ?)    
  out }
  
