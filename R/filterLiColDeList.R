#' Filter lines(rows) and/or columns from all suitable elements of list
#'
#' Filter all elements of list (or S3-object) according to criteria designed to one selected reference-element of the list.
#' All simple vectors, matrix, data.frames and 3-dimensional arrays will be checked if matching number of rows and/or columns to decide if they should be filtered the same way.
#' If the reference element has same number of rows and columns simple (1-dimensional) vectors won't be filtered since it not clear if this should be done to lines or columns.  
#'
#' @details
#' This function is used eg in package wrProteo to simultaneaously filter raw and transformed data.
#'
#' @param lst (list or S3 object) main input 
#' @param useLines (integer, logcial or character) vector to assign lines to keep when filtering along lines; 
#'   set to \code{NULL} for no filtering; if '\code{allNA}' all lines composed uniquely of \code{NA} values will be removed.
#' @param useCols (integer, logcial or character) vector for filtering columns; set to \code{NULL} for no filtering; if '\code{allNA}' all columns uniquely \code{NA} values will be removed
#' @param ref (integer) index for designing the elment of 'lst' to take as reference for checking which other list-elements have suitable number of rows or columns
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return correct input (object of same class, same length)
#' @seealso \code{\link{moderTest2grp}} for single comparisons, \code{\link[limma]{lmFit}} 
#' @examples
#' lst1 <- list(m1=matrix(11:18,ncol=2), m2=matrix(21:30,ncol=2), indR=31:34, 
#'   m3=matrix(c(21:23,NA,25:27,NA),ncol=2))
#' ## here $m2 has more lines than $m1, and thus will be ignored when ref=1
#' filterLiColDeList(lst1, useLines=2:3)
#' filterLiColDeList(lst1, useLines="allNA", ref=4)
#' 
#' 
#' 
#' 
#' @export
filterLiColDeList <- function(lst, useLines, useCols=NULL, ref=1, silent=FALSE, callFrom=NULL) {
  ## filter all list-elements matching number of lines from the 'ref'-th list-element
  ## 'ref' .. designs list-element where number of rows will be compared to other list-elements to decide which list-elements (have same number of rows and thus) will be filtered
  fxNa <- .composeCallName(callFrom, newNa="filterLiColDeList")
  msg <- "invalid argument 'ref' (should be index to existing element of 'lst')"
  doFilt <- TRUE
  chLe <- c(lst=length(lst), useLines=length(useLines), ref=length(ref))
  if(any(chLe <0)) doFilt <- FALSE
  if(doFilt) {
    if(length(ref) >1) { warning("'ref' should be of length=1 (ie only ONE reference) !  Reducing to first .."); ref <- ref[1]}
    if(ref > length(lst)) stop(msg)
    dims <- lapply(lst, dim)
    if(length(useLines) >0) {
      if(identical(useLines,"allNA")) {
        chFormat <- length(dim(lst[[ref]])) >1 
        if(!chFormat) { lst[[ref]] <- as.matrix(lst[[ref]])
          message(fxNa,"It appears lst[[ref]] is not matrix (or data.frame) ! Trying to reformat ..")
          dims <- lapply(lst, dim)                           # update
        }
        useLines <- rowSums(is.na(lst[[ref]])) < ncol(lst[[ref]]) }
      if(is.logical(useLines)) {           # convert logical argument to index
        ch <- identical(length(useLines), dims[[ref]][1])
        useLines <- if(ch) which(useLines) else NULL } 
      if(is.numeric(useLines)) {
        useLines <- naOmit(useLines)
        if(min(useLines) <1 | max(useLines) > dims[[ref]][1]) {
          if(!silent) message(fxNa,"'useLines' may not design lines higher than number of lines in ref, neither may not be negative")
          useLines <- NULL }
      }  
      if(length(useLines) >0 & identical(length(useLines), dims[[ref]][1])) { 
        if(!silent) message(fxNa,"'useLines' seems empty, nothing to do ...")
      } else {
        useEl <- sapply(dims, function(x) if(length(x) >1) x[1] else 0) == dims[[ref]][1]   # compare to ref
        if(any(useEl)) for(i in which(useEl)) {lst[[i]] <- if(length(dims[[i]])==2) lst[[i]][useLines,] else lst[[i]][useLines,,]}
        if(!silent) message(fxNa,"successfully filtered ",pasteC(names(lst)[which(useEl)],quoteC="'")," from ",dims[[ref]][1]," to ",length(useLines)," lines") } 
      ## check for single vectors matching nrow of ref (as long nrow not equal ncol of ref)
      chV <- sapply(dims, length)==0  & dims[[ref]][1]==dims[[ref]][2]
      if(any(chV)) { chV <- which(chV)
        chL <- sapply(lst[chV], length) == dims[[ref]][1]
        if(any(chL)) for(i in which(chL)) {lst[[i]] <- lst[[i]][useLines]}
      }
    }
    if(length(useCols) >0) {
      if(is.logical(useCols)) {
        ch <- length(useCols) == dims[[ref]][2]
        useLines <- if(ch) which(useCols) else NULL } 
      if(is.numeric(useCols)) {
        useCols <- naOmit(useCols)
        if(min(useCols) <1 | max(useCols) > dims[[ref]][2]) {
          if(!silent) message(fxNa,"'useCols' may not design columns higher than number of columns in ref, neither may not be negative")
          useCols <- NULL }
      }  
      if(length(useCols) >0 & length(useCols)==dims[[ref]][2]) { 
        if(!silent) message(fxNa,"'useCols' seems empty, nothing to do ...")
      } else {
        useEl <- sapply(dims, function(x) if(length(x) >1) x[2] else 0) == dims[[ref]][2]
        if(any(useEl)) for(i in which(useEl)) {cat("i=",i,"\n"); lst[[i]] <- if(length(dims[[i]])==2) lst[[i]][,useCols] else lst[[i]][,useCols,]}
        if(!silent) message(fxNa,"successfully filtered ",pasteC(names(lst)[which(useEl)],quoteC="'")," from ",dims[[ref]][2]," to ",length(useCols)," columns") } 
      ## check for single vectors matching ncol of ref (as long nrow not equal ncol of ref)
      chV <- sapply(dims, length)==0  & dims[[ref]][1]==dims[[ref]][2]
      if(any(chV)) { chV <- which(chV)
        chL <- sapply(lst[chV], length) == dims[[ref]][2]
        if(any(chL)) for(i in which(chL)) {lst[[i]] <- lst[[i]][useCols] }
      }
    } } else if(!silent) message(fxNa,"Incomplete data - nothing to do; either 'lst','useLines' or 'ref' is empty !") 
  lst }
   
