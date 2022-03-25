#' Summarize columns (as median,mean,min,last or other methods)
#'
#' \code{summarizeCols} summarizes all columns of matrix (or data.frame).
#' In case of text-columns the sorted middle (~median) will be given, unless 'maxLast', 'minLast',
#'  'maxLast','maxAbsLast' or 'minLast' .. consider only last column of 'matr' : choose from all columns the line where (max of) last col is at min;
#'  'medianComplete' or 'meanComplete' consideres only lines/rows where no NA occur (NA have influence other columns !)
#' @param matr data.frame matrix of data to be summarized by comlumn (may do different method for text and numeric comlumns)
#' @param meth (character) summarization method (eg 'maxLast','minLast','maxLast','maxAbsLast', 'minLast', 'medianComplete' or 'meanComplete')
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return vector with summary for each column
#' @seealso \code{rowMeans} in \code{\link[base]{colSums}}
#' @examples
#' t1 <- matrix(round(runif(30,1,9)), nc=3); rownames(t1) <- letters[c(1:5,3:4,6:4)]
#' summarizeCols(t1, me="median")
#' t(sapply(by(t1,rownames(t1), function(x) x), summarizeCols,me="maxLast"))
#' t3 <- data.frame(ref=rep(11:15,3), tx=letters[1:15],
#'   matrix(round(runif(30,-3,2),1), ncol=2), stringsAsFactors=FALSE)
#' by(t3,t3[,1], function(x) x)
#' t(sapply(by(t3,t3[,1], function(x) x), summarizeCols,me="maxAbsLast"))
#' @export
summarizeCols <- function(matr, meth="median", silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## summarize all columns of matrix (or data.frame) 'x' (most methods will call apply)
  ## in case of text-columns the sorted middle (~median) will be given, unless 'maxLast' or 'minLast'
  ##   'maxLast','maxAbsLast' or 'minLast' .. consider only last column of 'matr' : choose from all columns the line where (max of) last col is at min,max...
  ##   'medianComplete' or 'meanComplete' consideres only lines/rows where no NA occur (NA have influence other columns !)
  ## return vector with summary for each column
  fxNa <- .composeCallName(callFrom, newNa="summarizeCols")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  argOpt <- c("median","mean","aver","average","min","max","maxOfRef","minOfRef","maxAbsOfRef","firstLi","lastLi","first","last")
  argOpt <- c(argOpt,paste(argOpt,"Complete",sep=""),"Null")  
  txt <- c("Argument '","' should be "," seeting to first/default (meth='median')")
  if(length(dim(matr)) <2) {if(!silent) message(fxNa,txt[1],"matr",txt[2],"matrix or data.frame")
    if(length(dim(matr)) <1) {meth <- "Null"; matr=matrix(NA)} else matr <- as.matrix(matr)}
  if(length(meth) <1 & !silent) {message(fxNa,txt[3]); meth="median"}
  if(length(meth) !=1) {if(!silent) message(fxNa,txt[1],"meth",txt[2]," of length=1, ",txt[3]); meth <- meth[1]}
  maxLaArg2 <- cbind(old=c("maxAbsLast","maxLast","minLast"),new=c("maxOfRef","minOfRef","maxAbsOfRef"))
  if(meth %in% maxLaArg2[,1]) { tmp <- names(meth)
    meth <- maxLaArg2[which(meth %in% maxLaArg2[,1]),2]
    if(!silent) message(fxNa,"Argument 'meth' renamed to '",meth,"'") } 
  if(!meth %in% argOpt) stop(fxNa,txt[1],"meth",txt[2],"either one of :",pasteC(argOpt,quoteC="'"))
  colMod <- rep(NA,ncol(matr))
  for(i in 1:ncol(matr)) colMod[i] <- mode(matr[,i])
  colMod <- colMod != "numeric"                                                 # check which col is not numeric
  if(meth=="Null") out <- NULL else { if(any(colMod)) {                                # has character columns ..
    if(debug) message(fxNa," any(maxLast,minLast) %in% meth  ",any(c("maxLast","minLast") %in% meth))
    if(any(c("maxLast","maxAbsLast","minLast") %in% meth)) out <- .summarizeCols(matr,meth) else {
      out <- rep(NA, ncol(matr))
      out[which(colMod)] <- if(sum(colMod)==1) .sortMid(matr[,which(colMod)]) else apply(matr[,which(colMod)], 2, .sortMid)
      if(any(!colMod)) out[which(!colMod)] <- .summarizeCols(matr[,which(!colMod)],meth)    # and put in initial order
    } 
  } else out <- .summarizeCols(matr,meth)}  
  out }
  
#' @export
.summarizeCols <- function(x,me=c("median","medianComplete","mean","meanComplete","aver","average","min","max","maxOfRef","minOfRef",
  "maxAbsOfRef","lastLi","last","firstComplete","first","firstLi","summary"),vectAs1row=TRUE) {
  ## summarize columns of matrix (or data.frame) 'x' using apply
  ## CANNOT handle character entries !  (all results will be NA)
  ## 'vectAs1row' .. if TRUE will interprete non-matrix 'x' as matrix with 1 row (correct effect of automatic conversion when extracting 1 line)
  ## me='maxOfRef','maxAbsOfRef' or 'minOfRef': return line where last col of 'x' is at (first) max (or min) ...
  ## me='lastLi' .. return last line of 'x'
  ## any term of me containing 'Complete' (eg 'firstComplete' )..  first filter to lines of 'x' wo any NA
  ## me='medianComplete' .. median only of 'x' where no NA per line
  ## me='summary' will return matrix instead of vector !! (eah col for init cols of 'x')
  if(me=="med") me <- "median" 
  if(me %in% c("av","aver","average")) me <- "mean"                                   # synonyms ..
  if(length(dim(x)) <2) x <- if(vectAs1row) matrix(x,nrow=1,dimnames=list(NULL,names(x))) else as.matrix(x)
  if(length(grep("Complete",me)) >0) {                                              # reduce x to complete rows only 
    compl <- which(rowSums(is.na(x)) <1)
    me <- sub("Complete","",me)                                                    # term 'Complete' disappears from me ...
    if(length(compl) <1) me <- "Null"                                              # this way output will be NULL if 0 lines wo NAs
    x <- if(length(compl) >1) x[compl,] else if(length(compl) ==1) matrix(x[compl,],nrow=1,dimnames=list(rownames(x)[compl],colnames(x)))}
  switch(me, maxOfRef=x[which.max(x[,ncol(x)]),], minOfRef=x[which.min(x[,ncol(x)]),],
    maxAbsOfRef=x[which.max(abs(x[,ncol(x)])),], Null=NULL,
    median=apply(x,2,stats::median,na.rm=TRUE), mean=colMeans(x,na.rm=TRUE),
    max=apply(x,2,max,na.rm=TRUE), min=apply(x,2,min,na.rm=TRUE),
    summary=apply(x,2,summary),
    lastLi= x[nrow(x),], last= x[nrow(x),], firstLi= x[1,], first=x[1,])}
         
