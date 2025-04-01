#' Summarize columns (as median,mean,min,last or other methods)
#'
#' \code{summarizeCols} summarizes all columns of matrix (or data.frame).
#' In case of text-columns the sorted middle (~median) will be given, unless 'maxAbsLast', 'minAbsLast',
#'  .. consider only last column of 'matr' : choose from all columns the line where (max of) last col is at min;
#'  'medianComplete' or 'meanComplete' consideres only lines/rows where no NA occur (NA have influence other columns !)
#' 
#' @details
#' The argument \code{method} allows options that treat (summarize) all columns independently or to select one line (based on argument \code{refCol})
#' 
#' @param matr data.frame matrix of data to be summarized by comlumn (may do different method for text and numeric comlumns)
#' @param meth (character) summarization method, may be 'mean','aver','median','sd','CV', 'min','max','first','last','maxOfRef','minOfRef','maxAbsLast','minAbsLast',
#'  'medianComplete' or 'meanComplete', 'n' (number of non-\code{NA} elements),'n.NA' (number of \code{NA} elements), 'NULL' (returns \code{NULL})
#' @param refCol (character or integr) column to be used as reference
#' @param nEqu (logical) if \code{TRUE}, add additional column indicating the number of equal lines for choice (only with min or max) 
#' @param supl (numeric) supplemental parameters for the various summarizing functions (eg used with \code{meth="trimmedMean", supl=c(l=0.1,u=0.2)} to pass arguments to \code{\link{trimmedMean}})
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allows easier tracking of messages produced
#' @return vector with summary for each column
#' @seealso \code{\link[base]{colSums}}; if data has subgroups to be used in a \code{\link[base]{tapply}}-way please see \code{\link{makeNRedMatr}}
#' @examples
#' t1 <- matrix(round(runif(30,1,9)),nc=3); rownames(t1) <- letters[c(1:5,3:4,6:4)]
#' summarizeCols(t1, me="median")
#' t(sapply(by(t1,rownames(t1), function(x) x), summarizeCols,me="maxAbsLast"))
#' t3 <- data.frame(ref=rep(11:15,3), tx=letters[1:15],
#'   matrix(round(runif(30,-3,2),1), ncol=2), stringsAsFactors=FALSE)
#' by(t3,t3[,1], function(x) x)
#' by(t3,t3[,1], function(x) summarizeCols(x, me="maxAbsLast"))
#' t(sapply(by(t3, t3[,1], function(x) x), summarizeCols, me="maxAbsLast"))
#' @export
summarizeCols <- function(matr, meth="median", refCol=NULL, nEqu=FALSE, supl=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## summarize all columns of matrix (or data.frame) 'x' (most methods will call apply)
  ## in case of text-columns the sorted middle (~median) will be given, unless 'maxAbsLast' or 'minLast'
  ##   'maxAbsLast' or 'minLast' .. consider only last column of 'matr' : choose from all columns the line where (max of) last col is at min,max...
  ##   'medianComplete' or 'meanComplete' consideres only lines/rows where no NA occur (NA have influence other columns !)
  ## return vector with summary for each column
  fxNa <- .composeCallName(callFrom, newNa="summarizeCols")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  argOpt <- c("median","mean","aver","average","trimmedMean","min","max","maxAbsLast","minAbsLast","maxOfRef","minOfRef","maxAbsOfRef","minAbsOfRef","firstLi","lastLi","first","last","sd","CV")
  argOpt <- c(argOpt, paste0(argOpt,"Complete"), "n","n.NA","Null")  
  txt <- c("Argument '","' should be "," seeting to first/default (meth='median')")
  if(length(dim(matr)) <2) {if(!silent) message(fxNa,txt[1],"matr",txt[2],"matrix or data.frame")
    if(length(dim(matr)) <1) {meth <- "Null"; matr=matrix(NA)} else matr <- as.matrix(matr)}
  if(length(meth) <1 && !silent) { message(fxNa,txt[3]); meth <- "median"}
  if(length(meth) >1) {if(!silent) message(fxNa,txt[1],"meth",txt[2]," of length=1, ",txt[3]); meth <- meth[1]}  
  if(!meth %in% argOpt) stop(fxNa,txt[1]," meth ",txt[2]," either one of : ",pasteC(argOpt,quoteC="'"))
  if(debug) { message(fxNa, "sC1"); sC1 <- list(matr=matr,meth=meth,nEqu=nEqu,refCol=refCol)}
  if(length(nEqu)==1 && isTRUE(nEqu)) nEqu <- grepl("(max)|(min)Of", meth)

  ## main
  if(meth=="Null") out <- NULL else { 
    if(grepl("Ref$", meth) && length(refCol)==1) {
      ## prepare for reference column
      chRe <- try(matr[,refCol], silent=TRUE)
      if(inherits(chRe, "try-error")) {
        meth <- sub("[[:alpha:]]+OfRef","", meth)
        if(!silent) message(fxNa,"Invalid entry for refCol, ignoring, setting meth to ",meth)
        refCol <- NULL 
      } else { matr <- cbind(matr, ref=chRe) }    # add ref for single-column methods
    }
    if(grepl("Last$", meth)) meth <- sub("Last$","OfRef", meth)
    
    ## now summarize
    if(debug) { message(fxNa, "sC2"); sC2 <- list()}
    out <- .summarizeCols(matr, me=meth, nEq=nEqu, supl=supl, silent=silent,debug=debug,callFrom=fxNa)
    if(grepl("Ref$", meth) && length(refCol)==1) out <- out[,-ncol(out)]      # in case of refLi - remove added col
  }    
  out }
  

#' Summarize columns of matrix (or data.frame) 'x' using apply (main)
#'
#' This function summarizes columns of matrix (or data.frame) 'x' using apply
#' In case of character entries the 'median' of sorted values will be returned
#' 
#'  
#' @param x data.frame matrix of data to be summarized by comlumn 
#' @param me (character, length=1) summarization method (eg 'min','max','mean','mean.trim','median','sd','CV', 'medianComplete' or 'meanComplete' etc, see \code{\link{summarizeCols}})
#' @param nEq (logical) if TRUE, add additional column indicating the number of equal lines for choice (only with min or max) 
#' @param vectAs1row (logical) if TRUE will interprete non-matrix 'x' as matrix with 1 row (correct effect of automatic conversion when extracting 1 line) 
#' @param supl (numeric) supplemental parameters for the various summarizing functions (currently used with 'me=mean.trim' to assign upper and lower trimming fraction, passed to )
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return vector with summary for each column (unless 'me=="summary"', in this case a matrix or list will be returned )
#' @seealso \code{\link{summarizeCols}},  \code{\link[base]{table}} \code{\link[base]{table}}
#' @examples
#' m1 <- matrix(c(28,27,11,12,11,12), nrow=2, dimnames=list(1:2,c("y","x","ref")))
#' .summarizeCols(m1, me="median")
#' @export
.summarizeCols <- function(x, me="median", nEq=FALSE, vectAs1row=TRUE, supl=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## summarize columns of matrix (or data.frame) 'x' using apply
  ## CANNOT handle character entries !  (all results will be NA)
  ## 'vectAs1row' .. if TRUE will interprete non-matrix 'x' as matrix with 1 row (correct effect of automatic conversion when extracting 1 line)
  ## me='maxOfRef','maxAbsOfRef' or 'minOfRef': return line where last col of 'x' is at (first) max (or min) ...
  ## me may be "median","medianComplete","mean","meanComplete","aver","average","trimmedMean","mean.trim","sd","CV","min","max","maxOfRef","minOfRef", "maxAbsOfRef","lastLi","last","firstComplete","first","firstLi","summary"
  ## me='lastLi' .. return last line of 'x'
  ## any term of me containing 'Complete' (eg 'firstComplete') ..  first filter to lines of 'x' wo any NA
  ## me='medianComplete' .. median only of 'x' where no NA per line
  ## me='summary' will return matrix instead of vector !! (eah col for init cols of 'x')
  fxNa <- .composeCallName(callFrom, newNa=".summarizeCols")
  if(me %in% c("me","med")) me <- "median" 
  if(me %in% c("av","aver","average")) me <- "mean"                                     # synonyms ..
  if(me %in% c("trimmedMean","trimMean")) me <- "mean.trim"                             # synonyms ..
  if(length(dim(x)) <2) {
    x <- if(vectAs1row) matrix(x, nrow=1, dimnames=list(NULL,names(x))) else as.matrix(x)        
    numCol <- 1
    txtCol <- NULL
  } else { if(is.data.frame(x)) {                                   # check for text
      numCol <- which(sapply(x, is.numeric))
      txtCol <- if(length(numCol) == ncol(x)) NULL else (1:ncol(x))[-numCol] 
    } else { numCol <- if(is.numeric(x)) 1:ncol(x) else NULL; txtCol <- if(is.numeric(x)) NULL else 1:ncol(x)}}
  if(length(numCol) <1 && any(c("mean","median") %in% me)) { me <- "first"
    if(!silent) message(fxNa,"Invalid method for all character-data, setting me to 'first'") }  
  
  if(length(grep("Complete", me)) >0) {                                              # reduce x to complete rows only 
    compl <- which(rowSums(is.na(x)) <1)
    me <- sub("Complete","",me)                                                    # term 'Complete' disappears from me ...
    if(length(compl) <1) me <- "Null"                                              # this way output will be NULL if 0 lines wo NAs
    x <- if(length(compl) >1) x[compl,] else if(length(compl) ==1) matrix(x[compl,], nrow=1, dimnames=list(rownames(x)[compl],colnames(x)))}
  if(debug) {message(fxNa,"sCc1   me=",me); sCc1 <- list()}
  
  out <- switch(me,  Null=NULL,
    maxOfRef= if(is.numeric(x[,ncol(x)])) x[which.max(x[,ncol(x)]),] else x[which(x[,ncol(x)]== sort(x[,ncol(x)], decreasing=TRUE)),],   # checks last column only
    minOfRef= if(is.numeric(x[,ncol(x)])) x[which.min(x[,ncol(x)]),] else x[which(x[,ncol(x)]== sort(x[,ncol(x)], decreasing=FALSE)),],
    maxAbsOfRef= if(is.numeric(x[,ncol(x)])) x[which.max(abs(x[,ncol(x)])),] else x[which(x[,ncol(x)]== sort(x[,ncol(x)], decreasing=TRUE)),],   # checks last column only
    minOfRef= if(is.numeric(x[,ncol(x)])) x[which.min(abs(x[,ncol(x)])),] else x[which(x[,ncol(x)]== sort(x[,ncol(x)], decreasing=FALSE)),],
    median=apply(x[,numCol], 2, stats::median, na.rm=TRUE),    #   all columns
    mean=colMeans(x[,numCol], na.rm=TRUE),
    mean.trim=apply(x, 2, trimmedMean, trim=supl, callFrom=fxNa, silent=silent), 
    top3mean=NA,   # apply(x, 2, top3mean, callFrom=fxNa),
    max=apply(x, 2, max, na.rm=TRUE), 
    min=apply(x, 2, min, na.rm=TRUE),
    summary=if(is.matrix(x)) apply(x, 2, summary) else sapply(x, summary),
    sd=colSds(x, callFrom=fxNa),
    CV=colCVs(x, callFrom=fxNa),
    n=colSums(!is.na(x)),               # will count number of non-NA values (per column)
    n.NA=colSums(is.na(x)),             # will count number of NA values (per column)
    lastLi= x[nrow(x),], last= x[nrow(x),], firstLi= x[1,], first=x[1,])
  if(length(dim(out)) <1) out <- if(is.matrix(x)) matrix(out, ncol=ncol(x), dimnames=list(NULL, colnames(x))) else {if(length(numCol)==1) as.data.frame(out) else t(as.data.frame(out))} 
  if(debug) {message(fxNa,"sCc2"); sCc2 <- list(x=x,out=out,me=me,numCol=numCol,txtCol=txtCol,nEq=nEq,silent=silent,debug=debug)}
    
  if(any(c("median","mean") %in% me) && length(txtCol) >0) { #out2 <- apply(as.matrix(x[,txtCol]), 2, function(y) y[which(y==sort(y)[ceiling(length(y)/2)]]))
    out2 <- apply(as.data.frame(x)[,txtCol], 2, function(y) sort(y)[ceiling(length(y)/2)]) 
    out0 <- x[1:nrow(out),]
    if(is.matrix(x) && length(dim(out0)) <1) out0 <- matrix(out0, nrow=nrow(out), dimnames=list(rownames(out), colnames(x)[numCol]))
    out0[,numCol] <- out
    if(is.matrix(x) && length(dim(out0)) <1) out0 <- matrix(out0, nrow=nrow(out), dimnames=list(rownames(out), colnames(x)[numCol]))
    out0[,txtCol] <- out2
    out <- out0; rm(out0,out2)   
  }  
  if(isTRUE(nEq) && grepl("Ref$", me)) { 
    if(!grepl("Ref$", me)) me <- "other"
    out <- cbind(out, nEqu=switch(me, other=NA,
      maxOfRef=sum(x[,ncol(x)]==max(x[,ncol(x)])), 
      minOfRef=sum(x[,ncol(x)]==min(x[,ncol(x)])),
      minAbsOfRef=sum(x[,ncol(x)]==min(abs(x[,ncol(x)]))),
      maxAbsOfRef=sum(x[,ncol(x)]==max(abs(x[,ncol(x)]))) ) ) }
  out }  
