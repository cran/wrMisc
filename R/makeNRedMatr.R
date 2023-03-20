#' Make non-redundant matrix
#'
#' \code{makeNRedMatr} takes matrix or data.frame 'dat' to summarize redundant lines (column argument \code{iniID}) along method specified in \code{summarizeRedAs} 
#' to treat all lines with redundant \code{iniID} by same approach (ie for all columns the line where specified column is at eg max = 'maxOfRef' ).
#' If no name given, the function will take the last numeric (factors may be used - they will be read as levels).
#'
#' @param dat (matrix or data.frame) main input for making non-redundant
#' @param summarizeRedAs (character) summarization method(s), typical choices 'median','mean','min' or 'maxOfRef','maxAbsOfRef' for summarizing according to 1 specified column, may be single method for all or different method for each column (besides col 'iniID') or special method looking at column (if found, first of special methods used, everything else not considered). 
#' @param iniID (character) column-name used as initial ID (default="iniID")
#' @param retDataFrame (logical) if TRUE, check if text-columns may be converted to data.frame with numeric
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @param silent (logical) suppress messages
#' @param debug (logical) for bug-tracking: more/enhanced messages
#' @return This function returns a (numeric) matrix or data.frame with summarized data and add'l col with number of initial redundant lines
#' @seealso simple/partial functionality in \code{\link{summarizeCols}},  \code{\link{checkSimValueInSer}}
#' @examples
#' t3 <- data.frame(ref=rep(11:15,3),tx=letters[1:15],
#'   matrix(round(runif(30,-3,2),1),nc=2),stringsAsFactors=FALSE)
#' by(t3,t3[,1],function(x) x)
#' t(sapply(by(t3,t3[,1],function(x) x), summarizeCols, me="maxAbsOfRef"))
#' (xt3 <- makeNRedMatr(t3, summ="mean", iniID="ref"))
#' (xt3 <- makeNRedMatr(t3, summ=unlist(list(X1="maxAbsOfRef")), iniID="ref"))
#' @export
makeNRedMatr <- function(dat, summarizeRedAs, iniID="iniID", retDataFrame=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="makeNRedMatr")
  maxLaArg <- c("maxOfRef","minOfRef","maxAbsOfRef")    
  summOpt <- c("median","mean","min","max","first","last",maxLaArg)   
  summOpt <- c(summOpt,paste(summOpt,"Complete",sep=""))
  chSuMeth <- summarizeRedAs %in% summOpt
  txt <- " argument 'dat' must be matrix or data.frame with >1 line"
  if(debug) silent <- FALSE       
  if(length(dim(dat)) <1) stop(txt) else if(nrow(dat) <2) return(dat)
  if(all(!chSuMeth)) stop(fxNa,"Value(s) ",pasteC(utils::head(chSuMeth),quoteC="'")," in argument 'summarizeRedAs' not valid")  
  if(any(!chSuMeth) && !silent) message(fxNa,"TROUBLE ahead ? Some methods specified in 'summarizeRedAs' seem not valid")  
  iniColNa <- colnames(dat)
  txt <- "'iniID' must be single column name of 'dat' !"
  if(length(iniID) !=1) stop(fxNa,txt) else if(is.na(iniID)) stop(fxNa,txt)
  iniIDcol <- colnames(dat) ==iniID
  if(sum(iniIDcol) <1) stop(fxNa,txt,"  Cannot identify column '",iniID,"' in input !")
  ## check/correct for NAs in refID ?(ID for grouping) ??
  sumRedMode <- rep("",sum(!iniIDcol))                                        # need to know which col is numeric when treating each col individually
  for(i in which(!iniIDcol)) sumRedMode[i] <- mode(dat[,i])  
  if(sum(summarizeRedAs %in% maxLaArg,na.rm=TRUE) >0) {                          # has special methods (limit to those)
    if(!silent && length(summarizeRedAs) >1) message(fxNa,"Canot use all ",length(summarizeRedAs),
      " methods specified in 'summarizeRedAs', only 1 method can be applied, using 1st of special methods") 
    summarizeRedAs <- summarizeRedAs[which(summarizeRedAs %in% maxLaArg)]
    if(sum(names(summarizeRedAs) %in% colnames(dat),na.rm=TRUE) >0) {            # (col) names identified
      summarizeRedAs <- summarizeRedAs[which(names(summarizeRedAs) %in% colnames(dat))[1]]
    } else {
      summarizeRedAs <- summarizeRedAs[1]
      lastNum <- sumRedMode %in% c("numeric","integer")
      lastNum <- which(lastNum)[sum(lastNum,na.rm=TRUE)]      
      if(!silent) message(fxNa,"Which column to use with '",summarizeRedAs,"' not specified, using last numeric '",colnames(dat)[lastNum],"'")
      names(summarizeRedAs) <- colnames(dat)[lastNum] }
  } else if(length(summarizeRedAs) < ncol(dat)) {         
    if(length(summarizeRedAs) >1 && !silent) message(fxNa,"Too few (",length(summarizeRedAs),") methods specified, recycling methods")
    summarizeRedAs <- rep(summarizeRedAs,ncol(dat))[1:(ncol(dat))]
  }
  if(any(summarizeRedAs %in% maxLaArg) && sum(names(summarizeRedAs) %in% colnames(dat)>0,na.rm=TRUE)) {
    tm2 <- which.min(!colnames(dat) %in% names(summarizeRedAs))                                     # which col of data as key
    summarizeRedAs <- summarizeRedAs[which.min(!names(summarizeRedAs) %in% colnames(dat)[tm2])]     # need short 'summarizeRedAs' with 'maxLast'(or simil)  
  } else { if(length(summarizeRedAs) < sum(!iniIDcol)) {
    if(!silent) message(fxNa,"Argument 'summarizeRedAs' has to few elements, extening by ",sum(!iniIDcol) -length(summarizeRedAs)," elements")
    summarizeRedAs <- rep(summarizeRedAs,ceiling(sum(!iniIDcol)/length(summarizeRedAs)))[1:sum(!iniIDcol)]
    names(summarizeRedAs) <- colnames(dat)[which(!iniIDcol)] }}    
  if(debug) {message(fxNa," xxmakeNRedMatr1")}
  refID <- dat[,which(iniIDcol)]
  out <- matrix(NA,nrow=length(unique(refID)),ncol=ncol(dat),dimnames=list(unique(refID),colnames(dat)))       # initalize (for all summariz w/o special meth)
  if(any(summarizeRedAs %in% maxLaArg)) {                       ##  summarize all cols together (based on last col)      
    if(length(names(summarizeRedAs)) <1) {
      lastNum <- sumRedMode %in% c("numeric","integer")
      lastNum <- which(lastNum)[sum(lastNum,na.rm=TRUE)]
      if(!silent) message(fxNa,"Which column to use with '",summarizeRedAs,"' not specified, using last numeric '",colnames(dat)[lastNum],"'")
      names(summarizeRedAs) <- colnames(dat)[lastNum] }
    sumRefCo <- colnames(dat)==names(summarizeRedAs)
    sumRef <- dat[,which(sumRefCo)[1]]
    tmp <- cbind(dat,ref=sumRef)   
    tmp <- t(sapply(by(tmp, dat[,which(iniIDcol)],function(y) y), summarizeCols,meth=summarizeRedAs)) 
    out <- as.matrix(as.data.frame(tmp))[,-ncol(tmp)]
    colnames(out) <- colnames(dat) 
  } else { if(length(unique(summarizeRedAs)) >1) { for(i in which(!iniIDcol)) {              # various summarization methods defined for each col ...
      ## key for grouping : iniIDcol    key for summarizing : which(sumRefCo) 
      tmp <- tapply(dat[,i], refID, function(x) if(is.numeric(x) & !all(is.na(x))) .summarizeCols(x,me=summarizeRedAs[i]) else .sortMid(x))
      out[,i] <- tmp }
    if(!silent) message(fxNa,"Various summarization methods appear, run col by col")      
    } else {                                                     # all summerizatio by same mode, run batch for numeric & batch for character
      if(!silent) message(fxNa,"Common summarization method '",unique(summarizeRedAs),"', run as batch")      
      if(any(sumRedMode != "numeric")) {
        i <- which(sumRedMode != "numeric")   # & colnames(dat) !=iniID
        out[,i] <- tmp <- t(sapply(by(dat[,i],refID,function(y) y), function(x) apply(x,2,.sortMid))) }
      if(any(sumRedMode == "numeric")) {
        i <- which(sumRedMode == "numeric")   # & colnames(dat) !=iniID
        out[,i] <- tmp <- t(sapply(by(dat[,i],refID,function(y) y), function(x) .summarizeCols(x,me=summarizeRedAs[i][1]))) }
  } }
  if(!silent) message(fxNa,"Summarize redundant based on col '",iniID,"'  using method(s) : ",pasteC(summarizeRedAs,quoteC="'"),
    if(any(summarizeRedAs %in% maxLaArg)) c(" and col '",names(summarizeRedAs)[1],"'")," yielding ",ncol(out)," cols")
  if(debug) {message(fxNa," xxGetRe3")}
  if(length(dim(tmp)) >1) { if(length(rownames(tmp)) >0) rownames(out) <- rownames(tmp)
    } else if(length(names(tmp)) >0) rownames(out) <- names(tmp) 
  out <- cbind(out, tapply(dat[,1], dat[,which(iniIDcol)], length))
  colnames(out)[ncol(out)] <- if("nRedLi" %in% colnames(out)) paste(colnames(out)[rev(grep("^nRedLi",colnames(out)))[1]],"X",sep=".") else "nRedLi"
  if(retDataFrame && "character" %in% mode(out)) out <- convMatr2df(out, silent=silent)
  out }

#' Choose most frequent or middle of sorted vector
#'
#' This function chooses the (first) most frequent or  middle of sorted vector
#'
#' @param x (numeric) main input 
#' @param retVal (logical) return value of most frequent, if \code{FALSE} return index of (1st) 'x' for most frequent
#' @return This function returns a numeric verctor
#' @seealso simple/partial functionality in \code{\link{summarizeCols}},  \code{\link{checkSimValueInSer}}
#' @examples
#' .sortMid(11:14)
#' @export
.sortMid <- function(x, retVal=TRUE) {
  ##  choose (1st) most frequent or (if all equal) middle of sorted vector 
  ## 'x' .. character vector; return single 'representative' element
  ## 'retVal'.. return value of most frequent, if FALSE return index of (1st) 'x' for most frequent
  ## note : in  case of equally frequent only one is chose, no warning
  x <- naOmit(x); if(length(x) >0) {y <- table(x)
  out <- if(length(unique(y)) <2) names(y)[which.max(y)] else sort(x)[ceiling(length(x)/2)]} else NA
  if(!retVal) out <- which(x==out[1])[1]
  out }
    
