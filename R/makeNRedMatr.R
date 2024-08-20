#' Make non-redundant matrix
#'
#' This function takes matrix or data.frame 'dat' to summarize redundant lines (column argument \code{iniID}) along method specified in \code{summarizeRedAs} 
#' to treat all lines with redundant \code{iniID} by same approach (ie for all columns the line where specified column is at eg max = 'maxOfRef' ).
#' If no name given, the function will take the last numeric (factors may be used - they will be read as levels).
#'
#' @details
#' When using for selection of single initial line give the character-string of argument \code{summarizeRedAs} a name (eg  \code{summ=c(X1="minOfRef")}
#'  so that the function will use ONLY the column specified via the name for determining which line should be used/kept.    
#' 
#' It is possible to base the choice from 'redundant' lines on a single reference-column.  
#' For example, when \code{summarizeRedAs='maxOfRef'} summarizing of all (numeric) columns will be performed according to one single column (ie the line where the last numeric column is at its max).
#' Otherwiser, a name can be assigned as reference column to be used (eg see last example using \code{summarizeRedAs=c(x1='maxOfRef')})
#' 
#' @param dat (matrix or data.frame) main input for making non-redundant
#' @param summarizeRedAs (character) summarization method(s), typical choices 'median','mean','min' or 'maxOfRef';
#'    basic usage like \code{summarizeRedAs='mean'} will pick independently the mean for each (numeric) column; 
#'    it is also possible to specify different methods for each of columnw (length of \code{summarizeRedAs} should be equal number of numeric columns);
#'    special methods look at a single reference column to decide which line should be picked and their values reported (not compatible with specifying different methods for different columns), 
#' @param iniID (character) column-name used as reference for determining groups of redundant lines (default="iniID")
#' @param retDataFrame (logical) if \code{TRUE}, check if text-columns may be converted to data.frame with numeric
#' @param nEqu (logical) if \code{TRUE}, add additional column indicating the number of equal lines for choice (only with min or max) 
#' @param callFrom (character) allows easier tracking of messages produced
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @return This function returns a (numeric) matrix or data.frame with summarized data and add'l col with number of initial redundant lines
#' @seealso simple/partial functionality in \code{\link{summarizeCols}},  \code{\link{checkSimValueInSer}}
#' @examples
#' t3 <- data.frame(ref=rep(11:15,3),tx=letters[1:15],
#'   matrix(round(runif(30,-3,2),1),nc=2),stringsAsFactors=FALSE)
#' by(t3,t3[,1],function(x) x)
#' t(sapply(by(t3,t3[,1],function(x) x), summarizeCols, me="maxAbsOfRef"))
#' # calculate mean for lines concerened of all columns :
#' (xt3 <- makeNRedMatr(t3, summ="mean", iniID="ref"))
#' # choose lines based only on content of column 'X1' (here: max):
#' (xt3 <- makeNRedMatr(t3, summ=c(X1="maxOfRef"), iniID="ref")) 
#' @export
makeNRedMatr <- function(dat, summarizeRedAs, iniID="iniID", retDataFrame=TRUE, nEqu=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="makeNRedMatr")
  maxLaArg <- c("maxOfRef","minOfRef","maxAbsOfRef")    
  summOpt <- c("median","mean","min","max","first","last",maxLaArg)   
  summOpt <- c(summOpt, paste0(summOpt,"Complete"))
  chSuMeth <- summarizeRedAs %in% summOpt
  txt <- " argument 'dat' must be matrix or data.frame with >1 line"
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(dim(dat)) <1) stop(txt) else if(nrow(dat) <2) return(dat)
  if(all(!chSuMeth)) stop(fxNa,"Value(s) ",pasteC(utils::head(chSuMeth),quoteC="'")," in argument 'summarizeRedAs' not valid")  
  if(any(!chSuMeth) && !silent) message(fxNa,"TROUBLE ahead ? Some methods specified in 'summarizeRedAs' seem not valid")  
  iniColNa <- colnames(dat)
  txt <- "'iniID' must be single column name of 'dat' !"
  if(length(iniID) !=1) stop(fxNa,txt) else if(is.na(iniID)) stop(fxNa,txt)
  iniIDcol <- colnames(dat) ==iniID
  if(sum(iniIDcol) <1) stop(fxNa,txt,"  Cannot identify column '",iniID,"' in input !")
  refID <- dat[,iniID]
  refDat <- refCol <- NULL

  ## check/correct for NAs in refID ?(ID for grouping) ??
  sumRedMode <- rep("",sum(!iniIDcol))                                        # need to know which col is numeric when treating each col individually
  for(i in which(!iniIDcol)) sumRedMode[i] <- mode(dat[,i])  
  if(any(summarizeRedAs %in% maxLaArg, na.rm=TRUE)) {                          # has special methods (limit to those)
    if(!silent && length(summarizeRedAs) >1) message(fxNa,"Canot use all ",length(summarizeRedAs),
      " methods specified in 'summarizeRedAs', only 1 method can be applied, using 1st of special methods") 
    summarizeRedAs <- summarizeRedAs[which(summarizeRedAs %in% maxLaArg)]
    if(all(names(summarizeRedAs) %in% colnames(dat),na.rm=TRUE)) {            # (col) names identified
      summarizeRedAs <- summarizeRedAs[which(names(summarizeRedAs) %in% colnames(dat))[1]]
    } else {
      summarizeRedAs <- summarizeRedAs[1]
      lastNum <- sumRedMode %in% c("numeric","integer")
      lastNum <- which(lastNum)[sum(lastNum,na.rm=TRUE)]      
      if(!silent) message(fxNa,"Which column to use with '",summarizeRedAs,"' not specified, using last numeric '",colnames(dat)[lastNum],"'")
      names(summarizeRedAs) <- colnames(dat)[lastNum] }
  } else if(length(summarizeRedAs) < ncol(dat)) {         
    if(length(summarizeRedAs) >1 && !silent) message(fxNa,"Too few (",length(summarizeRedAs),") methods specified, recycling methods")
    summarizeRedAs <- rep(summarizeRedAs, ncol(dat))[1:(ncol(dat))]
  }
  if(debug) {message(fxNa,"mNR1"); mNR1 <- list(dat=dat,summarizeRedAs=summarizeRedAs,iniID=iniID,retDataFrame=retDataFrame,sumRedMode=sumRedMode,iniIDcol=iniIDcol,nEqu=nEqu)}

  out <- matrix(NA, nrow=length(unique(refID)), ncol=ncol(dat), dimnames=list(unique(refID),colnames(dat)))       # initalize (for all summariz w/o special meth)
  refID <- dat[,which(iniIDcol)]
  ## prepare argument "summarizeRedAs"
  if(any(summarizeRedAs %in% maxLaArg)) {
    if(any(names(summarizeRedAs) %in% colnames(dat), na.rm=TRUE)) {
      ## has name to use as ref
      if(length(summarizeRedAs) >1) {  if(!silent) message(fxNa,"Only a single value of argument 'summarizeRedAs' can be used when reference-approach is chosen !")
        summarizeRedAs <- summarizeRedAs[which.min(!names(summarizeRedAs) %in% colnames(dat)[refID])]}     # reduce 'summarizeRedAs' to 1st instance
    } else {
      lastNum <- sumRedMode %in% c("numeric","integer")
      lastNum <- which(lastNum)[sum(lastNum,na.rm=TRUE)]
      if(!silent) message(fxNa,"Which column to use with '",summarizeRedAs,"' not specified, using last numeric '",colnames(dat)[lastNum],"'")
      names(summarizeRedAs) <- colnames(dat)[lastNum] 
    }
    refCol <- which.min(!colnames(dat) %in% names(summarizeRedAs))       # which col of data as key
    refDat <- dat[,refCol]
    names(refCol) <- colnames(dat)[refCol] 
    if(debug) message(fxNa,"refCol = '",names(refCol),"'")                                    
  } else {
    if(length(summarizeRedAs) < sum(!iniIDcol)) {
      if(!silent) message(fxNa,"Argument 'summarizeRedAs' has to few elements, extening by ",sum(!iniIDcol) -length(summarizeRedAs)," elements")
      summarizeRedAs <- rep(summarizeRedAs, ceiling(sum(!iniIDcol)/length(summarizeRedAs)))[1:sum(!iniIDcol)]
      names(summarizeRedAs) <- colnames(dat)[which(!iniIDcol)] }    
  }

  ##  summarize all cols together (based on summarizeRedAs) 
  if(debug) {message(fxNa,"mNR2a"); mNR2a <- list(dat=dat,summarizeRedAs=summarizeRedAs,iniID=iniID,refDat=refDat,retDataFrame=retDataFrame,out=out,sumRedMode=sumRedMode,refID=refID,iniIDcol=iniIDcol,nEqu=nEqu,maxLaArg=maxLaArg)}
  if(any(summarizeRedAs %in% maxLaArg)) {                            
    tmp <- cbind(dat, ref=refDat) 
    tmp <- by(tmp, dat[,which(iniIDcol)], summarizeCols, meth=summarizeRedAs, refCol=refCol, silent=silent, debug=debug, callFrom=fxNa)
    out <- data.frame(do.call("rbind", tmp))[,1:ncol(dat)]    
    colnames(out) <- colnames(dat) 
  } else { if(length(unique(summarizeRedAs)) >1) { for(i in which(!iniIDcol)) {              # various summarization methods defined for each col ...
      ## key for grouping : iniIDcol    key for summarizing : which(colnames(dat)==names(summarizeRedAs)) 
      if(debug) {message(fxNa,"mNR2b"); mNR2b <- list()}
      tmp <- tapply(dat[,i], refID, function(x) if(is.numeric(x) && !all(is.na(x))) .summarizeCols(x,me=summarizeRedAs[i]) else .sortMid(x))
      out[,i] <- tmp }
    if(!silent) message(fxNa,"Various summarization methods appear, run col by col")      
    } else {                                                     # all summerization by same mode, run batch for numeric & batch for character
      if(!silent) message(fxNa,"Common summarization method '",unique(summarizeRedAs),"', run as batch")      
      if(any(sumRedMode != "numeric")) {
        i <- which(sumRedMode != "numeric")   # & colnames(dat) !=iniID
        out[,i] <- tmp <- t(sapply(by(dat[,i], refID,function(y) y), function(x) apply(x, 2, .sortMid))) }
      if(debug) {message(fxNa,"mNR2c"); mNR2c <- list()}
      if(any(sumRedMode == "numeric")) {
        i <- which(sumRedMode == "numeric")   # & colnames(dat) !=iniID
        out[,i] <- tmp <- t(sapply(by(dat[,i], refID, function(y) y), function(x) .summarizeCols(x, me=summarizeRedAs[i][1]))) }
  } }
  if(debug) {message(fxNa,"mNR3"); mNR3 <- list()}
  if(!silent) message(fxNa,"Summarize redundant based on col '",iniID,"'  using method(s) : ",pasteC(summarizeRedAs,quoteC="'"),
    if(any(summarizeRedAs %in% maxLaArg)) c(" and col '",names(summarizeRedAs)[1],"'")," yielding ",ncol(out)," cols")
  if(debug) {message(fxNa," mNR4")}
  if(length(dim(tmp)) >1) { if(length(rownames(tmp)) >0) rownames(out) <- rownames(tmp)
    } else if(length(names(tmp)) >0) rownames(out) <- names(tmp) 
  out <- cbind(out, tapply(dat[,1], dat[,which(iniIDcol)], length))
  colnames(out)[ncol(out)] <- if("nRedLi" %in% colnames(out)) paste(colnames(out)[rev(grep("^nRedLi",colnames(out)))[1]],"X",sep=".") else "nRedLi"
  if(retDataFrame && "character" %in% mode(out)) out <- convMatr2df(out, silent=silent)
  out }


#' Choose most frequent or middle of sorted vector
#'
#' This function chooses the (first) most frequent or  middle of sorted vector, similar to the concept of \code{mode}
#'
#' @param x (numeric) main input 
#' @param retVal (logical) return value of most frequent, if \code{FALSE} return index of (1st) 'x' for most frequent
#' @return This function returns a numeric verctor
#' @seealso simple/partial functionality in \code{\link{summarizeCols}},  \code{\link{checkSimValueInSer}}
#' @examples
#' .sortMid(11:14)
#' .sortMid(rep("b",3))
#' @export
.sortMid <- function(x, retVal=TRUE) {
  ##  choose (1st) most frequent or (if all equal) middle of sorted vector 
  ## 'x' .. character vector; return single 'representative' element
  ## 'retVal'.. return value of most frequent, if FALSE return index of (1st) 'x' for most frequent
  ## note : in  case of equally frequent only one is chose, no warning
  x <- naOmit(x); if(length(x) >0) { y <- table(x)
    out <- if(sum(duplicated(y))==length(y)-1)  sort(x)[ceiling(length(x)/2)] else names(y)[which.max(y)]} else NA 
  if(!retVal) out <- which(x==out[1])[1]
  out }
    
