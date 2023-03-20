#' Check for strict (ascencing or descending) order
#'
#' \code{checkStrictOrder} tests lines of 'dat' (matrix of data.frame) for strict order (ascending, descending or constant), 
#' each col of data is tested relative to the col on its left.
#' @param dat matrix or data.frame
#' @param invertCount (logical) inverse counting (ie return 0 for all elememts in order)
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return matrix with counts of up pairs, down pairs, equal-pairs, if 'invertCount'=TRUE all non-events are counted, ie a resulting 0 means that all columns are following the described characteristics (with variabale col-numbers easier to count)
#' @seealso \code{\link[base]{order}}, \code{\link{checkGrpOrder}}   
#' @examples
#' set.seed(2005); mat1 <- rbind(matrix(round(runif(40),1),nc=4), rep(1,4))
#' checkStrictOrder(mat1); mat1[which(checkStrictOrder(mat1)[,2]==0),]
#' @export
checkStrictOrder <- function(dat, invertCount=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="checkStrictOrder")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  testO <- array(NA, dim=c(nrow(dat), ncol(dat) -1, 2))
  testId <- matrix(NA, nrow=nrow(dat), ncol=ncol(dat) -1)
  for(i in 1:(ncol(dat)-1)) {
     testO[,i,1] <- dat[,i] > dat[,i+1]
     testO[,i,2] <- dat[,i] < dat[,i+1]
     testId[,i] <- dat[,i] == dat[,i+1]
  }
  if(isTRUE(invertCount)) {testO <- !testO; testId <- !testId}
  out <- cbind(up=rowSums(testO[,,2], na.rm=TRUE), down=rowSums(testO[,,1], na.rm=TRUE), eq=rowSums(testId, na.rm=TRUE))
  rownames(out) <- rownames(dat)
  out }
  

#' Get first minimum
#'
#' This function allows to find the first minimum of a numeric vector
#' 
#' @param x (numeric vector) main input
#' @param positionOnly (logical)
#' @return numeric vector
#' @seealso  \code{\link[base]{which.min}}
#' @examples
#' .firstMin(c(4,3:6))
#' @export
.firstMin <- function(x, positionOnly=FALSE) {
  ## get (first) min of series
  ## for longer series of data rather use getMedOf1stValley()
  minPos <- which.min(x)                # no problem with NA
  if(positionOnly) minPos else x[minPos] }


#' Rescale respective to specific group
#'
#' This function allows to rescale data 'x' so that specific group 'grpNum' gets normalized to predefined value 'grpVal'.
#' In normal case x will be multiplied by 'grpVal' and devided by value obtained from 'grpNum'.
#' If summary of 'grpNum-positions' or 'grpVal' is 0, then grpVal will be attained by subtraction of summary & adding grpVal
#' 
#' @param x (numeric vector) main input
#' @param grpNum (numeric)
#' @param grpVal (numeric)
#' @param sumMeth (character) method for summarizing
#' @param callFrom (character) allow easier tracking of messages produced
#' @return numeric vector
#' @seealso  \code{\link[base]{which.min}}
#' @examples
#' .firstMin(c(4,3:6))
#' @export
.medianSpecGrp <- function(x, grpNum, grpVal, sumMeth="median", callFrom=NULL){
  ## rescale data 'x' so that specific group 'grpNum' gets normalized to predefined value 'grpVal'
  ## in normal case x will be multiplied by 'grpVal' and devided by value obtained from 'grpNum'
  ## if summary of 'grpNum-positions' or 'grpVal' is 0, then grpVal will be attained by subtration of summary & adding grpVal
  fxNa <- .composeCallName(callFrom,newNa=".medianSpecGrp")
  msg1 <- c("argument ","'grpVal' should be numeric; ",
     "'grpNum' should be of length 1 and may be numeric or names of 'x'")
  if(sum(is.na(x))==length(x)) {
    message(fxNa,"argument 'x' seems all empty or NA, nothing to do")
  } else {
    if(length(grpVal) != 1) stop(fxNa,msg1[c(1:2)])
    if(any(sum(is.na(x)) ==length(x), sum(is.na(grpNum)) >0, is.na(grpVal))) stop(msg1[c(1:2,1,3)])
    if(length(grep("^[[:digit:]]+$",grpNum)) <1)  grpNum <- match(grpNum,names(x))
    grpNum <- convToNum(grpNum, remove=NULL)
    grpNum <- grpNum[grpNum <= length(x)]
    msg2 <- "'grpNum' should be either numeric for positions in 'x' or character for names of 'x'"
    if(length(grpNum) <1) stop(fxNa,msg1[1],msg2)
    grp1ini <- if(length(grpNum) >1) {
      if(identical(sumMeth,"median")) stats::median(x[grpNum],na.rm=TRUE) else mean(x[grpNum],na.rm=TRUE)
      } else x[grpNum]
    x <- if(all(c(grpVal,grp1ini) !=0)) grpVal*x/grp1ini else x + grpVal - grp1ini             # set values
  }
  x }

#' Rescale respective to specific group
#'
#' This function allows to rescale data 'x' so that 2 specific groups get normalized to predefined values (and all other values follow proportionally)
#' 'grp1Num' and 'grp2Num' should be either numeric for positions in 'x' or character for names of 'x';
#' if 'grp1Num' and/or 'grp2Num' design mulitple locations: perform median or mean summarization, according to 'sumMeth'
#' 
#' @param x (numeric vector) main input
#' @param grp1Num (numeric)
#' @param grp1Val (numeric)
#' @param grp2Num (numeric)
#' @param grp2Val (numeric)
#' @param sumMeth (character) method for summarizing
#' @param callFrom (character) allow easier tracking of messages produced
#' @return numeric vector
#' @seealso  \code{\link[base]{which.min}}
#' @examples
#' .firstMin(c(4,3:6))
#' @export
.scaleSpecGrp <- function(x, grp1Num, grp1Val, grp2Num=NULL, grp2Val=NULL, sumMeth="mean",callFrom=NULL){
  ## rescale data 'x' so that 2 specific groups get normalized to predefined values (and all other values follow proportionally)
  ##  'grp1Num' and 'grp2Num' should be either numeric for positions in 'x' or character for names of 'x'
  ##   if 'grp1Num' and/or 'grp2Num' design mulitple locations: perform median or mean summarization, according to 'sumMeth'
  ## return object of same dim as 'x'
  fxNa <- .composeCallName(callFrom, newNa=".scaleSpecGrp")
  msg1 <- c("argument ","'grp1Val'"," and ","'grp2Val'"," should be numeric of length 1; ",
     "'grp1Num'","'grp2Num'"," may be numeric or names of 'x'"," .. ignoring")
  if(sum(is.na(x)) ==length(x)) {
    message(fxNa,"Argument 'x' seems all empty or NA, nothing to do")
  } else {
    if(sum(is.na(x))==length(x) || sum(is.na(grp1Num)) >0 || is.na(grp1Val)) stop(msg1[c(1:2,5,6,8)])
    if(length(grp1Val) !=1) stop(msg1[c(1:2,5)])
    if(length(grep("^[[:digit:]]+$", grp1Num)) <1) grp1Num <- match(grp1Num, names(x))
    grp1Num <- convToNum(grp1Num, remove=NULL)
    grp1Num <- grp1Num[grp1Num <= length(x)]
    if(length(grp1Num) <1) stop(fxNa,"Can't find positions/matches for 'grp1Num' in 'x' !")
    grp1ini <- if(length(grp1Num) >1) {
      if(identical(sumMeth,"median")) stats::median(x[grp1Num],na.rm=TRUE) else mean(x[grp1Num],na.rm=TRUE)
      } else x[grp1Num]
    if(identical(grp1Val, grp2Val)) {grp2Num <- NULL; message(fxNa," grp1Val and grp2Val should be different !  ignoring grp2Val")}
    if(length(grp2Num) >0 && length(grp2Val) !=1) {grp2Num <- NULL; message(fxNa,paste(msg1[c(1,4,5,9)]))}
    if(length(grp2Num) >0) {
      if(any(sum(is.na(grp2Num)) >0, is.na(grp2Val))) stop(msg1[c(1:2,1,3)])
      grp2ini <- if(length(grp2Num) >1) {
        if(identical(sumMeth,"median")) stats::median(x[grp2Num],na.rm=TRUE) else mean(x[grp2Num], na.rm=TRUE)
        } else x[grp2Num]
      if(length(grep("^[[:digit:]]+$",grp2Num)) <1)  grp2Num <- match(grp2Num, names(x))
      grp2Num <- convToNum(grp2Num, remove=NULL)
      grp2Num <- grp2Num[grp2Num <= length(x)]
      msg2 <-  c("'grp2Num' should be either numeric for positions in 'x' or character for names of 'x'"," ... ignoring")
      if(length(grp2Num) <1) {grp2Num <- NULL; message(fxNa,msg1[1],msg2)} }
    if(length(grp2Num) <1) {
      x <- if(all(c(grp1Val, grp1ini) !=0)) grp1Val*x/grp1ini else x + grp1Val - grp1ini             # set values
    } else {
      x <- x - grp1ini              # set 1st val to 0
      x <- grp1Val + x*(grp2Val -grp1Val)/grp2ini } }
  x }
  
