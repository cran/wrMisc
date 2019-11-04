#' Check how multiple groups of data separate or overlap based on mean +/- sd
#'
#' \code{checkAvSd} compares if/how neighbour groups separate/overlap via the 'engineering approach' (+/- 2 standard-deviations is similar to a=0.05 \code{t.test}).
#' This approach may be used as less elegant alternative to (multi-group) logistic regression.
#' The function uses 'daAv' as matrix of means (rows are tested for up/down character/progression) which get compared with boundaries taken from daSd (for Sd values of each mean in 'daAv').
#' @param daAv matrix or data.frame
#' @param daSd matrix or data.frame
#' @param nByGr optinal specifying number of Elements per group, allows rather using SEM (adopt to variable n of different groups)
#' @param multSd (numeric) the factor specifyin how many sd values should be used as margin
#' @param codeConst (character) which term/word to use when specifying 'constant'
#' @param extSearch (logical) if TRUE, extend search to one group further (will call result 'nearUp' or 'nearDw')
#' @param outAsLogical to switch between 2col-output (separate col for 'up' and 'down') or simple categorical vector ('const','okDw','okUp')
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return vector describing character as 'const' or 'okUp','okDw' (or if extSearch=TRUE 'nearUp','nearDw')
#' @seealso \code{\link[wrMisc]{rowGrpMeans}}
#' @examples
#' mat1 <- matrix(rep(11:24,3)[1:40],byrow=TRUE,ncol=8)
#' checkGrpOrderSEM(mat1,grp=gl(3,3)[-1])
#' checkAvSd(rowGrpMeans(mat1,gl(3,3)[-1]),rowGrpSds(mat1,gl(3,3)[-1]) )
#' # consider variable n :
#' checkAvSd(rowGrpMeans(mat1,gl(3,3)[-1]),rowGrpSds(mat1,gl(3,3)[-1]),nByGr=c(2,3,3)) 
#' @export
checkAvSd <- function(daAv,daSd,nByGr=NULL,multSd=2,codeConst="const",extSearch=FALSE,outAsLogical=TRUE,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="checkAvSd")
  if(!identical(dim(daAv),dim(daSd))) stop(fxNa," dimensions of 'daAv' and 'daSd' not same !!")
  if(is.null(nByGr)) nByGr <- rep(1,ncol(daAv))
  nGr <- ncol(daAv)
  if(length(nByGr) != nGr & !silent) {
    message(fxNa," 'nByGr' doesn't match number of columns in 'daAv' !!")  }
  incr <- daAv[,-nGr]+ multSd*daSd[,-nGr]/sqrt(nByGr[-nGr]) < daAv[,-1]- multSd*daSd[,-1]/sqrt(nByGr[-1])
  decr <- daAv[,-nGr]- multSd*daSd[,-nGr]/sqrt(nByGr[-nGr]) > daAv[,-1]+ multSd*daSd[,-1]/sqrt(nByGr[-1])
  if(outAsLogical) {
    out <- cbind(up=rowSums(incr,na.rm=TRUE) >= ncol(incr), down=rowSums(decr,na.rm=TRUE) >= ncol(decr))
  } else {
    outInc <- rowSums(incr,na.rm=TRUE)
    outDec <- rowSums(decr,na.rm=TRUE)
    out <- rep(codeConst,nrow(daAv))
    out[which(outInc == nGr-1)] <- "okUp"
    out[which(outDec == nGr-1)] <- "okDw"
    if(extSearch & nGr >3){
      out[which(outInc == nGr-2)] <- "nearUp"
      out[which(outDec == nGr-2)] <- "nearDw" }
  }
  out }
   
