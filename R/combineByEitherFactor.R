#' Create factor-like column regrouping data regrouping simultaneaously by two factors
#'
#' This function aims to address the situation when two somehow different groupins (of the same data) exist and need to be joined.
#' It is not necessary that both alternative groupings use the same labels, neither.
#' \code{combineByEitherFactor} adds new (last) column named 'grp' to input matrix representing the combined factor 
#' relative to 2 specified columns from input matrix  \code{mat} (via 'refC1','refC2'). Optionally, the output may be 
#' sorted and a column giving n per factor-level may be added. 
#' The function treats selected columns of  \code{mat} 
#' as pairwise combination of 2 elements (that may occur multiple times over all lines of  \code{mat})
#' and sorts/organizes all instances of such combined elements (ie from both selected columns) as repeats of a given group,
#' who's class number is given in output column 'grp', the (total) number of repeats may be displayed in column 'nGrp' ( \code{nByGrp=TRUE}).
#' If groups are overlapping (after re-ordering), an iterative process of max 3x2 passes will be launched after initial matching.
#' Works on numeric as well as character input.  
#'
#' @param mat main input matrix
#' @param refC1 (integer) column-number of 'mat' to use as 1st set
#' @param refC2 (integer) column-number of 'mat' to use as 2nd set
#' @param nByGrp (logical) add last col with n by group
#' @param convergeMax (logical) if \code{TRUE}, run 2 add'l iteartive steps to search convergence to stable result
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns a matrix containing both selected columns plus additional column(s) indicating group-number of the pair-wise combination 
#' (and optional the total n by group)
#' @examples
#' nn <- rep(c("a","e","b","c","d","g","f"),c(3,1,2,2,1,2,1))
#' qq <- rep(c("m","n","p","o","q"),c(2,1,1,4,4))
#' nq <- cbind(nn,qq)[c(4,2,9,11,6,10,7,3,5,1,12,8),]
#' combineByEitherFactor(nq,1,2,nBy=TRUE); combineByEitherFactor(nq,1,2,nBy=FALSE)
#' combineByEitherFactor(nq,1,2,conv=FALSE); combineByEitherFactor(nq,1,2,conv=TRUE)
#' ##
#' mm <- rep(c("a","b","c","d","e"),c(3,4,2,3,1)); pp <- rep(c("m","n","o","p","q"),c(2,2,2,2,5))
#' combineByEitherFactor(cbind(mm,pp), 1, 2, con=FALSE, nBy=TRUE)
#' combineByEitherFactor(cbind(mm,pp), 1, 2, con=TRUE, nBy=TRUE) 
#' @export
combineByEitherFactor <- function(mat, refC1, refC2, nByGrp=FALSE, convergeMax=TRUE, callFrom=NULL, debug=FALSE, silent=FALSE) {
  fxNa <- .composeCallName(callFrom,newNa="combineByEitherFactor")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(mat) <1) stop(fxNa," nothing found in 'mat'")
  if(length(dim(mat)) !=2) stop("'mat' must be matrix with min 2 columns")
  msg <- "Arguments 'refC1' & 'refC1' should be of length=1 (truncating)"
  if(length(refC1) >1) {if(!silent) message(fxNa,msg); refC1 <- refC1[1]}
  if(length(refC2) >1) {if(!silent) message(fxNa,msg); refC2 <- refC1[2]}
  if(nrow(mat) <2) return(cbind(mat, grp=1, nGrp=if(nByGrp) 1 else NULL))
  mat <- cbind(mat, iniOrder=1:nrow(mat))              # add col initial order
  mat <- mat[order(mat[,refC1], mat[,refC2]),]         # reorder by 1st fact & 2nd fact
  ## express 1st index as 2nd :
  mat <- cbind(mat, grp=unlist(tapply(mat[,refC1],mat[,refC2], function(x) rep(x[1],length(x))))[rank(mat[,refC2],ties.method="first")]) 
  sup1 <- unlist(tapply(mat[,ncol(mat)],mat[,refC1], function(x) rep(x[1],length(x))))[order(mat[,refC1])]
  msg <- c(" did not reach convergence at ","2nd","4th"," pass, continuing ..","6th pass (stop iterating)"," pass")
  if(convergeMax & !all(sup1==mat[,ncol(mat)])) {
    if(!silent && nrow(mat) >999) message(fxNa,msg[c(1,2,4)])
    sup2 <- unlist(tapply(sup1,mat[,refC2], function(x) rep(x[1],length(x))))[order(mat[,refC2])]  
    sup3 <- unlist(tapply(sup2,mat[,refC1], function(x) rep(x[1],length(x))))[order(mat[,refC1])]
    if(!all(sup3==sup2) & !silent) { message(fxNa,msg[c(1,2,4)])
      sup2 <- unlist(tapply(sup3,mat[,refC2], function(x) rep(x[1],length(x))))[order(mat[,refC2])]
      sup3 <- unlist(tapply(sup2,mat[,refC1], function(x) rep(x[1],length(x))))[order(mat[,refC1])]
      if(!all(sup3==sup2) && !silent) message(fxNa,msg[c(1,5)]) }
    mat[,ncol(mat)] <- sup3 } else if(!silent && !convergeMax) message(fxNa,msg[c(1,2,6)])
  mat[,ncol(mat)] <- as.numeric(as.factor(mat[,ncol(mat)]))                   # reset grp names
  if(nByGrp) { mTa <- table(mat[,ncol(mat)])
    mat <- cbind(mat,nGrp=mTa[match(mat[,ncol(mat)],names(mTa))]) }
  mat[as.numeric(mat[,ncol(mat) -1 -nByGrp]), -ncol(mat) +1 +nByGrp] }
   
