#' Check order of multiple groups including non-overlapping SEM-margins
#'
#' \code{checkGrpOrderSEM} tests each line of 'x' if expected order of (replicate-) groups (defined in 'grp') appears intact,
#'  while inluding SEM of groups (replicates) via a proportional weight 'sdFact' as (avGr1-gr1SEM) < (avGr1+gr1SEM) < (avGr2-gr2SEM) < (avGr2+gr2SEM).  
#' Used for comparing groups of measures with expected profile (by matching expected order)
#' to check if data in 'x' represting groups ('grp') as lines follow.  
#' Groups of size=1: The sd (and SEM) can't be estimated directly without any replicates, however, an estimate can be given by shrinking if 'shrink1sampSd'=TRUE 
#' under the hypothesis that the overall mechanisms determining the variances is constant across all samples.
#' @param x matrix or data.frame
#' @param grp (factor) to organize replicate columns of (x)
#' @param sdFact (numeric) is proportional factor how many units of SEM will be used for defining lower & upper bounds of each group
#' @param revRank (logical) optionally revert ranks
#' @param shrink1sampSd (logical)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return logical vector if order correct (as expected based on ranks)
#' @seealso takes only 10% more time than \code{\link[wrMisc]{checkGrpOrder}} wo considering intra-group sd 
#' @examples
#' mat1 <- matrix(rep(11:24,3)[1:40],byrow=TRUE,ncol=8)
#' checkGrpOrderSEM(mat1,grp=gl(3,3)[-1])
#' @export
checkGrpOrderSEM <- function(x,grp,sdFact=1,revRank=TRUE,shrink1sampSd=TRUE,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="checkGrpOrderSEM")
  if(length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(length(grp) != ncol(x)) stop(" 'grp' should be of length of number of cols in 'x'")
  if(length(grp) <1 | sum(is.na(grp)) == length(grp)) stop(" 'grp' appears to be empty or all NAs")
  if(!is.factor(grp)) grp <- as.factor(grp)
  ## main
  avs <- .rowGrpMeans(x,grp)
  sds <- .rowGrpSds(x,grp)
  sdsNaCol <- colSums(is.na(sds)) ==nrow(x)
  if(shrink1sampSd & sum(sdsNaCol) >0) {
    if(!silent) message(fxNa,sum(sdsNaCol)," single-column groups : estimating possible sd based on other samples/groups")
    sds[,which(sdsNaCol)] <- stats::median(naOmit(as.numeric(sds)),na.rm=TRUE) }
  nMa <- matrix(rep(naOmit(as.numeric(table(grp))[order(unique(grp))]),each=nrow(x)),nrow=nrow(x))
  lims <- list(low=avs - sdFact*sds, hi=avs + sdFact*sds)
  out <- matrix(nrow=nrow(x),ncol=2*ncol(avs))
  for(i in 1:ncol(avs)) out[,(2*i-1):(2*i)] <- cbind(lims[[1]][,i],lims[[2]][,i])
  checkGrpOrder(out,rankExp=1:ncol(out),revRank=revRank) }
   
