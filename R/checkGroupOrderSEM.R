#' Check Order Of Multiple Groups Including Non-Overlapping SEM-Margins
#'
#' This function tests each line of 'x' if expected order of (replicate-) groups (defined in 'grp') appears intact,
#'  while inluding SEM of groups (replicates) via a proportional weight 'sdFact' as (avGr1-gr1SEM) < (avGr1+gr1SEM) < (avGr2-gr2SEM) < (avGr2+gr2SEM).  
#' 
#' @details
#' This function may be used for comparing groups of measures with expected profiles (by matching expected order)
#' to check if data in 'x' represt ascending groups ('grp') .  
#' Groups of size=1: The sd (and SEM) can't be estimated directly without any replicates, however, an estimate can be given by shrinking if 'shrink1sampSd'=TRUE 
#' under the hypothesis that the overall mechanisms determining the variances is constant across all samples.
#' 
#' @param x matrix or data.frame
#' @param grp (factor) to organize replicate columns of (x)
#' @param sdFact (numeric) is proportional factor how many times the SD will be used for defining lower & upper bounds of each group (ie \code{sdFact=2} will mimick 95-percent confidence intervals)
#' @param revRank (logical) (not implemented yet : optionally revert rank)
#' @param shrink1sampSd (logical)
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list with $rankExp as logical vector telling if group-ranges are asending and $lims wityh the sd-based group-limits
#' @seealso takes only 10% more time than \code{\link[{checkGrpOrder}} without considering intra-group sd 
#' @examples
#' mat1 <- matrix(rep(11:24,3)[1:40], byrow=TRUE, ncol=8)
#' dimnames(mat1) <- list(letters[1:nrow(mat1)], paste(rep(LETTERS[3:1], each=3)[-1], 
#'   rep(1:3, 3)[-1], sep="."))
#' checkGrpOrderSEM(mat1, grp=gl(3,3, labels=LETTERS[3:1])[-1])
#' @export
checkGrpOrderSEM <- function(x, grp, sdFact=1, revRank=TRUE, shrink1sampSd=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="checkGrpOrderSEM")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(dim(x)) !=2) stop(fxNa," 'x' should be data.frame or matrix of 2 dimensions")
  if(length(grp) != ncol(x)) stop(fxNa," 'grp' should be of length of number of cols in 'x'")
  if(length(grp) <1 | sum(is.na(grp)) == length(grp)) stop(fxNa," 'grp' appears to be empty or all NAs")
  if(!is.factor(grp)) grp <- as.factor(grp)
  
  ## main
  avs <- .rowGrpMeans(x, grp)
  sds <- .rowGrpSds(x, grp)
  sdsNaCol <- colSums(is.na(sds)) ==nrow(x)
  if(isTRUE(shrink1sampSd) && sum(sdsNaCol) >0) {
    if(!silent) message(fxNa, sum(sdsNaCol)," single-column groups : estimating possible sd based on other samples/groups")
    sds[,which(sdsNaCol)] <- stats::median(naOmit(as.numeric(sds)), na.rm=TRUE) }
  nMa <- matrix(rep(naOmit(as.numeric(table(grp))[order(unique(grp))]), each=nrow(x)), nrow=nrow(x))
  lims <- list(low=avs - sdFact*sds, hi=avs + sdFact*sds)
  if(debug) {message(fxNa,"chGO1")}
  if(length(lims)==2) {
    out <- matrix(nrow=nrow(x), ncol=2*ncol(avs))
    for(i in 1:ncol(avs)) out[,(2*i-1):(2*i)] <- cbind(lims[[1]][,i], lims[[2]][,i])
    colnames(out) <- paste0(rep(naOmit(unique(grp)), each=2), ".", c("low","hi"),"Lim")
    list(rankExp=rowSums( out[, 2*(1:(ncol(avs) -1)) ] >= out[, 2*(2:ncol(avs)) -1  ]) < 1, lims= out) }}
    
    
 