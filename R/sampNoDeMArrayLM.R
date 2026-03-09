#' Locate Sample Index From Index Or Name Of Pair-Wise Comparisons In List Or MArrayLM-Object  
#'
#' When multiple series of data are tested simultaneaously (eg using \code{moderTestXgrp}), multiple pairwise comparisons get performed. 
#' This function helps locating the groups of samples, eg respective mean-columns, corresponding to specific pairwise comparisons.
#' 
#' @details  
#' As main input one gives a list or MArrayLM-object containing testing results from pairwise comparisons,
#' and specific comparisons indicated by \code{useComp} to get located in the elements of mean-columns (\code{lstMeans}).
#'
#' @param MArrayObj (list or MArray-object) main input
#' @param useComp (character or integer) index or name of pairwise-comparison to be addressed
#' @param outTy (character) choose if just vector of indexes or list with $ind (with \code{outTy="index"}), 
#'   vector of group-names (with \code{outTy="names"}) or list with $ind and $GrpNames  should get returned
#' @param groupSep (NULL or character of length=1) separator for pair of names; if \code{NULL} a suitable separator (among '-','--','_','__','.',' ' and '  ') will be used
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{moderTestXgrp}}, this function gets used eg in \code{\link[wrGraph]{MAplotW}} or \code{\link[wrGraph]{VolcanoPlotW}}
#' @return This function returns a numeric vector (length=2) with index indicating the columns of (replicate) mean-values corresponding to the comparison specified in \code{useComp} 
#' @examples
#' grp3 <- factor(rep(LETTERS[4:2], c(2,3,3)))
#' set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4), 2), ncol=8,
#'   dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste0(grp3,c(1:2,1:3,1:3))))
#' if(requireNamespace("limma", quietly=TRUE)) {  # need limma installed...
#'   test8 <- moderTestXgrp(t8, grp3) 
#'   head(test8$p.value)         # all pairwise comparisons available
#'   sampNoDeMArrayLM(test8, 1)   
#'   unique(grp3)[sampNoDeMArrayLM(test8, 1)]
#'   head(test8$means[,sampNoDeMArrayLM(test8, 1)])
#'   head(test8$means[,sampNoDeMArrayLM(test8, "C-D")]) }
#'  
#' @export
sampNoDeMArrayLM <- function(MArrayObj, useComp=NULL, outTy="index", groupSep=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## full list of groups rather in mean-column - if available (seed into indexGroupsFromPW() ) 
  ## locate sample index from index or name of pair-wise comparisons in list or MArrayLM-object
  ## no need to give grp since columns are considered exhaustive ??
  fxNa <- .composeCallName(callFrom, newNa="sampNoDeMArrayLM")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  
  errMsg <- c("Argument 'MArrayObj' is ","empty")
  if(length(MArrayObj) <1) stop(errMsg[1:2])
  if(debug) {message(fxNa,"sNDM1"); sNDM1 <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep)}
  
  ## check for complete set of groups from  respective mean-columns
  grp <- if("means" %in% names(MArrayObj)) colnames(MArrayObj$means) else {if("contrasts" %in% MArrayObj) rownames(MArrayObj$constratsts) else NULL}  # possibly remove Av. prefix ??
  if(length(grp) ==0 && debug) message(fxNa,"Unable to extract names of groups from MArrayObj$means neither $constratsts")

  if(length(useComp) ==0) useComp <- colnames(if("contrasts" %in% MArrayObj) MArrayObj$constratsts else MArrayObj$coefficients) else {
    if(is.numeric(useComp)) useComp <- colnames(if("contrasts" %in% MArrayObj) MArrayObj$constratsts else MArrayObj$coefficients)[as.integer(useComp)]
  }
  if(length(useComp) ==0) warning(fxNa,"Unable to find names of pairwise comparisons, nothing to do !") else {
    out <- indexGroupsFromPW(compNames=useComp, grp=grp, potSep=groupSep, includeGrp=TRUE, silent=silent, debug=debug, callFrom=fxNa)
    if(debug) {message(fxNa,"sNDM2"); sNDM2 <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep,grp=grp,out=out)}
   
    if(any(grepl("^ind", outTy))) out <- out$ind else if("names" %in% outTy) out <- out$GrpNames
    if("unique" %in% outTy && !is.list(out)) out <- unique(sort(out))
    out }
}
    
