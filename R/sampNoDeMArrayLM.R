#' Locate Sample Index From Index or Name Of Pair-Wise Comparisons in list or MArrayLM-Object  
#'
#' When multiple series of data are tested simultaneaously (eg using \code{moderTestXgrp}), multiple pairwise comparisons get performed. 
#' This function helps locating the samples, ie mean-columns, corresponding to a specific pairwise comparison.
#' 
#' @details  
#' As main input one gives a list or MArrayLM-object containing testing results contain the pairwise comparisons
#' and a specific comparison indicated by \code{useComp} to get located in the element of mean-columns (\code{lstMeans}) among all pairwise comparisons.
#'
#' @param MArrayObj (list or MArray-object) main input
#' @param useComp (character or integer) index or name of pairwise-comparison to be addressed
#' @param groupSep (character, length=1) separator for paitr of names 
#' @param lstMeans (character, length=1) the list element containing the individual sample names, typically the matrix containing the replicate-mean values for each type of sample, the column-names get used
#' @param lstP (character, length=1) the list element containing all pairwise comparisons performed, the column-names get used
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{moderTestXgrp}}, this function gets used eg in \code{\link[wrGraph]{MAplotW}} or \code{\link[wrGraph]{VolcanoPlotW}}
#' @return This fuction returns a numeric vector (length=2) with index indicating the columns of (replicate) mean-values corresponding to the comparison specified in \code{useComp} 
#' @examples
#' grp <- factor(rep(LETTERS[c(3,1,4)],c(2,3,3)))
#' set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4),2), ncol=8,
#'   dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste(grp,c(1:2,1:3,1:3),sep="")))
#' test8 <- moderTestXgrp(t8, grp) 
#' head(test8$p.value)         # all pairwise comparisons available
#' if(requireNamespace("limma", quietly=TRUE)) {  # need limma installed...
#'   sampNoDeMArrayLM(test8,1)
#'   head(test8$means[,sampNoDeMArrayLM(test8,1)])
#'   head(test8$means[,sampNoDeMArrayLM(test8,"C-D")]) }
#' 
#' @export
sampNoDeMArrayLM <- function(MArrayObj, useComp, groupSep="-", lstMeans="means",lstP=c("BH","FDR","p.value"), silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## locate sample index from index or name of pair-wise comparisons in list or MArrayLM-object
  fxNa <- .composeCallName(callFrom, newNa="sampNoDeMArrayLM")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  errMsg <- c("Argument 'MArrayObj' is ","empty","doesn't contain the list-element needed  ('",lstMeans,"') !")
  if(length(MArrayObj) <1) stop(errMsg[1:2])
  if(length(MArrayObj[[lstMeans]]) <1) stop(errMsg[-2])
  if(length(colnames(MArrayObj[[lstMeans]])) <1) stop("Problem with 'MArrayObj$lstMeans' (does not contain matrix of means)")
  if(ncol(MArrayObj[[lstMeans]])==2) {             # only 2 mean-values, no other choice, don't need to try matching anything
    if(!identical(as.character(useComp),"1") & !silent) message(fxNa,"Only 2 columns of mean-values available, can't interpret properly 'useComp=",useComp,"'")
    out <- 1:2
  } else {
    if(length(lstP) <0) stop(" 'lstP' is empty !")
    chP <- lstP %in% names(MArrayObj)
    if(any(chP) & length(lstP) >0) lstP <- lstP[which(chP)[1]]
    if(length(colnames(MArrayObj[[lstP]])) <1)  stop("Problem with 'MArrayObj' (can't find names of comparisons in '",lstP,"')")
    ## convert/locate names to index 
    if(is.character(useComp) & length(grep("[[:alpha:]]",useComp)) >0) useComp <- naOmit(match(useComp, colnames(MArrayObj[[lstP]]) ))
    if(length(useComp) <1) stop("Argument 'useComp' is empty or can't locate in comparison-names")
    ## main
    out <- if(ncol(MArrayObj[[lstP]])==2 & colnames(MArrayObj[[lstP]])[1] =="(Intercept)") 1:2 else {
      matchSampToPairw(grpNa=colnames(MArrayObj[[lstMeans]]), pairwNa=colnames(MArrayObj[[lstP]])[useComp], sep=groupSep, silent=silent, callFrom=fxNa)}
    if(length(useComp)==1) out <- as.integer(out)}
  out }
   
