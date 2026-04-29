#' Locate Sample Index From Index Or Name Of Pair-Wise Comparisons In List Or MArrayLM-Object  
#'
#' When multiple series of data are tested simultaneaously (eg using \code{moderTestXgrp}), multiple pairwise comparisons get performed. 
#' This function helps locating the groups of samples, eg respective mean-columns, corresponding to specific pairwise comparisons.
#' 
#' @details  
#' As main input one gives a list or MArrayLM-object containing testing results from pairwise comparisons,
#' and specific comparisons indicated by \code{useComp} to get located in the elements of mean-columns (\code{lstMeans}).
#' 
#' Note, that that the numeric indexes should be used with caution as they refer to the levels of the groups which are sorted
#' and thus not necessarily in the order of other elements like the group-means. 
#'
#' This function allows finding inverted questions (eg 'D-C' even though 'C-D' was actually run in testing), but the result will always be presented in the initial order
#' 
#' @param MArrayObj (list or MArray-object) main input, object to examine and extract pairwise question information
#' @param useComp (character or integer, length=1) index or name of pairwise-comparison to be addressed
#' @param outTy (character) choose if just vector of indexes (with \code{outTy="index0"}), matrix of indexes (organized by pairwise questions, with \code{outTy="index"}), 
#'    or matrix of group-names (with \code{outTy="names"}) or list with $index0 (osrted unique vector or indexes),  $pwGrpNa, $pwIndex, $concat, $sep and $compIndex will get returned
#' @param groupSep (NULL or character of length=1) separator for pair of names; if \code{NULL} a suitable separator (among '-','--','_','__','.',' ' and '  ') will be used
#' @param combPwSep (character) sequence of separators for building combined names (may be combPwSep='combine1' or 'combine2'); will be used in pwSeparatorList()  
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{moderTestXgrp}}, this function gets used eg in \code{\link[wrGraph]{MAplotW}} or \code{\link[wrGraph]{VolcanoPlotW}}
#' @return This function returns if \code{outTy="names"} a numeric vector with indexes indicating the sorted (!!) columns of (replicate) mean-values corresponding to the comparison specified in \code{useComp};
#'   or if \code{outTy="names"} or  \code{outTy="pwGrpNa"} :  the (separated) names of pairwise groups will be returned as matrix (each pairwise question as separate line) 
#'   or if \code{outTy="list"} a list with $index0 (sorted unique vector or indexes), $pwGrpNa (the separated pairwise group-names), $pwIndex (the ith pairwise comparison), $concat (the concatenated pairwise names), $sep (pairwise separator) and $compIndex 
#' @examples
#' grp3 <- factor(rep(LETTERS[4:2], c(2,3,3)))
#' set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4), 2), ncol=8,
#'   dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste0(grp3,c(1:2,1:3,1:3))))
#' if(requireNamespace("limma", quietly=TRUE)) {  # need limma installed...
#'   test8 <- moderTestXgrp(t8, grp3, useComparison="all") 
#'   head(test8$p.value)         # all pairwise comparisons available
#'   sampNoDeMArrayLM(test8, 3)   
#'   levels(grp3)[sampNoDeMArrayLM(test8, 3)]
#'   head(test8$means[,sampNoDeMArrayLM(test8, 3, outTy="names")], 3)      
#'   head(test8$means[,sampNoDeMArrayLM(test8, "D-C", outTy="names")], 3) # inverted question
#' } 
#' @export
sampNoDeMArrayLM <- function(MArrayObj, useComp=NULL, outTy="index", groupSep=NULL, combPwSep="combine1", silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## full list of groups rather in mean-column - if available (seed into indexGroupsFromPW() ) 
  ## locate sample index from index or name of pair-wise comparisons in list or MArrayLM-object
  ## no need to give grp since columns are considered exhaustive ??
  fxNa <- .composeCallName(callFrom, newNa="sampNoDeMArrayLM")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  
  invQuestion <- FALSE
  errMsg <- c("Argument 'MArrayObj' is ","empty")
  if(length(MArrayObj) <1) stop(errMsg[1:2])
  if(debug) {message(fxNa,"sNDM1"); sNDM1 <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep)}
  
  ## check for complete set of groups from  respective mean-columns
  setup <- if("setup" %in% names(MArrayObj)) MArrayObj$setup else getPairwiseSetup(pwGrpNa=colnames(MArrayObj$coefficients), grpNa=if("means" %in% names(MArrayObj)) colnames(MArrayObj$means) else colnames(MArrayObj$design), sep=groupSep, combPwSep=combPwSep, silent=silent, debug=debug, callFrom=fxNa)
  
  if(length(useComp)==0) useComp <- 1 else {                          # set to default if empty
    chNA <- is.na(useComp)
    if(all(chNA)) useComp <- 1 else if(any(chNA)) useComp <- useComp[which(!chNA)]} 
  chNum <- grepl("^[[:digit:]]+$", useComp)
  if(length(dim(useComp)) ==2 && ncol(useComp)==2) {
    ## matrix
    if(debug) {message(fxNa,"sNDM2"); sNDM2 <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep,chNum=chNum,setup=setup)}
    if(all(chNum)) {  useComp <- matrix(as.integer(useComp), ncol=ncol(useComp), dimnames=dimnames(useComp))
      ## need to match pair of indexes (NOTE : interpreted to/as index to SORTED names of groups)
      useComp <- which(paste0(setup$pwIndex[,1],"-",setup$pwIndex[,2]) %in% paste0(useComp[,1],"-",useComp[,2]))
      if(length(useComp)==0) { 
        useComp <- which(paste0(setup$pwIndex[,1],"-",setup$pwIndex[,2]) %in% paste0(useComp[,2],"-",useComp[,1]))
        if(length(useComp)==0) {
          if(!silent) message(fxNa,"Don't understand index-indications for 'useComp' (supposed as matrix of pairwise indexes), setting to default =1  (68)")
          useComp <- 1 } else invQuestion <- TRUE
      }    
    } else {                 ## suppose as matrix of group-names
      if(debug) {message(fxNa,"sNDM2b"); sNDM2b <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep,chNum=chNum,setup=setup)}    
      useComp <- matrix(match(useComp, colnames(if("means" %in% names(MArrayObj)) MArrayObj$means else MArrayObj$design)), ncol=2, dimnames=dimnames(useComp))
      chNA <- is.na(useComp)
      useComp <- which(rowSums(useComp)==0)
      if(all(chNA) || length(useComp)) { if(!silent) message(fxNa,"Don't understand index-indications for 'useComp' (supposed as matrix of pairwise group-names), setting to default =1  (76)")
        useComp <- 1
      }
    }                # finish useComp as pairwise matrix
  } else {
    if(debug) {message(fxNa,"sNDM3"); sNDM3 <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep,chNum=chNum,setup=setup)}
    ##  useComp as vector
    if(all(chNum)) { useComp <- as.integer(useComp)
      ch1 <- (useComp >0) & (useComp <= nrow(setup$pwGrpNa))
      if(!all(ch1)) {if(!silent) message(fxNa,"Don't understand index-inidications for 'useComp' (values not in range available), setting to default =1  (85)")
        useComp <- 1
      } else if(any(!ch1)) useComp <- useComp[which(ch1)]
    } else {
      ## character vector - try to find in pairwise names
      if(debug) {message(fxNa,"sNDM3b"); sNDM3b <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep,chNum=chNum,setup=setup)}
      
      useComp1 <- useComp2 <- NULL
      ch2 <- useComp %in% colnames(if("contrasts" %in% MArrayObj) MArrayObj$contrasts else MArrayObj$coefficients)
      if(any(ch2)) useComp1 <- which(colnames(if("contrasts" %in% MArrayObj) MArrayObj$contrasts else MArrayObj$coefficients) %in% useComp) 
      if(any(!ch2)) {
        ## could contain reversed question(s)
        ch3 <- useComp %in% paste0(setup$pwGrpNa[,2], setup$sep, setup$pwGrpNa[,1])
        if(any(ch3)) { useComp2 <- which(paste0(setup$pwGrpNa[,2], setup$sep, setup$pwGrpNa[,1]) %in% useComp)
          invQuestion <- TRUE}
      }
      useComp <- c(useComp1, useComp2)
      if(length(useComp) ==0) {
        if(!silent) message(fxNa,"Don't understand text-indications for 'useComp' (names not appearing in existing pairwise sets), setting to default =1  (103)")
        useComp2 <- 1 
      }       
    }
  }       # finish determining useComp (as index)
  if(!silent && invQuestion) message(fxNa,"NOTE : Some questions were found as inverted questions")
  
  chTy <- c("names","pwGrpNa","list") %in% outTy
  if(debug) {message(fxNa,"sNDM4"); sNDM4 <- list(MArrayObj=MArrayObj,useComp=useComp,groupSep=groupSep,chNum=chNum,setup=setup,outTy=outTy,chTy=chTy)}
  if(any(chTy, na.rm=TRUE)) {
    ##
    pwNa <- colnames(if("contrasts" %in% MArrayObj) MArrayObj$contrasts else MArrayObj$coefficients)[useComp]
    if(any(chTy[1:2], na.rm=TRUE)) setup$pwGrpNa[useComp,,drop=FALSE] else {
      list(index0= unique(sort(as.integer(setup$pwIndex[useComp,]))), pwGrpNa=setup$pwGrpNa[useComp,,drop=FALSE], pwIndex=setup$pwIndex[useComp,,drop=FALSE], concat=setup$concat[useComp], sep=setup$sep, compIndex=outTy)
    }
  } 
}  
  
 
