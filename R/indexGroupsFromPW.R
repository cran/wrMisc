#' Index Names Of Groups From Vector Of Concatenated Pairwise Group-Names  
#' 
#' This function allows matching which groups are cited in a pairwise concatenated manner.
#' This function can also be used to split concatenated group-names (while automatically determining a suitable separator) and display as matrix when \code{includeGrp=TRUE}.
#' Replaces matchSampToPairw() ???
#'
#' @param compNames (vector of character or integer, or matrix) names of pairwise combined group-names; if matrix the rownames of \code{compNames} will be used
#' @param grp (character or factor) optional groups (may include group-names that do not occur in \code{compNames})
#' @param potSep (character) potential separators to be checked if occuring in \code{useComp}; the first fitting will used
#' @param includeGrp (logical) instead of matrix, rather return list with $pwInd, $pwGrpNames (group-names corresponding to index), $sep (separator used to split) and $grpNa
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{moderTestXgrp}} (colnames of testing results from this function may be used); \code{\link{replacePWseparator}}; used internally \code{\link{getPWseparator}},  \code{\link{pwSeparatorList}}, \code{\link{.getPWseparator}}
#' @return This function returns a numeric matrix with indexes corresponding to (sorted) group-names as they appear in \code{compNames};
#'   or if \code{includeGrp=TRUE} a list with $ind (matrix indexes for each \code{compNames}), $grpNames (group-names corresponding to index), $sep (separator used to split) and $grpNa (sorted names of types of groups)
#' @examples
#' indexGroupsFromPW(c("C-B","B-D","C-D")) 
#' indexGroupsFromPW(c("C-B","B-D","C-D"), grp=rep(LETTERS[1:6],2), includeGrp=TRUE) 
#' @export
indexGroupsFromPW <- function(compNames, grp=NULL, potSep=pwSeparatorList("combine1"), includeGrp=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## Index Names Of Groups From Vector Of Concatenated Pairwise Group-Names
  fxNa <- .composeCallName(callFrom, newNa="indexGroupsFromPW")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  out <- out2 <- tmp <- NULL
  if(length(potSep) >0) potSep <- unique(naOmit(potSep))
  if(length(potSep)==0 || any(is.na(potSep))) { potSep <- pwSeparatorList("combine1") # c("-","---","_","___","."," ","  ","--","__")      # set to default
    if(debug) message(fxNa,"Argument 'potSep' appers empty or invalid, using default")
    if(length(compNames) >3) potSep <- unique(c(potSep[1],"--",potSep[2:3],"__",potSep[4:length(potSep)])) }
  ## main  
  if(length(compNames) >0) {
    if(debug) {message(fxNa,"iGFP1"); iGFP1 <- list(compNames=compNames, grp=grp,potSep=potSep)}
      #pwGrpNa, grpNa=
    if(length(potSep) != 1) {
       # getPWseparator(iGFP1$compNames, grp=iGFP1$grp)
       # getPWseparator(iGFP1$compNames, grp=iGFP1$grp, potSep=iGFP1$potSep, includeGrp=TRUE)
      tmp <- try(getPWseparator(compNames=if(length(dim(compNames))==2) rownames(compNames) else compNames, grp=grp, potSep=potSep, includeGrp=TRUE, silent=silent, debug=debug, callFrom=fxNa), silent=TRUE)
      if(debug) {message(fxNa,"iGFP1b"); iGFP1b <- list(compNames=compNames, grp=grp,potSep=potSep,tmp=tmp)}
      if(inherits(tmp, "try-error")) warning(fxNa,"Cannot figure out proper separator (returning NULL)") else potSep <- tmp$sep 
    }
    if(length(potSep) == 1) {
      if(length(grp) >0) {                    # use grp (if valid) instead of automatically determined grpNa
        grp <- unique(naOmit(grp))
        if(length(tmp) >0) {                  # from checking multiple potential sep : compNames already split to show grp
          chGr <- tmp$grpNa %in% grp
          if(any(!chGr)) { warning(fxNa,sum(chGr)," elements found by splitting not present in user-provided 'grp', adding ..")
            grp <- unique(grp, tmp$grpNa)
          } else grp <- tmp$grpNa   
        }
        if(debug) {message(fxNa,"iGFP1c"); iGFP1c <- list(compNames=compNames, grp=grp,potSep=potSep,tmp=tmp)}
      } 
      grp <- as.factor(grp)
      #if(length(tmp) >0 && !is.factor(tmp$grpNa)) tmp$grpNa <- as.factor(tmp$grpNa)
      if(debug) {message(fxNa,"iGFP3"); iGFP3 <- list(compNames=compNames, grp=grp,potSep=potSep,tmp=tmp)}
      
      out2 <- strsplit(if(length(dim(compNames)==2)) rownames(compNames) else compNames, potSep)
      chLe <- sapply(out2, length) !=2
      if(debug) {message(fxNa,"iGFP3b"); iGFP3b <- list(compNames=compNames, grp=grp,potSep=potSep,tmp=tmp,out2=out2,chLe=chLe,includeGrp=includeGrp)}
      if(any(chLe)) out <- NULL else {
          #matrix(match(unlist(iGFP3b$out2), iGFP3b$grp),ncol=2, byrow=TRUE)
        out <- matrix(match(unlist(out2), grp), ncol=2, byrow=TRUE, dimnames=list(if(length(dim(compNames)==2)) rownames(compNames) else compNames, c("samp","ref")))
        if(length(out2) >0) { out2 <- t(as.data.frame(out2)); dimnames(out2) <- dimnames(out)}          
        if(isTRUE(includeGrp)) { 
          out <- list(pwGrpNa=if(length(out2) >0) out2 else matrix(tmp$grpNa[out], ncol=2, dimnames=dimnames(out)), sep=potSep, index=out, grpNa=grp)
        }
      }
    } 
  } else if(debug) message(fxNa,"Argument 'compNames' is empty, nothing to do")
  out                              
}
    
