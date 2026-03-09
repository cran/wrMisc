#' Index Names Of Groups From Vector Of Concatenated Pairwise Group-Names  
#' 
#' This function allows matching which groups are cited in a pairwise concatenated manner.
#' This function can also be used to split concatenated group-names (while automatically determining a suitable separator) and display as matrix when \code{includeGrp=TRUE}.
#' Replaces matchSampToPairw() ???
#'
#' @param compNames (vector of character or integer, or matrix) names of pairwise combined group-names; if matrix the rownames of \code{compNames} will be used
#' @param grp (character or factor) optional groups (may include group-names that do not occur in \code{compNames})
#' @param potSep (character) potential separators to be checked if occuring in \code{useComp}; the first fitting will used
#' @param includeGrp (logical) instead of matrix, rather return list with $ind, $GrpNames (group-names corresponding to index), $sep (separator used to split) and $grpTy 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{moderTestXgrp}} (colnames of testing results from this function may be used); \code{\link{replacePWseparator}}; \code{\link{getPWseparator}}  used internally  \code{\link{.getPWseparator}}
#' @return This function returns a numeric matrix with indexes corresponding to (sorted) group-names as they appear in \code{compNames};
#'   or if \code{includeGrp=TRUE} a list with $ind (matrix indexes for each \code{compNames}), $GrpNames (group-names corresponding to index), $sep (separator used to split) and $grpTy (sorted names of types of groups)
#' @examples
#' indexGroupsFromPW(c("C-B","B-D","C-D")) 
#' indexGroupsFromPW(c("C-B","B-D","C-D"), grp=rep(LETTERS[1:6],2), includeGrp=TRUE) 
#' @export
indexGroupsFromPW <- function(compNames, grp=NULL, potSep=c("-","---","_","___","."," ","  ","--","__"), includeGrp=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## Index Names Of Groups From Vector Of Concatenated Pairwise Group-Names
  fxNa <- .composeCallName(callFrom, newNa="indexGroupsFromPW")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  out <- out2 <- NULL
  if(length(potSep) >0) potSep <- unique(naOmit(potSep))
  if(length(potSep)==0) { potSep <- c("-","---","_","___","."," ","  ","--","__")      # set to default
    if(length(compNames) >0) potSep <- c(potSep[1],"--",potSep[2:3],"__",potSep[4:length(potSep)]) }
  ## main  
  if(length(compNames) >0) {
    if(debug) {message(fxNa,"iGFP1"); iGFP1 <- list(compNames=compNames, grp=grp,potSep=potSep)}
    tmp <- try(.getPWseparator(if(length(dim(compNames)==2)) rownames(compNames) else compNames, potSep=potSep, includeGrp=TRUE, silent=silent, debug=debug, callFrom=fxNa), silent=TRUE)
    if(debug) {message(fxNa,"iGFP1b"); iGFP1b <- list(compNames=compNames, grp=grp,potSep=potSep,tmp=tmp)}
    if(inherits(tmp, "try-error")) warning(fxNa,"Cannot figure out proper separator (returning NULL)") else { 
      if(length(grp) >0) {                    # use grp (if valid) instead of automatically determined grpTy
        grp <- unique(naOmit(grp))
        chGr <- tmp$grpTy %in% grp
        if(any(!chGr)) { grp <- NULL
          warning(fxNa,"Invalid entry of argument 'grp', ",sum(!chGr)," labels missing in 'grp' but occuring in 'compNames'; ignoring")
        } else tmp$grpTy <- grp
      } 
      if(!is.factor(tmp$grpTy)) tmp$grpTy <- as.factor(tmp$grpTy)
      if(debug) {message(fxNa,"iGFP3"); iGFP3 <- list(compNames=compNames, grp=grp,potSep=potSep,tmp=tmp)}
      if(!inherits(tmp, "try-error")) {  
        out2 <- strsplit(if(length(dim(compNames)==2)) rownames(compNames) else compNames, tmp$sep)
        chLe <- sapply(out2, length) !=2
        if(any(chLe)) out <- NULL else {
          out <- matrix(match(unlist(out2), levels(tmp$grpTy)), ncol=2, byrow=TRUE, dimnames=list(if(length(dim(compNames)==2)) rownames(compNames) else compNames, c("samp","ref")))
          if(length(out2) >0) {out2 <- t(as.data.frame(out2)); dimnames(out2) <- dimnames(out)}          
          if(isTRUE(includeGrp)) { 
            out <- list(ind=out, GrpNames=if(length(out2) >0) out2 else matrix(tmp$grpTy[out], ncol=2, dimnames=dimnames(out)), sep=tmp$sep, grpTy=tmp$grpTy)
          }
        }
      }  
    } 
  } else if(debug) message(fxNa,"Argument 'compNames' is empty, nothing to do")
  out                              
}
  
#' Identify Separator In Pairwise Group-Names Or Find Separator For Use With Pairwise Group-Names  
#'   
#' This function allows identifing separator used when pairwise groups are presented. 
#'
#' @details  
#' Note : Potential separators \code{potSep} must be at least 1 character long
#'   
#' The character '.' may used as \code{potSep}, it will be protected internally for correct results.
#' Note : The indexing is relative to the levels of grp, thus to sorted group-names !
#' 
#' Cases of usage : 
#' 1) Identify separator used in concatenated labels, ie concatenated group-names are given, (argument \code{grp} is not obligatory)
#' 
#' 2) Find suitable identifyer for given group-labels (ie separator not occurring in any of the group-labels) when names of \code{grp} given, 
#'    the first instance of \code{potSep} fitting will be chosen
#' 
#' @param compNames (character or integer) names of pairwise combined group-names
#' @param grp (character or factor) optional groups (may include group-names that do not occur in \code{compNames})
#' @param potSep (character) potential separators to be checked if occuring in \code{useComp}; the first fitting will used
#' @param includeGrp (logical) if \code{includeGrp=TRUE} a list including also the group-names is returned
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso \code{\link{indexGroupsFromPW}}, \code{\link{getPWseparator}}, colnames of testing results from \code{\link{moderTestXgrp}}, used by \code{\link{replacePWseparator}} 
#' @return This function returns a character vector (length=1) of the (first) separator fitting to all data, or a list with $sep and $grpTy if \code{includeGrp=TRUE}
#' @examples
#' getPWseparator(compNames=c("B-C","B-D","C-D")) 
#' getPWseparator(grp=c("B-C","D","E")) 
#' @export
getPWseparator <- function(compNames=NULL, grp=NULL, potSep=c("-","---","_","___",".","-vs-","_vs_","--vs--","__vs__"," ","  ","--","__"), includeGrp=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## find sperator of pairwise groups (knowing names of groups)
  fxNa <- .composeCallName(callFrom, newNa="getPWseparator")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  out <- out2 <- sep <- NULL
  if(length(compNames) >0) compNames <- naOmit(unique(compNames))
  if(length(dim(compNames)) >1) { compNames <- NULL
    warning(fxNa,"Invalid entry of 'compNames', should be factor or (character) vector")}
  if(length(compNames) >0) {
    out <- .getPWseparator(compNames, potSep=potSep, includeGrp=TRUE, silent=silent, debug=debug, callFrom=fxNa)
    sep <- out$sep
    grp <- naOmit(unique(grp))
  } else {
    if(length(grp) > 0) grp <- naOmit(unique(grp))
    if(length(grp) > 0) {      
      potSep2 <- protectSpecChar(potSep)               ## protect '.' etc
      ## check for optimal separator
      chS <- try(sapply(potSep2, grepl, grp), silent=TRUE)
      if(debug) {message(fxNa,"gPWS1"); gPWS1 <- list(compNames=compNames,grp=grp,potSep=potSep,potSep2=potSep2,chS=chS)}
      if(!inherits(chS, "try-error")) {
        chS <- colSums(chS)
        if(any(chS ==0)) sep <- potSep[which(chS==0)[1]]
      } else if(!silent) message(fxNa,"Failed checking for suitable separator (maybe content of 'potSep' contains special characters that need to get protected ?)")
    } else warning(fxNa,"Arguments 'grp' and 'compNames' seems empty, nothing to do")
  }
  if(isFALSE(includeGrp)) sep else list(sep=sep, grpTy=grp)
}  


#' Find Separator In Vector Of Pairwise Group-Names  
#'   
#' This function allows identifing separator used when pairwise groups are presented. 
#'
#' @details  
#' Note : Potential separators \code{potSep} to be tested must be at least 1 character long
#'   
#' The character '.' may used as \code{potSep}, it will be protected internally for correct results.
#' Note : The indexing is relative to the levels of grp, thus to sorted group-names !
#' 
#' @param compNames (character or integer) names of pairwise combined group-names
#' @param potSep (character) potential separators to be checked if occuring in \code{useComp}; the first fitting will used
#' @param includeGrp (logical) if \code{includeGrp=TRUE} a list including also the group-names is returned
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{indexGroupsFromPW}}, \code{\link{getPWseparator}}, colnames of testing results from \code{\link{moderTestXgrp}}, used by \code{\link{replacePWseparator}}
#' @return This function returns a character vector (length=1) of the (first) separator fitting to all data, or a list with $sep and $grpTy if \code{includeGrp=TRUE}
#' @examples
#' .getPWseparator(compNames=c("B-C","B-D","C-D")) 
#' @export
.getPWseparator <- function(compNames, potSep=c("-","---","_","___",".","-vs-","_vs_","--vs--","__vs__"," ","  ","--","__"), includeGrp=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## find sperator of pairwise groups (knowing names of groups) : which potSep occurs only once in all cases
  fxNa <- .composeCallName(callFrom, newNa=".getPWseparator")
  if("" %in% potSep) potSep <- potSep[-which(potSep =="")]
  if(length(potSep)==0) { potSep <- c("-","--","_","__",".","-vs-","_vs_","--vs--","__vs__"," ","  ","--","__") 
    if(!silent) message(fxNa,"Argument 'potSep' appears empty, setting to default") }
  potSep2 <- potSep
  chSepFx <- function(sep, txt) all(nchar(gsub(sep,"",txt)) +nchar(gsub("\\\\","",sep)) ==nchar(txt)) && !any(grepl(paste0(sep,"$"), txt)) && !any(grepl(paste0("^",sep), txt)) # check that candiates for separator if occoring only once .. 
  ## protect \\
  chP <- grepl("\\.", potSep2)
  if(any(chP)) potSep2 <- gsub("\\.","\\\\.", potSep2)      
  ## protect '.'
  chSpc <- potSep %in% '.' 
  if(any(chSpc))  potSep2[which(chSpc)] <- "\\."

  ## check for optimal separator
  chSep <- sapply(potSep2, chSepFx, compNames)
  if(sum(chSep, na.rm=TRUE) >0) groupSep <- potSep[which(chSep)[1]] else stop(fxNa,"Cannot figure out proper separator for elements like  ",pasteC(utils::head(compNames), quoteC="'"))
  if(!isTRUE(includeGrp)) groupSep else list(sep=groupSep, grpTy= sort(unique(unlist(strsplit(compNames,groupSep)))) )
}


#' Replace Separator In Vector Of Pairwise Group-Names  
#'   
#' This function allows identifying and substituting a separator used in a character vector concatenated of pairwise groups.
#'
#' @param compNames (character or integer) names of pairwise combined group-names
#' @param newSep (character of length=1) new separator between group-names for pair of names; if \code{NULL} a suitable separator (among '-','--','_','__','.',' ' and '  ') will be used
#' @param potSep (character) potential separators to be checked if occuring in \code{useComp}; the first fitting will used
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso  \code{\link{indexGroupsFromPW}}, \code{\link{getPWseparator}}, \code{\link{.getPWseparator}}
#' @return This function returns a character vector with group-wise substituted separators (or \code{NULL} if conflicts appear) 
#' @examples
#' replacePWseparator(compNames=c("B-C","B-D","C-D"), newSep="-vs-") 
#' @export
replacePWseparator <- function(compNames, newSep, potSep=c("-","---","_","___",".","-vs-","_vs_","--vs--","__vs__"," ","  "), silent=FALSE, debug=FALSE, callFrom=NULL)  {
  ## Replace Separator In Vector Of Pairwise Group-Names  
  fxNa <- .composeCallName(callFrom, newNa="replacePWseparator")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(length(potSep)==0) potSep <- c("-","---","_","___",".","-vs-","_vs_","--vs--","__vs__"," ","  ")
  if(length(newSep)==0) stop("Argument 'newSep' may not be NULL")
  if(length(newSep) >1) {newSep <- naOmit(newSep)[1]; message(fxNa,"Only 1st value of 'newSep' will be used")}
  tmp <- try(.getPWseparator(compNames, potSep=potSep, includeGrp=FALSE, debug=debug, callFrom=fxNa), silent=TRUE)
  ## check for mult subst
  if(!inherits(tmp, "try-error")) {
    ch1 <- grepl(newSep, compNames) 
    if(any(ch1)) warning(fxNa,"Problem : ",sum(ch1)," entries contain already ",newSep," , ie group-names will appear different")
    potSepP <- protectSpecChar(tmp)
    out <- sub(potSepP, newSep, compNames)
    out } else NULL
}   
    
