#' Identify Separator In Pairwise Group-Names Or Find Separator For Use With Pairwise Group-Names  
#'   
#' This function allows identifing separator used when pairwise groups are presented. 
#'
#' @details  
#' Note : Potential separators \code{potSep} must be at least 1 character long
#'   
#' The character '.' may used as separator (argument \code{potSep}), it will be protected internally for obtaining correct results when using grep().
#' Note : The indexing is relative to the levels of grp, thus to sorted group-names !
#' 
#' Cases of usage : 
#' 1) Identify separator used in concatenated labels, ie concatenated group-names are given, (only argument \code{compNames} is obligatory)
#' 
#' 2) Suggets/Find suitable separator for given group-labels (ie a separator not occurring in any of the group-labels) when names 
#'    of \code{grp} given (thus argument \code{grp} is obligatory), 
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
#' @return This function returns a character vector (length=1) of the (first) separator fitting to all data, or a list with $sep and $grpNa if \code{includeGrp=TRUE}
#' @examples
#' ## Suggest separator
#' getPWseparator(grp=LETTERS[1:3])
#' getPWseparator(grp=c("B-C","D","E")) 
#' getPWseparator(grp=c("B-C","D","E"), includeGrp=TRUE) 
#' 
#' ## Identify separator used
#' getPWseparator(compNames=c("B-C","B-D","C-D")) 
#' getPWseparator(compNames=c("B-C","B-D","C-D"), includeGrp=TRUE) 
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
  if(length(potSep)==0) { potSep <- pwSeparatorList(type="split1", callFrom=fxNa) # c("-","---","_","___",".","-vs-","_vs_","--vs--","__vs__"," ","  ","--","__")  
    if(debug) message(fxNa,"User provided 'potSep' seems empty, setting to default panel for checking") } 
  if(length(compNames) >0) {
    out <- .getPWseparator(compNames, potSep=potSep, includeGrp=TRUE, silent=silent, debug=debug, callFrom=fxNa)
    sep <- out$sep
    if(length(grp) >0) {
      chGrp <- out$grpNa %in% naOmit(unique(grp))
      if(any(!chGrp)) { if(!silent) message(fxNa,"NOTE: ",sum(!chGrp)," elements found based on 'compNames' but not in user-provided 'grp', adding ..")
        grp <- naOmit(unique(grp, out$grpNa)) }
    } else grp <- out$grpNa
  } else {
    if(length(grp) > 0) grp <- naOmit(unique(grp))
    if(length(grp) > 0) {
      if(length(potSep)==1 && any(grepl("(split)|(combine)[[:digit:]]{0,1}", potSep), na.rm=TRUE)) potSep <- pwSeparatorList(type=potSep, silent=silent, debug=debug, callFrom=fxNa)
      potSep2 <- protectSpecChar(potSep)               ## protect '.' etc
      ## check for optimal separator
      chS <- try(sapply(potSep2, grepl, grp), silent=TRUE)
      if(debug) {message(fxNa,"gPWS1"); gPWS1 <- list(compNames=compNames,grp=grp,potSep=potSep,potSep2=potSep2,chS=chS)}
      if(!inherits(chS, "try-error")) {
        chS <- colSums(chS)
        if(any(chS ==0)) sep <- potSep[which(chS==0)[1]]
      } else if(!silent) message(fxNa,"FAILED checking for suitable SEPARATOR (maybe content of 'potSep' contains special characters that need getting protected ?)")
    } else warning(fxNa,"Arguments 'grp' and 'compNames' seems empty, nothing to do")
  }
  if(isFALSE(includeGrp)) sep else list(sep=sep, grpNa=grp)
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
#' @return This function returns a character vector (length=1) of the (first) separator fitting to all data, or a list with $sep and $grpNa if \code{includeGrp=TRUE}
#' @examples
#' .getPWseparator(compNames=c("B-C","B-D","C-D")) 
#' @export
.getPWseparator <- function(compNames, potSep="split1", includeGrp=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## find sperator of pairwise groups (knowing names of groups) : which potSep occurs only once in all cases
  fxNa <- .composeCallName(callFrom, newNa=".getPWseparator")
  if(length(potSep) !=0 && "" %in% potSep) potSep <- potSep[-which(potSep =="")]
  if(length(potSep) ==0 || any(is.na(potSep))) { potSep <- pwSeparatorList(type="split1", callFrom=fxNa)  #c("-","--","_","__",".","-vs-","_vs_","--vs--","__vs__"," ","  ","--","__") 
    if(!silent) message(fxNa,"Argument 'potSep' appears empty, setting to default") 
  } else potSep <- potSep[1]
  if(grepl("(split)|(combine)[[:digit:]]{0,1}", potSep)) potSep <- pwSeparatorList(potSep, callFrom=fxNa)

  potSep2 <- potSep  # for protected version
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
  if(!isTRUE(includeGrp)) groupSep else list(sep=groupSep, grpNa= sort(unique(unlist(strsplit(compNames,groupSep)))) )
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
  if(length(newSep)==0) stop("Argument 'newSep' may not be NULL")
  if(length(potSep)==0 || any(is.na(potSep))) potSep <- pwSeparatorList("split1")  #c("-","---","_","___",".","-vs-","_vs_","--vs--","__vs__"," ","  ")
  
  if(length(newSep) >1) {newSep <- naOmit(newSep)[1]; message(fxNa,"Only 1st value of 'newSep' will be used")}
  tmp <- try(.getPWseparator(compNames, potSep=potSep, includeGrp=FALSE, debug=debug, callFrom=fxNa), silent=TRUE)
  ## check for mult subst
  if(!inherits(tmp, "try-error")) {
    ch1 <- grepl(newSep, compNames) 
    if(any(ch1)) warning(fxNa,"Problem : ",sum(ch1)," entries contain already ",newSep," , ie group-names will appear different")
    potSepP <- protectSpecChar(tmp)
    out <- sub(potSepP, newSep, compNames)
    out 
  } else NULL
}    
        
 
