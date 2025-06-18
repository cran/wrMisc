#' Choose Column Most Likely For Sample-Names 
#'
#' This function looks at all comumns of  \code{mat} which columns may be likely choices for sample-names and derives then group-names after stripping terminal enumerators.
#' Ideal sample-names should contain some replicates indicates as terminal enumerators.
#'
#' @details
#' The basic idea is that the column containing (good) samples-names contains all different entries and that by stripping terminal enumerators one can understand the grouping of replicates.
#' Note arguments  \code{asUnique} and \code{partEnumerator} influence which columns of \code{mat} will be evaluated/checked
#'
#'
#' @param mat (matrix or data.frame) contains possible choices for sample-names
#' @param useCoNa (character) optional custom choice for columns of \code{mat} to check; if \code{NULL} all columns will be used/checked
#' @param method (character) decide how to choose number of groups as : min, low, med, high, max or mode
#'   Note arguments  \code{asUnique} and \code{partEnumerator} influence which columns of \code{mat} will be evaluated/checked
#' @param sep (character) separators considered when searching and removing common words
#' @param rmTxt (character, length=1) optional removing of custom text (eg variable file-extensions); no obligation that \code{rmTxt} occurs in all instances
#' @param asUnique (logical) requires all (potential) samples-names to be unique (ie no repeats) to be considered for group-names; also removes all candidate columns with all different names
#' @param partEnumerator (logical) when \code{TRUE} allows some instances of (potential) sample-names without numerator: a1, a2, b (ie some wo enumerator)
#' 
#' @param fullReport (logical) if \code{TRUE} returns list with $group, $sampleNames, $col (iondex of column from mat and name of c)
#' 
#' @param silent (logical) suppress messages if \code{TRUE}
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a character vector with grouop-names (and sample-names as names of entries) or 
#'   if \code{fullReport=TRUE} a list with $group, $sampleNames, $col (index of column from \code{mat} and name of column
#' 
#' @seealso \code{\link{rmSharedWords}}, \code{\link{replicateStructure}}, \code{\link{protectSpecChar}}
#' @examples
#'  mat <- cbind(a=letters[1:6], b=paste(rep(c("b","B"), each=3), 1:3), c=rep(1,6), 
#'    d=gl(3,2), e=rep(c("e","E"),3), f=paste(rep(c("F","f","ff"), each=2), 1:2))
#' chooseGroupNames(mat, method="median")         # col 2 (b/B)
#' chooseGroupNames(mat, method="median", fullReport=TRUE) 
#' chooseGroupNames(mat, method="min")            # col 2 (b/B)
#' chooseGroupNames(mat, method="max")            # col 6 (F/f/ff)
#' chooseGroupNames(mat, method="max", asUnique=FALSE) # col 1 (a..)
#' @export
chooseGroupNames <- function(mat, useCoNa=NULL, method="median", sep=c("_","-"," ",".","=",";"), rmTxt=NULL, asUnique=TRUE, partEnumerator=FALSE, fullReport=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## Each column of mat gets inspected if it may contain sample-names from which group-names could be derived by dropping terminal enumerators
  ## returns vector  with group-names, names of vector as sample-names; In case of multiple possible hist the 1st is returned
  ## Aim : from 'mat' matrix (or data.frame) with possible choices (per column) choose most likely to use as sample-names :
  ##   should contain some replicates after stripping terminal enumerators
  ## method : how to choose number of groups lin, low, med, high, max, mode
  ## sep (character) separator for common words and/or terminal enumerators
  ## rmTxt (character) : remove text (no obligation to occur in all cases, eg variable writing of extensions)
  ## asUnique . requires all (potential) samples-names to be unique (ie no repeats) to be considered for group-names; also removes all candidate columns with all different names
  ## partEnumerator =T : allow : a1, a2, b (ie some wo enumerator)
  ## fullReport .. return list with $group, $sampleNames, $col (iondex of column from mat and name of c)
  ## find column containing suitable sample-names out of matrix or data.frame : remove extension .raw, simplify by removing shared words
  fxNa <- .composeCallName(callFrom, newNa="getSampleNames")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  chPat <- "[[:digit:]]+[[:space:]]*$"
  out <- NULL
  ## check input (useCoNa)
  iniColNa <- colnames(mat)
  if(length(mat) <1) warning("Nothing to do ..") else {
    if(length(dim(mat)) <1 && !is.list(mat)) mat <- matrix(mat, ncol=1, dimnames=list(names(mat), "col1"))}
  if(length(useCoNa) >0) {useCoNa <- useCoNa[which(useCoNa %in% colnames(mat))]
    if(length(useCoNa) <1 && !silent) message(fxNa,"None of specified columns found !! (using all available)") }
  if(length(useCoNa) <1 || all(is.na(useCoNa))) useCoNa <- colnames(mat) 
  ## remove file-ending, remove shared words
  saNa <- lapply(useCoNa, function(z) { if(z %in% colnames(mat)) {
    saNa1 <- if(length(rmTxt) ==1) sub(rmTxt,"", mat[,z]) else mat[,z]        # remove text
    rmSharedWords(saNa1, sep=sep, silent=silent, callFrom=fxNa)} })           # simplify
  if(debug) {message("cGN1")}

  ## optional filter for columns with unique (sample-)names only AND no NAs
  if(isTRUE(asUnique)) { chSa <- sapply(saNa, function(z) length(unique(z))==length(z) && !any(is.na(z)) )
    if(all(!chSa)) { saNa <- NULL
      warning(fxNa,"All of the potential columns for sample-names have non-unique sample-names or contains NAs (returning NULL)")
    } else {
      if(any(!chSa)) {chSa <- which(chSa); mat <- mat[, chSa, drop=FALSE]; useCoNa <- useCoNa[chSa]; saNa <- saNa[chSa]}}     
  }

  ## remove enumerator extensions
  if(length(saNa) >0) { 
     rmExt <- function(z, chPat, allEnumerator=TRUE) { out <- if(allEnumerator) {   # remove terminal enumerator: allEnumerator=TRUE : only when everywhere (ie don't allow empty)
        if(all(grepl(chPat, z))) sub(paste0("(",paste0(protectSpecChar(sep), collapse="|"),")*", "[[:digit:]]+[[:space:]]*$"),"", z) else z
      } else sub(paste0("(",paste0(protectSpecChar(sep), collapse="|"),")*", "[[:digit:]]+[[:space:]]*$"),"", z)
      if(any(nchar(out) <1, na.rm=TRUE)) out <- z
      out }
    ls2 <- lapply(saNa, rmExt, chPat, allEnumerator=!isTRUE(partEnumerator))
    if(debug) {message(fxNa,"cGN2")}
    ## check/choose for best human-readable, ie no-of-chars >0, text-numeric combination & containing some 'replicates' (but not all replic of 1st) 
    if(length(ls2) >1) { 
      nGr2 <- nGrp <- sapply(ls2, function(z) length(unique(naOmit(z))))
      if(isTRUE(asUnique)) nGr2[which(nGrp==1 | nGrp==nrow(mat))] <- NA 
      hasNA <- sapply(ls2, function(z) any(is.na(z)))
      nGrS <- sort(naOmit(nGr2))
      nGrX <- sort(unique(naOmit(nGr2)))           # overal possible numbers of groups
      if(TRUE && any(hasNA)) nGr2[which(hasNA)] <- NA
      modeX <- function(z) {y <- table(z); names(y)[which.max(y)]}      
      if(any(c("median","me") %in% method)) method <- "med"
      if(any(c("hi","hig","h","highest") %in% method)) method <- "high"
      if(any(c("lo","l","lowest") %in% method)) method <- "low"
      if(debug) message(fxNa,"Using method '",method,"'")
      useCol <- switch(method, 
        min= which(nGr2==min(nGr2, na.rm=TRUE))[1],
        low= which(nGr2==nGrX[1+floor(length(nGrX)/5)]),
        med= nGrS[floor(length(nGrS)/2)],  #which(nGr2==floor(median(nGr2, na.rm=TRUE)))[1],
        mode=which(nGr2==modeX(nGr2))[1],
        high=which(nGr2==nGrX[min(length(nGrX), 1+ceiling(4*length(nGrX)/5))]),
        max= which(nGr2==max(nGr2, na.rm=TRUE))[1] )
      if(length(useCol) <1) warning(fxNa,"Unable to understand 'method' (returning NULL)") else {
        out <- ls2[[useCol[1]]]
        names(out) <- saNa[[useCol[1]]] 
      } 
    } else {
      out <- ls2[[1]]
      names(out) <- saNa[[1]]  
    }
    if(isTRUE(fullReport)) {out <- list(group=out, sampleNames=names(out), col=which(iniColNa==colnames(mat)[useCol[1]])); names(out$col) <- useCoNa[useCol[1]]; out} else out 
  } else if(isTRUE(fullReport)) list(group=NULL, sampleNames=NULL, col=NULL) else NULL
}  
 
    