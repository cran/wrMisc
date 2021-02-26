#' Match names to concatenated pairs of names   
#'
#' @description
#' The column-names of multiple pairwise testing contain the names of the initial groups/conditions tested, plus there is a separator (eg '-' in \code{moderTestXgrp}).  
#' Thus function allows to map back which groups/conditions were used by returning the index of the respective groups used in pair-wise sets. 
#' 
#' @details 
#' There are two modes of operation : 1) Argument \code{sep} is set to \code{NULL} :  The names of initial groups/conditions (\code{grpNa}) 
#' will be tested for exact pattern matching either at beginning or at end of pair-wise names (\code{pairwNa}).
#' This approach has the advantage that it does not need to be known what character(s) were used as separator (or they may change), 
#' but the disadvantage that in case the perfect \code{grpNa} was not given, the longest best match of \code{grpNa} will be returned.
#'
#' 2) The separator \code{sep} is given and exact matches at both sides will be searched.
#' However, if the character(s) from \code{sep} do appear inside \code{grpNa} no matches will be found.
#'
#' If some \code{grpNa} are not found in \code{pairwNa} this will be marked as NA.  
#'	 
#' @param grpNa (character) the names of the groups of replicates (ie conditions) used to test
#' @param pairwNa (character) the names of pairwise-testing (ie 'concatenated' \code{sampNa}
#' @param sep (character) if not \code{NULL} the characters given will be used via \code{stringsplit} 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return matrix of 2 columns with inidices of \code{sampNa} with \code{pairwNa} as rows
#' @seealso (for running multiple pair-wise test) \code{\link{moderTestXgrp}}, \code{\link[base]{grep}}, \code{\link[base]{strsplit}}
#' @examples
#' pairwNa1 <- c("abc-efg","abc-hij","efg-hij")
#' grpNa1 <- c("hij","abc","abcc","efg","klm")
#' matchSampToPairw(grpNa1, pairwNa1) 
#' 
#' pairwNa2 <- c("abc-efg","abcc-hij","abc-hij","abc-hijj","zz-zz","efg-hij")
#' matchSampToPairw(grpNa1, pairwNa2) 
#' @export
matchSampToPairw <- function(grpNa, pairwNa, sep=NULL, silent=FALSE, callFrom=NULL) {
  ## return index of grpNa for each pairwNa
  fxNa <- .composeCallName(callFrom, newNa="matchSampToPairw")
  chPw <- duplicated(pairwNa)
  if(any(chPw)) {if(!silent) message(fxNa," some entries of 'pairwNa' are duplicated, removing")
    pairwNa <- pairwNa[-which(chPw)]} 
  chNa <- duplicated(grpNa)
  if(any(chNa)) {if(!silent) message(fxNa," some entries of 'grpNa' are duplicated, removing")
    grpNa <- grpNa[-which(chNa)]} 
  if(length(grpNa) <2) stop("Insufficient 'grpNa' furnished") 
  if(length(pairwNa) <1) stop("Insufficient 'pairwNa' furnished")
  if(length(sep) >1) {sep <- wrMisc::naOmit(as.character(sep))[1]
    if(!silent) message(fxNa,"'sep' should be of length=1, using first entry")} 
  if(any(is.na(sep))) { sep <- NULL
    if(!silent) message(fxNa,"invalid entry for 'sep', setting to NULL")} 
  ## main
  if(length(sep==1)) {
    spl <- strsplit(pairwNa, as.character(sep))
    chLe <- sapply(spl, length)
    if(any(chLe <2)) stop(" Problem: Separator '",sep,"' seems NOT to occur in 'pairwNa' !")
    if(any(chLe >2)) { if(!silent) message(fxNa," separator '",sep,"' seems to occur multiple times in ",sum(chLe >2),"'pairwNa', using 1st and last of strsplit")
      spl[which(chLe >2)] <- lapply(spl[which(chLe >2)], function(x) x[c(1,length(x))]) }
    out <- t(sapply(spl,match,grpNa))
  } else {
    le <- apply(sapply(pairwNa, function(x) {w <- sapply(grpNa, function(y) length(grep(paste0("^",y),x)) >0)
      if(sum(w) >1) {v <- rep(FALSE, sum(w)); v[which.max(nchar(grpNa)[which(w)])] <- TRUE; w[which(w)] <- v}; w} ), 2, which)
    ri <- apply(sapply(pairwNa, function(x) {w <- sapply(grpNa, function(y) length(grep(paste0(y,"$"),x)) >0)
      if(sum(w) >1) {v <- rep(FALSE, sum(w)); v[which.max(nchar(grpNa)[which(w)])] <- TRUE; w[which(w)] <- v}; w} ), 2, which)
    if(is.list(le)) {le[which(sapply(le,length) <1)] <- NA; le <- unlist(le)}
    if(is.list(ri)) {ri[which(sapply(ri,length) <1)] <- NA; ri <- unlist(ri)}
    out <- cbind(le,ri) }
  #if(length(pairwNa)==nrow(out)) names(out) <- pairwNa
  dimnames(out) <- list(pairwNa,c("le","ri"))
  out }
   
