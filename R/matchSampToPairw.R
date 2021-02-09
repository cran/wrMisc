#' Match names to concatenated pairs of names   
#'
#' @description
#' The column-names of multiple pairwise testing contain the names of the initial groups/conditions tested, plus there is a separator (eg '-' in \code{moderTestXgrp}).  
#' Thus function allows to map back which groups/conditions were used by returning the index of the respective groups used in pair-wise sets. 
#' 
#' @details 
#' The names of initial groups/conditions (\code{grpNa}) will be tested for exact pattern matching either at beginning or at end of pair-wise names (\code{pairwNa}).
#' If there is no 'perfect' fit, the names concerned will not get matched.
#' Thus, it is not necessary to know what character(s) were used as separator when constructing the concatenated pair-wise names.
#' If some \code{grpNa} are not found in \code{pairwNa} this will be marked as NA.  
#'	 
#' @param grpNa (character) the names of the groups of replicates (ie conditions) used to test
#' @param pairwNa (character) the names of pairwise-testing (ie 'concatenated' \code{sampNa}  
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return matrix of 2 columns with inidices of \code{sampNa} with \code{pairwNa} as rows
#' @seealso (for running multiple pair-wise test) \code{\link{moderTestXgrp}}
#' @examples
#' pairwNa1 <- c("abc-efg","abc-hij","efg-hij")
#' grpNa1 <- c("hij","abc","efg","klm")
#' matchSampToPairw(grpNa1, pairwNa1) 
#' 
#' pairwNa2 <- c("abc-efg","abc-hij","zz-zz","efg-hij")
#' matchSampToPairw(grpNa1, pairwNa2) 
#' @export
matchSampToPairw <- function(grpNa, pairwNa, silent=FALSE, callFrom=NULL) {
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
  ## main
  le <- apply(sapply(pairwNa, function(x) sapply(grpNa, function(y) length(grep(paste0("^",y),x)) >0)), 2, which)
  ri <- apply(sapply(pairwNa, function(x) sapply(grpNa, function(y) length(grep(paste0(y,"$"),x)) >0)), 2, which)
  if(is.list(le)) {le[[which(sapply(le,length) <1)]] <- NA; le <- unlist(le)}
  if(is.list(ri)) {ri[[which(sapply(ri,length) <1)]] <- NA; ri <- unlist(ri)}
  out <- cbind(le,ri) 
  if(length(pairwNa)==nrow(out)) names(out) <- pairwNa
  out }
   

