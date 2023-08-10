#' Merge Named Vectors
#'
#' This function allows merging for multiple named vectors (each element needs to be named).
#' Basically, all elements carrying the same name across different input-vectors will be aligned in the same column of the output (input-vectors appear as lines).
#' If vectors are not given using a name (see first example below), they will be names 'x.1' etc (see argument \code{namePrefix}).
#' 
#' @details
#' Note : The arguments '\code{namePrefix}', '\code{NAto0}', '\code{callFrom}' and '\code{silent}' must be given with full name to be recognized as such (and not get considered as vector for merging).   
#' 
#' @param ... all vectors that need to be merged 
#' @param namePrefix (character) prefix to numers used when vectors are not given with explicit names (second exammple)
#' @param NAto0 (logical) optional replacemet of \code{NA}s by 0
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix of merged values
#' @seealso \code{\link[base]{merge}} (for two data.frames)
#' @examples
#' x1 <- c(a=1, b=11, c=21)
#' x2 <- c(b=12, c=22, a=2)
#' x3 <- c(a=3, d=43)
#' mergeVectors(vect1=x1, vect2=x2, vect3=x3)
#' x4 <- 41:44     # no names - not conform for merging
#' mergeVectors(x1, x2, x3, x4)
#' @export
mergeVectors <- function(..., namePrefix="x.", NAto0=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## merge for simple named vectors (each element needs to be named)
  fxNa <- .composeCallName(callFrom, newNa="mergeVectors")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  inpL <- list(...)
  chNa <- sapply(inpL, function(x) length(unique(names(x)))==length(x) && length(x) >0)
  if(any(!chNa)) {if(!silent) message(fxNa," Vectors must be longer than 0 and must have names on each element for merging; omit ",sum(!chNa)," (out of ",length(inpL),") vector(s)")
    inpL <- inpL[which(chNa)] }
  if(length(inpL)==1 && is.list(inpL[[1]])) inpL <- inpL[[1]]
  chNa <- names(inpL)
  if(length(names(inpL)) <1) { names(inpL) <- paste0(namePrefix,1:length(inpL))}
  if(length(inpL) >0) {
    if(isTRUE(NAto0)) inpL <- lapply(inpL, function(x) {chNa <- is.na(x); if(any(chNa)) x[which(chNa)] <- 0; x})
    spe <- sort(unique(unlist(lapply(inpL, names))))
    ta3 <- matrix(NA, nrow=length(inpL), ncol=length(spe), dimnames=list(names(inpL), spe))
    for(i in 1:length(inpL)) ta3[i, match(names(inpL[[i]]), spe)] <- inpL[[i]]
    ta3
  } else NULL }
      
