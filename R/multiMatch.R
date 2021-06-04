#' Simple Multi-to-Multi Matching of (Concatenated) Terms
#'
#' This function allows convenient matching of multi-to-multi relationships between two objects/vectors. 
#' It was designed for finding common elements in multiple to multiple matching situations (eg when comparing \code{c("aa; bb", "cc")}  to \code{c("bb; ab","dd")}, 
#' ie to find 'bb' as matching between both objects).
#'  
#'  
#' @param x (vector or list) first object to compare; if vector, the (partially) concatenated identifyers (will be split using separator \code{sep}), or list of items to be matched (ie already split) 
#' @param y (vector or list) second object to compare; if vector, the (partially) concatenated identifyers (will be split using separator \code{sep}), or list of items to be matched (ie already split) 
#' @param sep (character, length=1) separator used to split concatenated identifyers (if \code{x} or  \code{y} is vector)
#' @param sep2 (character, length=1) optional separator used when \code{method="matched"} to concatenate all indexes of \code{y} for column \code{y.allInd}
#' @param method (character) mode of operation: 'asIndex' to return index of y (those hwo have matches) with names of x (which x are the correpsonding match)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#'
#' @details
#' \code{method='asIndex'} .. returns list (length matches x) where values indicate index of which y is matched by which elment of x (after split). Most concise, but not human reader friendly output.
#'                                                                                                                                                       
#' \code{method='matchedL'} .. returns list with IDs (from split of 'x') from x that have been matched to y, names indicate in which y they have been found.
#'                                                                                                                                                       
#' \code{method='matched'} .. returns data.frame for all matches in \code{x} with columns:  \code{$matchItem} (not necessarily unique),  \code{$x.ind} (index of x where item has been found),  \code{$x} (value of x), \code{$y.firInd} (first index of y with matched item), \code{y.allInd} (all indexes of y with matched item), \code{y.fir} (full content of first y matched to item).
#'                                                                                                                                                       
#' \code{method='adjustXtoY'} .. returns vector with \code{x} adjusted to \code{y}, ie those elements of  \code{x} matching are replace by the exact corresponding term of  \code{y}.
#'                                                                                                                                                       
#' If no term matching the terms shown above is given, another version of 'asIndex' is returned, but indexes to \code{y} _after_ spliting by \code{sep}. 
#'                                                                                                                                                       
#' @return list or data.frame (depending on \code{method} chosed)
#' @seealso \code{\link[base]{match}}; \code{\link[base]{strsplit}}
#' @examples
#' aa <- c("m","k","j; aa","m; aa; bb; o","n; dd","aa","cc")
#' bb <- c("aa","dd; r","aa; bb; q","p; cc") 
#' (aOnB <- multiMatch(aa, bb))                        # match aa on bb
#' (aOnB <- multiMatch(aa, bb, method="matchedL"))     # match aa on bb
#' (aOnB <- multiMatch(aa, bb, method="asIndex"))      # match aa on bb
#' 
#' @export
multiMatch <- function(x, y, sep="; ", sep2=NULL, method="matched", silent=FALSE, callFrom=NULL) {
  ## for finding common in multiple to multiple matching (eg c("aa; bb", "cc")  vs c("bb; ab","dd"))
  ## tells which x (names of output) was found matching (at least by 1 subunit) to which (part of) y, names of y tell which x (index)
  ## note: multiple matches are ignored (only 1st match reported)
  fxNa <- .composeCallName(callFrom, newNa="multiMatch")
  doMa <- TRUE
  if(length(x) <1) { doMa <- FALSE; if(!silent) message(fxNa,"argument 'x' is empty, nothing to do !")}
  if(length(y) <1) { doMa <- FALSE; if(!silent) message(fxNa,"argument 'y' is empty, nothing to do !")}
  out <- NULL
  if(doMa) {
    if(is.list(x)) {xL <- x; x <- sapply(x, paste, collapse=sep)} else xL <- strsplit(x, sep)
    if(is.list(y)) {yL <- y; y <- sapply(y, paste, collapse=sep)} else yL <- strsplit(y, sep)
    names(xL) <- 1:length(x)
    yV <- unlist(yL)
    names(yV) <- rep(1:length(y), sapply(yL,length))
    
    ## 'asIndex' .. return list (length matches x) where values indicate index of which y is matched by which elment of x (after split)
    out <- lapply(xL, function(z) {w <- match(yV,z); v <- naOmit(w); if(length(v) >0) { w <- which(!is.na(w)); names(w) <- v; w}})
    if(length(out) >0) out <- out[which(sapply(out,length) >0)]
        
    ## 'matchedL' .. return list with IDs (from split of 'x') from x that have been matched to y, names indicate in which y they have been found
    if(any(sapply(c("asIndex","adjustXtoY"), identical,method)) & length(out) >0) {   # replace index to yV by index to y
      xI <- rep(1:length(yL), sapply(yL, length))[unlist(out, use.names=FALSE)]
      names(xI) <- unlist(lapply(out, function(z) names(z)), use.names=FALSE)
      outN <- names(out)
      out <- tapply(xI, rep(1:length(out), sapply(out,length)), function(z) z) 
      names(out) <- outN
    }
    if(any(sapply(c("matched","matchedL"), identical,method))) out <- lapply(xL, function(z) {
      w <- match(yV,z); v <- naOmit(w); if(length(v) >0) { u <- z[v]; names(u) <- names(yV)[which(!is.na(w))]; u}}) 

    if(identical(method,"matched") & length(out) >0) { if(length(sep2) !=1) sep2 <- sep
      out <- out[which(sapply(out,length) >0)]
      matchItem=as.character(sapply(out, function(z) if(TRUE) z[1] else paste(z,collapse=sep) ))
      out <- data.frame(matchItem=matchItem, x.ind=as.integer(names(out)), x=x[as.integer(names(out))], 
        y.firInd=sapply(out, function(z) as.integer(names(z)[1])), y.allInd <- as.character(sapply(out, function(z) paste(names(z),collapse=sep2) )) ) 
      out <- cbind(out[,1:4], y.allInd=out[,5], y=y[as.integer(out[,4])] )
    }
    if(identical(method,"adjustXtoY") & length(out) >0) { xA <- x
      xA[as.integer(names(out))] <- y[as.integer(sapply(out, function(z) as.integer(z[1])))]
      out <- xA; rm(xA) 
    }
  } 
  out }
  
