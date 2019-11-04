#' Number of fragments after cut at specific character(s)
#'
#' \code{nFragments0} tells the number of fragments/entry when cutting after 'cutAt' 
#' @param protSeq (character) text to be cut
#' @param cutAt (integer) position to cut
#' @return numeric vector with number of fragments for each entry 'protSeq' (names are 'protSeq')
#' @seealso  more elaborate \code{{nFragments}}; \code{\link{cutAtMultSites}}
#' @examples
#' tmp <- "MSVSRTMEDSCELDLVYVTERIIAVSFPSTANEENFRSNLREVAQMLKSKHGGNYLLFNLSERRPDITKLHAKVLEFGWPDLHTPALEKI" 
#' nFragments0(c(tmp,"ojioRij"),c("R","K"))
#' @export
nFragments0 <- function(protSeq,cutAt) {sapply(protSeq,function(x) length(cutAtMultSites(x,cutAt)))}

#' Number of fragments after cut at specific character(s) within size-range
#'
#' \code{nFragments} determines number of fragments /entry within range of 'sizeRa' (numeric,length=2) when cutting after 'cutAt' 
#' @param protSeq (character)  text to be cut
#' @param cutAt (character) position to cut
#' @param sizeRa (numeric,length=2) min and max size to consider
#' @return numeric vector with number of fragments for each entry 'protSeq' (names are 'protSeq')
#' @seealso \code{\link{cutAtMultSites}}, simple version \code{{nFragments0}} (no size-range) 
#' @examples
#' tmp <- "MSVSREDSCELDLVYVTERIIAVSFPSTANEENFRSNLREVAQMLKSKHGGNYLLFNLSERRPDITKLHAKVLEFGWPDLHTPALEKI"
#' nFragments(c(tmp,"ojioRij"),c("R","K"),c(4,31))
#' 
#' @export
nFragments <- function(protSeq,cutAt,sizeRa) {   # number of fragments /entry within range of 'sizeRa' (numeric,length=2) when cutting after 'cutAt' 
  if(length(naOmit(unique(sizeRa))) <2) stop(" 'sizeRa' should be numeric of length=2")
  if(length(cutAt) <0) rep(1,length(protSeq)) else {
    sapply(protSeq,function(x) {y <- cutAtMultSites(x,cutAt)
    sum(nchar(y) > min(sizeRa,na.rm=TRUE) & nchar(y) < max(sizeRa,na.rm=TRUE),na.rm=TRUE)})}}
    
