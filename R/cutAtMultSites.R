#' Cut Character-Vector At Multiple Sites
#'
#' This function cuts character vector after 'cutAt' (ie keep the search subtsting 'cutAt', different to \code{strsplit}). 
#' Used for theoretical enzymatic digestion (eg in proteomics)
#' 
#' @param y character vector (better if of length=1, otherwise one won't know which fragment stems from which input)
#' @param cutAt (character) search subtsting, ie 'cutting rule'
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced 
#' @return This function returns a modified (ie cut) character vector
#' @seealso \code{\link[base]{strsplit}}, \code{\link{nFragments0}}, \code{\link{nFragments}} 
#' @examples
#' tmp <- "MSVSRTMEDSCELDLVYVTERIIAVSFPSTANEENFRSNLREVAQMLKSKHGGNYLLFNLSERRPDITKLHAKVLEFGWPDLHTPALEKI"
#' cutAtMultSites(c(tmp,"ojioRij"),c("R","K"))
#' @export
cutAtMultSites <- function(y, cutAt, silent=FALSE, debug=FALSE, callFrom=NULL) {
  for(i in cutAt) {y <- strsplit(y,i); y <- as.character(unlist(sapply(y, .addLetterWoLast, addChr=i)))}; y}

#' Add letter to all elements but not last
#'
#' This function allows to add 'addChr' to all entries, without the last entry
#' 
#' @param x (character) main input
#' @param addChr (character)
#' @return This function returns a modified character vector
#' @seealso  \code{\link[base]{paste}}; used in \code{\link{cutAtMultSites}}
#' @examples
#' .addLetterWoLast(c("abc","efgh"),"Z")
#' @export  
.addLetterWoLast <- function(x, addChr) paste0(x,rep(c(addChr,""),c(length(x)-1,1)))            # add 'addChr' to all entries wo last entry  
  
