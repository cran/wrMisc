#' Cut character-vector at multiple sites
#'
#' This function cuts character vector after 'cutAt' (ie keep the search subtsting 'cutAt', different to \code{strsplit}). 
#' Used for theoretical enzymatic digestion (eg in proteomics)
#' @param y character vector (better if of length=1, otherwise one won't know which fragment stems from which input)
#' @param cutAt (character) search subtsting, ie 'cutting rule'
#' @return modified (ie cut) character vector
#' @seealso \code{\link[base]{strsplit}}, \code{\link[wrMisc]{nFragments0}}, \code{\link[wrMisc]{nFragments}} 
#' @examples
#' tmp <- "MSVSRTMEDSCELDLVYVTERIIAVSFPSTANEENFRSNLREVAQMLKSKHGGNYLLFNLSERRPDITKLHAKVLEFGWPDLHTPALEKI"
#' cutAtMultSites(c(tmp,"ojioRij"),c("R","K"))
#' @export
cutAtMultSites <- function(y,cutAt) {
  for(i in cutAt) {y <- strsplit(y,i); y <- as.character(unlist(sapply(y,.addLetterWoLast,addChr=i)))}; y}

#' @export  
.addLetterWoLast <- function(x,addChr) paste(x,rep(c(addChr,""),c(length(x)-1,1)),sep="")            # add 'addChr' to all entries wo last entry  
  
