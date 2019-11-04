#' Count same start- and end- sites of edges (or fragments)
#'
#' Suppose a parent sequence/string 'ABCDE' gets cut in various fragments (eg 'ABC','AB' ...).   
#' \code{countSameStartEnd} counts how many (ie re-occuring) start- and end- sites of edges do occur in the input-data. 
#' The input is presented as matrix of/indicating start- and end-sites of edges.
#' The function is used to characterize partially redundant edges and accumulation of cutting/breakage sites.
#' @param frag (matrix) 1st column \code{beg} start-sites, 2nd column \code{end} end-sites of edges, rownames to precise fragment identities are recommended  
#' @param minFreq (integer) min number of accumulated sites for taking into account (allows filtering with large datasets)  
#' @param nDig (integer) rounding: number of digits for columns \code{beg.rat} and \code{end.rat} in output
#' @return matrix of 6 columns: input (beg and end), beg.n, beg.rat, end.n, end.rat
#' @seealso to build initial tree \code{\link{buildTree}}, \code{\link{contribToContigPerFrag}}, \code{\link{simpleFragFig}}
#' @examples
#' frag1 <- cbind(beg=c(2,3,7,13,13,15,7,9,7, 3,3,5), end=c(6,12,8,18,20,20,19,12,12, 4,5,7))
#' rownames(frag1) <- letters[1:nrow(frag1)]
#' countSameStartEnd(frag1)
#' simpleFragFig(frag1)
#' @export
countSameStartEnd <- function(frag,minFreq=2,nDig=4) {
  if(!is.matrix(frag)) frag <- as.matrix(frag)
  if(is.null(rownames(frag))) rownames(frag) <- paste("li",1:nrow(frag),sep="") else {
    frag <- frag[sort.list(rownames(frag)),]}
  chTab <- function(tab) {chT <- tab > minFreq-1; if(any(chT)) tab[which(chT)] else NULL}
  headT <- chTab(table(frag[,1]))
  tailT <- chTab(table(frag[,2]))
  out <- matrix(nrow=nrow(frag),ncol=2)
  if(length(headT) >0) { tmp <- which(frag[,1] %in% names(headT)); out[tmp,1] <- as.integer(as.character(factor(frag[tmp,1],labels=headT)))}
  if(length(tailT) >0) { tmp <- which(frag[,2] %in% names(tailT)); out[tmp,2] <- as.integer(as.character(factor(frag[tmp,2],labels=tailT)))}
  cbind(frag,beg.n=out[,1],beg.rat=signif(out[,1]/nrow(frag),digits=nDig), end.n=out[,2],end.rat=signif(out[,2]/nrow(frag),digits=nDig))
}
  
