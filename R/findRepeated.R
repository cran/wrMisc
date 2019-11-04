#' Find repeated elements
#'
#' \code{findRepeated} gets index of repeated items/values in vector 'x' (will be treated as character). 
#' Return (named) list of indexes for each of the repeated values, or \code{NULL} if all values are unique.
#' This approach is similar but more basic compared to \code{\link{get1stOfRepeatedByCol}}.
#' @param x character vector
#' @param nonRepeated (logical) if \code{=TRUE}, return list with elements \code{$rep} and \code{$nonrep}
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return (named) list of indexes for each of the repeated values, or NULL if all values unique
#' @seealso similar approach but more basic than \code{\link{get1stOfRepeatedByCol}}
#' @examples
#' aa <- c(11:16,14:12,14); findRepeated(aa) 
#' @export
findRepeated <- function(x, nonRepeated=FALSE,silent=FALSE,callFrom=NULL) {
  y <- as.character(x)          
  tab <- table(y)
  tab2 <- which(tab >1)
  if(length(tab2) >0){
    out <- lapply(names(tab)[tab2],function(z) which(y %in% z))               # slightly faster than which(!is.na(match(y,z))))
    names(out) <- x[sapply(out,utils::head,1)]                                # use initial value of repeated items as name
  } else out <- NULL
  if(nonRepeated) out <- list(rep=out,nonrep=match(names(tab)[which(tab <2)],y))
  out }
   
