#' Value Matching with optional reversing of sub-parts of non-matching elements
#'
#' This function provides a variant to \code{\link[base]{match}}, where initially non-matching elements of \code{x}  
#' will be tested by decomposing non-matching elements, reversing the parts in front and after the separator \code{sep} and re-matching.
#' If separator \code{sep} does not occur, a warning will be issued, if it occurs more than once, 
#' the parts before and after the first separartor will be used and a warning issued.
#'
#' @param x (character) first vector for match 
#' @param y (character) second vector for match
#' @param sep (character) separator between elements
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return index for matching (integer) x to y
#' @seealso \code{\link[base]{match}} 
#' @examples
#' tx1 <- c("a-b","a-c","d-a","d-b","b-c","d-c")
#' tmp <- triCoord(4)
#' tx2 <- paste(letters[tmp[,1]],letters[tmp[,2]],sep="-")
#' ## Some matches won't be found, since 'a-d' got reversed to 'd-a', etc... 
#' match(tx1,tx1)  
#' matchNamesWithReverseParts(tx1,tx2)
#' @export
matchNamesWithReverseParts <- function(x,y,sep="-",silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="matchNamesWithReverseParts")
  if(length(x) <1) stop(" Nothing to do, 'x' seems empty")
  if(length(y) <1) stop(" Nothing to do, 'y' seems empty")
  ma1 <- match(x,y)
  if(any(is.na(ma1))) {
    sup <- which(is.na(ma1))
    x2 <- strsplit(x[sup],sep)
    chX <- sapply(x2,length)
    if(any(chX <2)& !silent) message(fxNa," Can't find separator ('",sep,"') in  ",sum(chX <2)," elements of 'x'")
    if(any(chX >2) & !silent) message(fxNa," BEWARE, results may be different to expected !  Separator ('",
      sep,"') occurs more than once in ",sum(chX >2)," elements of 'x'")
    x2 <- sapply(x2,function(z) if(length(z) >1) paste(z[2],z[1],sep=sep) else z)
    x[sup] <- x2
    ma1 <- match(x,y) } 
  ma1 }
