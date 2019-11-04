#' make numeric vector non-ambiguous (ie unique)
#'
#' \code{nonAmbiguousNum} makes (named) values of numeric vector 'x' unique. 
#' Note: for non-numeric use  \code{\link{firstOfRepeated}} - but 1000x slower !
#' Return sorted non-ambigous numeric vector (or list if 'asList'=TRUE and 'uniqOnly'=FASLSE)
#' @param x (numeric) main input
#' @param uniqOnly (logical) if=TRUE return unique only, if =FALSE return unique and single representative of non-unique values (with '' added to name), selection of representative of repeated: first (of sorted) or middle if >2 instances
#' @param asList (logical) return list
#' @param nameMod (character) text to add in case on ambiguous values, default="amb_"
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return sorted non-ambigous numeric vector (or list if 'asList'=TRUE and 'uniqOnly'=FALSE)
#' @seealso \code{\link{firstOfRepeated}} for non-numeric use (much slower !!!), \code{\link[base]{duplicated}} 
#' @examples
#' set.seed(2017); aa <- round(rnorm(100),2); names(aa) <- 1:length(aa)
#' str(nonAmbiguousNum(aa))
#' str(nonAmbiguousNum(aa,uniq=FALSE,asLi=TRUE))
#' @export
nonAmbiguousNum <- function(x,uniqOnly=FALSE,asList=FALSE,nameMod="amb_",callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="nonAmbiguousNum")
  y <- sort(x)
  ab <- which(diff(y)==0)
  if(length(ab) <1) return(if(asList) list(unique=x) else x) else {
    ac <- diff(ab)
    ac <- ab[c(which(ac >1),if(ac[length(ac)] >1) length(ab) else NULL)]   # index : single instance of repeated
    red <- y[ac]
    names(red) <- paste(nameMod,names(red),sep="")
    out <- y[-1*unique(sort(c(ab,ab+1)))]
    if(!uniqOnly) out <- if(asList) list(unique=out,ambig=red) else c(out,red)
    out} }
     
