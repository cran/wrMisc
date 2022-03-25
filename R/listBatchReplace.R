#' Replacements in list
#'
#' \code{listBatchReplace} replaces in list \code{lst} all entries with value \code{searchValue} by \code{replaceBy}
#' @param lst input-list to be used for replacing
#' @param searchValue (character, length=1) 
#' @param replaceBy (character, length=1)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns a corrected list
#' @seealso basic replacement \code{sub} in \code{\link[base]{grep}}
#' @examples
#' lst1 <- list(aa=1:4, bb=c("abc","efg","abhh","effge"), cc=c("abdc","efg"))
#' listBatchReplace(lst1, search="efg", repl="EFG", sil=FALSE)
#' @export
listBatchReplace <- function(lst,searchValue,replaceBy,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="listBatchReplace")
  msg1 <- " 'searchValue' and 'replaceBy' should be vectors"
  if(length(searchValue) <1 | length(replaceBy) <1) stop(fxNa,msg1)
  if(length(lst) <1 | !inherits(lst, "list")) stop(fxNa," 'lst' should be list with at least 1 element")
  outNa <- names(lst)
  if(length(searchValue) ==1 & length(replaceBy) ==1){
    out <- lapply(lst, function(x) {x[x==searchValue] <- replaceBy; x})
  } else {
    if(length(searchValue) ==length(replaceBy)) {
      out <- lapply(lst,function(x) {for(se in 1:length(searchValue)) x[x==searchValue[se]] <- replaceBy[se]; x})
    } else if(length(replaceBy) ==1) {
      out <- lapply(lst,function(x) {for(se in 1:length(searchValue)) x[x==searchValue[se]] <- replaceBy; x})
      } else { out <- lst
      if(!silent) message(fxNa," number of elments in 'searchValue' doesn't match well those from 'replaceBy'; doing nothing")}
  }
  names(out) <- outNa
  out }
    
