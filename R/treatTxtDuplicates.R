#' Locate duplicates in text and make non-redundant
#'
#' \code{treatTxtDuplicates} locates duplictes in character-vector 'x' and return list (length=3) : with $init (initial),
#'   $nRed .. non-redundant text by adding number at end or beginning, and $nrLst .. list-version with indexes per unique entry. 
#'  Note : NAs (if multiple) will be renamed to NA_1, NA_2
#' @param x (character) vector with character-entries to identify (and remove) duplicates
#' @param atEnd (logical) decide location of placing the counter (at end or at beginning of ID) (see \code{\link{correctToUnique}})
#' @param sep (character) separator to add before counter when making non-redundant version
#' @param onlyCorrectToUnique (logical) if TRUE, return only vector of non-redundant 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return list with $init, $nRed, $nrLst
#' @seealso For simple correction use \code{\link{correctToUnique}}
#' @examples
#' treatTxtDuplicates(c("li0",NA,rep(c("li2","li3"),2)))
#' correctToUnique(c("li0",NA,rep(c("li2","li3"),2)))
#' @export
treatTxtDuplicates <- function(x,atEnd=TRUE,sep="_",onlyCorrectToUnique=FALSE,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="treatTxtDuplicates")
  if(length(dim(x)) >1) { if(!silent) message(fxNa," expecting simple (text) vector as 'x' but got class ",class(x))
    x <- if(is.list(x)) unlist(x) else as.character(x) }
  xIni <- x
  if(any(is.na(x))) x[which(is.na(x))] <- "NA"
  out <- correctToUnique(x,sep=sep,atEnd=atEnd)
  if(!onlyCorrectToUnique) {
    anyDu <- base::duplicated(x,fromL=FALSE) | base::duplicated(x,fromL=TRUE)
    if(any(anyDu)) { nrLst <- sapply(unique(x),function(z) which(x %in% z))
      if(!is.list(nrLst)) nrLst <- as.list(as.data.frame(nrLst))
      } else {nrLst <- as.list(1:length(x)); names(nrLst) <- x}
  list(init=xIni,nRed=out,nrLst=nrLst)} else out }
       
