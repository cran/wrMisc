#' Advanced paste-collapse
#'
#' \code{pasteC} is a variant of \code{\link[base]{paste}} for convenient use of paste-collapse and separation of last element to paste (via 'lastCol').
#' This function was mode for more human like enumeriating in output and messages.
#' If multiple arguments are given without names they will all be concatenated, if they contain names lazy evaluation for names will be tried
#' (with preference to longest match to argument names).
#' Note that some special characters (like backslash) may need to be protetected when used with 'collapse' or 'quoteC'.
#' Returns character vector of length 1 (everything pasted together)
#' @param ... (character) main input to be collapsed
#' @param collapse (character,length=1) element to use for collapsing
#' @param lastCol (character) text to use before last item enumerated element
#' @param quoteC character to use for citing with quotations (default "")
#' @return character vector of length=1 of the concatenated input/values.
#' @keywords character
#' @seealso  \code{\link[base]{paste}} for basic paste
#' @examples
#' pasteC(1:4)
#' @export
pasteC <- function(...,collapse=", ",lastCol=" and ",quoteC=""){
  inp <- list(...)
  chNa0 <- list(.seqCutStr("collapse",2),.seqCutStr("lastCol",2),.seqCutStr("quoteC",2))   # variants of argument names recognized
  chNa <- lapply(chNa0,function(x) stats::na.omit(match(x,names(inp)))[1])                 # find location of match to longest variant of argument name
  if(any(!is.na(chNa[[1]]))) {collapse <- inp[[chNa[[1]]]]; inp <- inp[-chNa[[1]]]}
  if(any(!is.na(chNa[[2]]))) {lastCol <- inp[[chNa[[2]]]]; inp <- inp[-chNa[[2]]]}
  if(any(!is.na(chNa[[3]]))) {quoteC <- inp[[chNa[[3]]]]; inp <- inp[-chNa[[3]]]}
  .paQ <- function(x,quo) sapply(x,function(y) paste(quo,y,quo,sep=""))
  if(nchar(quoteC) >0) inp <- lapply(inp,.paQ,quoteC)
  if(length(unlist(inp)) >1) {
    out <- if(length(inp) >1) paste(unlist(inp[-length(inp)]),collapse=collapse) else NULL
    tmp <- inp[[length(inp)]]
    tmp <- list(tmp[-length(tmp)],tmp[length(tmp)])
    out <- paste(paste(c(out,tmp[[1]]),collapse=collapse),lastCol,tmp[[2]],sep="")
  } else out <- as.character(unlist(inp))
  out }
  
#' @export
.seqCutStr <- function(txt,startFr=1,reverse=TRUE){
  ## sequential cutting of character string (length=1 !!) starting from position 'startFr'
  ## use eg for variants of argument calling with lazy evaluation
  nCh <- nchar(txt)
  out <- if(startFr > nCh) NULL else sapply(startFr:nCh,function(x) substr(txt,1,x))
  if(reverse & length(out) >1) out <- rev(out)
  out }
   
