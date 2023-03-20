#' Advanced paste-collapse
#'
#' This function is a variant of \code{\link[base]{paste}} for convenient use of paste-collapse and separation of last element to paste (via 'lastCol').
#' This function was mode for more human like enumeriating in output and messages.
#' If multiple arguments are given without names they will all be concatenated, if they contain names lazy evaluation for names will be tried
#' (with preference to longest match to argument names).
#' Note that some special characters (like backslash) may need to be protetected when used with 'collapse' or 'quoteC'.
#' Returns character vector of length 1 (everything pasted together)
#' @param ... (character) main input to be collapsed
#' @param collapse (character,length=1) element to use for collapsing
#' @param lastCol (character) text to use before last item enumerated element
#' @param quoteC character to use for citing with quotations (default "")
#' @return This function returns a character vector of truncated versions of intpup \code{txt}
#' @keywords character
#' @seealso  \code{\link[base]{paste}} for basic paste
#' @examples
#' pasteC(1:4)
#' @export
pasteC <- function(..., collapse=", ", lastCol=" and ", quoteC=""){
  inp <- list(...)
  chArgNa <- c("collapse","lastCol","quoteC")
  chNa0 <- lapply(chArgNa, function(x) rev(substring(x, 1, 2:nchar(x))))
  chNa <- lapply(chNa0, function(x) stats::na.omit(match(x, names(inp)))[1])                 # find location of match to longest variant of argument name
  if(any(!is.na(chNa[[1]]))) {collapse <- inp[[chNa[[1]]]]; inp <- inp[-chNa[[1]]]}
  if(any(!is.na(chNa[[2]]))) {lastCol <- inp[[chNa[[2]]]]; inp <- inp[-chNa[[2]]]}
  if(any(!is.na(chNa[[3]]))) {quoteC <- inp[[chNa[[3]]]]; inp <- inp[-chNa[[3]]]}
  .paQ <- function(x,quo) sapply(x, function(y) paste0(quo, y, quo))
  if(nchar(quoteC) >0) inp <- lapply(inp, .paQ, quoteC)
  if(length(unlist(inp)) >1) {
    out <- if(length(inp) >1) paste(unlist(inp[-length(inp)]), collapse=collapse) else NULL
    tmp <- inp[[length(inp)]]
    tmp <- list(tmp[-length(tmp)], tmp[length(tmp)])
    out <- paste0(paste(c(out, tmp[[1]]), collapse=collapse), lastCol, tmp[[2]])
  } else out <- as.character(unlist(inp))
  out }
  
#' Cut string to get all variants from given start with min and max length
#'
#' This function allows truncating character vector to all variants from given start, with min and optonal max length
#' Used to evaluate argument calls without giving full length of argument
#' 
#' @param txt (character) main input, may be length >1
#' @param startFr (interger) where to start
#' @param minLe (interger) minimum length of output
#' @param maxLe (interger) maximum length of output
#' @param reverse (logical) return longest text-fragments at beginning of vector
#' @return This function returns a character vector
#' @seealso used in \code{\link{pasteC}}; \code{\link[base]{substr}}
#' @examples
#' .cutStr("abcdefg", minLe=2)
#' @export
.cutStr <- function(txt, startFr=1, minLe=1, maxLe=NULL, reverse=TRUE){
  maxChar <- max(nchar(txt), na.rm=TRUE)
  if(length(startFr) >1) {startFr <- startFr[1]}
  if(startFr <1) startFr <- 1 
  if(length(minLe) >1) {minLe <- minLe[1]}
  if(length(maxLe) >0) if(any(is.na(maxLe))) maxLe <- NULL
  if(length(maxLe) >0) maxChar <- min(startFr +maxLe -1, maxChar)
  minStop <- startFr + minLe -1
  out <- if(maxChar - startFr +1 >= minLe && minLe <= minStop) unique(substring(txt, startFr, rep((minStop):maxChar, each=length(txt)))) else NULL 
  nChar <- nchar(out) >= minLe  
  if(any(!nChar)) out <- out[which(nChar)]
  if(isTRUE(reverse) && length(out) >1) out <- rev(out)
  out }


#' Cut string to get all variants from given start with min length, depreciated
#'
#' This function is depreciated, please use \code{/cutStr} instead !
#' This function allows truncating character vector to all variants from given start, with min and optonal max length
#' Used to evaluate argument calls without giving full length of argument
#' 
#' @param txt (character) main input, may be length >1
#' @param startFr (interger) where to start
#' @param minLe (interger) minimum length of output
#' @param reverse (logical) return longest text-fragments at beginning of vector
#' @return This function returns a character vector
#' @seealso \code{\link{pasteC}}; \code{\link[base]{substr}}
#' @examples
#' .seqCutStr("abcdefg", minLe=2)
#' @export
.seqCutStr <- function(txt, startFr=1, minLe=1, reverse=TRUE){
  fxNa <- ".seqCutStr" 
  maxChar <- max(nchar(txt), na.rm=TRUE)
  message(fxNa, "Depreciated function, please use .cutStr() instead  (also from package wrMisc)")
  if(length(startFr) >1) {startFr <- startFr[1]}
  out <- unique(substring(txt, 1, rep(max(1, startFr):maxChar, each=length(txt))))   # get all variants from start of min 3 to max 6 letters  (substr() will give only 1st)
  nChar <- nchar(out) >= minLe  
  if(any(!nChar)) out <- out[which(nChar)]
  if(isTRUE(reverse) && length(out) >1) out <- rev(out)
  out }
   

    
