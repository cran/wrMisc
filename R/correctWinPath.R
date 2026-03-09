#' Correct Mixed Slash And Backslash In File-Path
#'
#' @description
#' This function corrects paths character strings for mixed slash and backslash in file path.
#' In Windows the function \code{tempdir()} will use double backslashes as separator while \code{file.path()} uses regular slashes.
#' So when combining these two one might encounter a mix of slashes and double backslashes which may cause trouble, unless this is streightened out to a single separator used.
#' When pointig to given files inside html-files, paths need to have a prefix, this can be added using the argument \code{asHtml}.
#'	 
#' @param x (character) input path to test and correct
#' @param asHtml (logical) option for use in html : add prefix "file:/" 
#' @param anyPlatf (logical, length=1) if \code{TRUE}, checking will only be performed in Windows environement
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a character vector with corrected path
#' @seealso \code{\link[base]{tempfile}}, \code{\link[base]{file.path}}
#' @examples
#' path1 <- 'D:\\temp\\Rtmp6X8/working_dir\\RtmpKC/example.txt'
#' (path1b <- correctWinPath(path1, anyPlatf=TRUE)) 
#' (path1h <- correctWinPath(path1, anyPlatf=TRUE, asHtml=TRUE)) 
#' @export
correctWinPath <- function(x, asHtml=FALSE, anyPlatf=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## correct mixed slash and backslash in file path
  fxNa <- .composeCallName(callFrom,newNa="correctWinPath")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(anyPlatf || length(grep("ming.32", R.Version()$platform)) >0) {
    x <- gsub("\\\\","/",x)      #"
    if(asHtml && length(grep("[[:upper:]]:", substr(x,1,2))) >0) {
      x <- paste0("file:///",x) }
  } 
  if(asHtml && length(grep("^/",x)) >0) x <- paste0("file:///", x,)
  x } 
  
