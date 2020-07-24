#' Correct mixed slash and backslash in file path
#'
#' @description
#' This function corrects paths character strings for mixed slash and backslash in file path.
#' In Windows the function \code{tempdir()} will use double backslashes as separator while \code{file.path()} uses regular slashes.
#' So when combining these two one might encounter a mix of slashes and double backslashes which may cause trouble, unless this is streightened out to a single separator used.
#' When pointig to given files inside html-files, paths need to have a prefix, this can be added using the argument \code{asHtml}.
#'	 
#' @param x (character) input path to test and correct
#' @param asHtml (logical) option for use in html : add prefix "file:/" 
#' @param anyPlatf (logical) if \code{TRUE}, checking will only be performed in Windows environement
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return character vector with corrected path
#' @seealso \code{\link[base]{tempfile}}, \code{\link[base]{file.path}}
#' @examples
#' path1 <- 'D:\\temp\\Rtmp6X8/working_dir\\RtmpKC/example.txt'
#' (path1b <- correctWinPath(path1, anyPlatf=TRUE)) 
#' (path1h <- correctWinPath(path1, anyPlatf=TRUE, asHtml=TRUE)) 
#' @export
correctWinPath <- function(x, asHtml=FALSE, anyPlatf=FALSE, silent=TRUE, callFrom=NULL) {
  ## correct mixed slash and backslash in file path
  fxNa <- .composeCallName(callFrom,newNa="correctWinPath")
  if(anyPlatf | length(grep("ming.32", R.Version()$platform)) >0) {
    x <- gsub("\\\\","/",x)      #"
    if(asHtml & length(grep("[[:upper:]]:", substr(x,1,2))) >0) {
      x <- paste("file:///",x,sep="") }
  } 
  if(asHtml & length(grep("^/",x)) >0) x <- paste("file:///", x, sep="")
  x } 
  
