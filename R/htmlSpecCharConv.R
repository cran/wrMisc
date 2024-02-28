#' Html special character conversion
#'
#' Converts 'txt' so that (the most common) special characters (like 'beta','micro','square' etc) will be displayed correctly whe used for display in html (eg at mouse-over).
#' Note : The package \href{https://CRAN.R-project.org/package=stringi}{stringi} is required for the conversions (the input will get returned if \code{stringi} is not available).
#' Currently only the 16 most common special characters are implemented.
#' 
#' @param txt character vector, including special characters
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a corrected character vector adopted for html display
#' @seealso tables on \url{https://www.htmlhelp.com/reference/html40/entities/latin1.html},  
#'   \url{https://www.degraeve.com/reference/specialcharacters.php}, or \code{https://ascii.cl/htmlcodes.htm}
#' @examples
#' ## we'll use the package stringi to generate text including the 'micro'-symbol as input
#' x <- if(requireNamespace("stringi", quietly=TRUE)) {
#'   stringi::stri_unescape_unicode("\\u00b5\\u003d\\u0061\\u0062")} else "\"x=axb\""
#' htmlSpecCharConv(x)
#' @export
htmlSpecCharConv <- function(txt, silent=FALSE, callFrom=NULL, debug=FALSE) {
  fxNa <- .composeCallName(callFrom, newNa="htmlSpecCharConv")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(!requireNamespace("stringi", quietly=TRUE)) {
    message(fxNa,"Package 'stringi' needed for conversion not found ! Please install from CRAN. Returning initial 'txt'")
  } else {
    speCh <- c("\\u00b5","\\u00ba","\\u00b9","\\u00b2","\\u00b3",beta="\\u00df","\\u00e0", ced="\\u00e7",
      "\\u00e8","\\u00e9", ae="\\u00e2","\\u00f6","\\u00fc","\\u00f7","\\u00f5",x="\\u00d7")
    conv <- try(stringi::stri_unescape_unicode(speCh), silent=TRUE) 
    if(inherits(conv, "try-error")) { warning(fxNa,": UNABLE to tun stringi::stri_unescape_unicode() !"); txt <- NULL
    } else {
      conv <- matrix(c(conv, "&micro","&ordm","&sup1","&sup2","&sup3","&szlig",          
        "&agrave","&ccedil","&egrave","&eacute","&auml", "&ouml","&uuml","&divide","&otilde","&times"), ncol=2)  
      conv <- rbind(conv, c('"','&quot'))
      for(i in 1:nrow(conv)) { che <- grep(conv[i,1], txt)
        if(length(che) >0) txt[che] <- gsub(conv[i,1], conv[i,2], txt[che]) } } }
  txt }

