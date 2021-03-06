#' Html special character conversion
#'
#' Converts 'txt' so that (the most common) special characters (like 'beta','micro','square' etc) will be displayed correctly whe used for display in html (eg at mouse-over).
#' Note : The package \href{https://CRAN.R-project.org/package=stringi}{stringi} is required for the conversions (the input will get returned if \code{stringi} is not available).
#' @param txt character vector inclusing special characters
#' @param callFrom (character) allow easier tracking of message produced
#' @return corrected character vector adopted to html display
#' @seealso tables on \url{https://www.htmlhelp.com/reference/html40/entities/latin1.html},  \url{https://www.degraeve.com/reference/specialcharacters.php},  \url{https://ascii.cl/htmlcodes.htm}
#' @examples
#' (x <- stringi::stri_unescape_unicode("\\u00b5\\u003d\\u0061\\u0062"))
#' htmlSpecCharConv(x)
#' @export
htmlSpecCharConv <- function(txt, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="htmlSpecCharConv")
  chPa <- try(find.package("stringi"), silent=TRUE)
  if("try-error" %in% class(chPa)) { message(fxNa,"package 'stringi' not found ! Returning initial txt")
  } else {
    speCh <- c("\\u00b5","\\u00ba","\\u00b9","\\u00b2","\\u00b3",beta="\\u00df","\\u00e0", ced="\\u00e7",
      "\\u00e8","\\u00e9", ae="\\u00e2","\\u00f6","\\u00fc","\\u00f7","\\u00f5",x="\\u00d7")
    conv <- matrix(c(stringi::stri_unescape_unicode(speCh), "&micro","&ordm","&sup1","&sup2","&sup3","&szlig",          
       "&agrave","&ccedil","&egrave","&eacute","&auml", "&ouml","&uuml","&divide","&otilde","&times"),ncol=2) 
    conv <- rbind(conv,c('"','&quot'))
    for(i in 1:nrow(conv)) { che <- grep(conv[i,1],txt)
      if(length(che) >0) txt[che] <- gsub(conv[i,1],conv[i,2],txt[che])}}
  txt }

