#' Html special character conversion
#'
#' Converts 'txt' so that special characters (like 'beta','micro','square' etc) will be displayed correctly whe used for display in html (eg at mouse-over).
#' Note : uses package \href{https://CRAN.R-project.org/package=stringi}{stringi} 
#' @param txt character vector inclusing special characters
#' @return corrected character vector adopted to html display
#' @seealso tables on \url{http://www.htmlhelp.com/reference/html40/entities/latin1.html},  \url{http://www.degraeve.com/reference/specialcharacters.php},  \url{http://www.ascii.cl/htmlcodes.htm}
#' @examples
#' (x <- stringi::stri_unescape_unicode("\\u00b5\\u003d\\u0061\\u0062"))
#' htmlSpecCharConv(x)
#' @export
htmlSpecCharConv <- function(txt) {
  speCh <- c("\\u00b5","\\u00ba","\\u00b9","\\u00b2","\\u00b3",beta="\\u00df","\\u00e0", ced="\\u00e7",
    "\\u00e8","\\u00e9", ae="\\u00e2","\\u00f6","\\u00fc","\\u00f7","\\u00f5",x="\\u00d7")
  conv <- matrix(c(stringi::stri_unescape_unicode(speCh), "&micro","&ordm","&sup1","&sup2","&sup3","&szlig",          
     "&agrave","&ccedil","&egrave","&eacute","&auml", "&ouml","&uuml","&divide","&otilde","&times"),ncol=2) 
  conv <- rbind(conv,c('"','&quot'))
  for(i in 1:nrow(conv)) { che <- grep(conv[i,1],txt)
    if(length(che) >0) txt[che] <- gsub(conv[i,1],conv[i,2],txt[che])}
  txt }

