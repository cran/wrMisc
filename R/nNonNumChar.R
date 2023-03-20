#' Count number of non-numeric characters
#'
#' \code{nNonNumChar} counts number of non-numeric characters.
#' Made for positive non-scientific values (eg won't count neg-sign, neither Euro comma ',') 
#' @param txt character vector to be treated
#' @return This function returns a numeric vector with  numer of non-numeric characters (ie not '.' or 0-9))
#' @seealso \code{\link[base]{nchar}}
#' @examples
#' nNonNumChar("a1b "); sapply(c("aa","12ab","a1b2","12","0.5"), nNonNumChar)
#' @export
nNonNumChar <- function(txt) {txt <- as.character(txt)
   sum(!sapply(1:nchar(txt), function(x) substr(txt, x, x) %in% c(".",0:9))) } 

#' Extract number(s) before capital character
#'
#' This function aims to extract number(s) before capital character
#' 
#' @param x character vector to be treated
#' @return This function returns a numeric vector
#' @seealso  \code{\link[base]{grep}}, \code{\link[base]{nchar}}
#' @examples
#' .extrNumHeadingCap(" 1B ")
#' @export
.extrNumHeadingCap <- function(x){
  ## extract number(s) before capital character
  tmp <- substr(x, regexpr("[[:digit:]]+[[:upper:]]",x), nchar(x))
  as.numeric(substr(tmp, 0, regexpr("[[:upper:]]",tmp) -1)) }

#' Extract numbers before separator followed by alphabetic character
#'
#' This function aims to extract number(s) before separator followed by alphabetic character (return named numeric vector, NAs when no numeric part found)
#' 
#' @param x character vector to be treated
#' @param sep (character) separator
#' @return This function returns a numeric vector 
#' @seealso \code{\link[base]{nchar}}
#' @examples
#' .extrNumHeadingSepChar(" 1B ")
#' @export
.extrNumHeadingSepChar <- function(x, sep="_"){
  ## extract number(s) before separator followed by alphabetic character (return named numeric vector, NAs when no numeric part found)
  tmp <- substr(x, 1, attributes(regexpr(paste("[[:digit:]]+",sep,"[[:alpha:]]",sep=""),x))[[1]] -2)
  out <- as.numeric(tmp)
  if(!is.null(names(x))) names(out) <- names(x)
  out }
  
#' Set lowest value to given value 
#'
#' This function aims to set lowest value of x to value 'setTo'
#' 
#' @param x (numeric) main vector to be treated
#' @param setTo (numeric) replacement value
#' @return This function returns a numeric vector 
#' @seealso \code{\link[base]{nchar}}
#' @examples
#' .setLowestTo(9:4, 6)
#' @export
.setLowestTo <- function(x, setTo) {
  ## set lowest value of x to value 'setTo'
  mi <- min(x[which(is.finite(x))], na.rm=TRUE); x[which(x==mi)] <- setTo; x}
  
