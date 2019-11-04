#' Count number of non-numeric characters
#'
#' \code{nNonNumChar} counts number of non-numeric characters.
#' Made for positive non-scientific values (eg won't count neg-sign, neither Euro comma ',') 
#' @param txt character vector to be treated
#' @return numeric vector with  numer of non-numeric characters (ie not '.' or 0-9))
#' @seealso \code{\link[base]{nchar}}
#' @examples
#' nNonNumChar("a1b "); sapply(c("aa","12ab","a1b2","12","0.5"),nNonNumChar)
#' @export
nNonNumChar <- function(txt) {txt <- as.character(txt)
   sum(!sapply(1:nchar(txt),function(x) substr(txt,x,x) %in% c(".",0:9))) } 

#' @export
.extrNumHeadingCap <- function(x){
  ## extract number(s) before capital character
  tmp <- substr(x,regexpr("[[:digit:]]+[[:upper:]]",x),nchar(x))
  as.numeric(substr(tmp,0,regexpr("[[:upper:]]",tmp)-1)) }

#' @export
.extrNumHeadingSepChar <- function(x,sep="_"){
  ## extract number(s) before separator followed by alphabetic character (return named numeric vector, NAs when no numeric part found)
  tmp <- substr(x,1,attributes(regexpr(paste("[[:digit:]]+",sep,"[[:alpha:]]",sep=""),x))[[1]]-2)
  out <- as.numeric(tmp)
  if(!is.null(names(x))) names(out) <- names(x)
  out }
  
#' @export
.setLowestTo <- function(x,setTo) {
  ## set lowest value of x to value 'setTo'
  mi <- min(x[which(is.finite(x))]); x[which(x==mi)] <- setTo; x}
  
