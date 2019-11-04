#' Fast na.omit 
#'
#' \code{naOmit} removes NAs from input vector. This function has no slot for removed elements while \code{na.omit} does so. 
#' Resulting objects from \code{naOmit} are smaller in size and subsequent execution (on large vectors) is faster (in particular if many NAs get encountered).
#' Note : Behaves differently to \code{na.omit} with input other than plain vectors. Will not work with data.frames !
#' @param x (vector or matrix) input 
#' @return vector without NAs (matrix input will be transformed to vector). Returns NULL if input consists only of NAs. 
#' @seealso \code{\link[stats]{na.fail}}, \code{na.omit}   
#' @examples
#' aA <- c(11:13,NA,10,NA); 
#' naOmit(aA) 
#' @export
naOmit <- function(x) {chNa <- is.na(x); if(all(chNa)) NULL else x[which(!chNa)]}
   
