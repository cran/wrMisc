#' Fast na.omit 
#'
#' This function removes NAs from input vector, in contrast to \code{na.omit} this function has no slot for removed elements. 
#' @details
#' Resulting objects from \code{naOmit} are smaller in size and subsequent code execution (on large vectors) may be faster (in particular if many NAs get encountered).
#' Note : This function behaves differently to \code{na.omit} with input other than plain vectors. Will not work with data.frames !
#' @param x (vector or matrix) data to check & remove \code{NA}s 
#' @param silent (logical) suppress messages if \code{TRUE}
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a vector without NAs (matrix input will be transformed to vector). Returns NULL if input consists only of NAs. 
#' @seealso \code{\link[stats]{na.fail}}, \code{na.omit}   
#' @examples
#' aA <- c(11:13,NA,10,NA); 
#' naOmit(aA) 
#' @export
naOmit <- function(x, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="naOmit")
  chNa <- is.na(x)
  if(all(chNa)) NULL else x[which(!chNa)] }
   
