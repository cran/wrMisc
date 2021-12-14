#' Append vectors or lists, without duplcating common elements
#'
#' This function allows combining two vectors or lists without duplicating common content (definded by name of list-elements). 
#' 
#' @details When setting the argument \code{rmDuplicate=FALSE} the function will behave like \code{append}. 
#' 
#' @param x (vector or list) must have names to allow checking for duplicate names in y
#' @param y (vector or list) must have names to allow checking for duplicate names in x
#' @param rmDuplicate (logical) avoid duplicating liste-elements present in both x and y (based on names of list-elements)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return If both \code{x} and \code{y} are vectors, the output will be a vector, otherwise it will be a list
#' @seealso  \code{\link[base]{append}}; \code{\link{lrbind}}
#' @examples
#' li1 <- list(a=1, b=2, c=3)
#' li2 <- list(A=11, B=12, c=3)
#' appendNR(li1, li2)
#' append(li1, li2) 
#' @export
appendNR <- function(x, y, rmDuplicate=TRUE, silent=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="appendNR")
  if(!rmDuplicate | length(x) <1 | length(y) <1) { x <- append(x, y)
  } else {
    chY <- !names(y) %in% names(x)         #  names of y not in x (ie new)
    if(any(chY)) {
      xLe <- length(x)
      x <- append(x, y[which(chY)])
      if(!silent) message(fxNa," adding ",sum(chY)," new names/elements (",sum(!chY)," already present)")
      names(x)[xLe + seq(sum(chY))] <- names(y[which(chY)]) } }
  x }  
  
