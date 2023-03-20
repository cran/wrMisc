#' Find first of repeated elements
#'
#' This function works similar to \code{unique}, but provides additional information about which elements of original input \code{'x'} are repeatd by providing indexes realtoe to the input.
#' \code{firstOfRepeated} makes list with 3 elements : $indRepeated.. index for first of repeated 'x', $indUniq.. index of all unique + first of repeated, $indRedund.. index of all redundant entries, ie non-unique (wo 1st).
#' Used for reducing data to non-redundant status, however, for large numeric input the function nonAmbiguousNum() may perform better/faster.
#' NAs won't be considered (NAs do not appear in reported  index of results), see also firstOfRepLines() .
#' @param x (charcter or numeric) main input
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param debug (logical) display additional messages for debugging
#' @return list with indices: $indRepeated,  $indUniq, $indRedund
#' @seealso \code{\link[base]{duplicated}}, \code{\link{nonAmbiguousNum}}, \code{\link{firstOfRepLines}} gives less detail in output (lines/elements/indexes of omitted not directly accessible) and works fsster
#' @examples
#' x <- c(letters[c(3,2:4,8,NA,3:1,NA,5:4)]); names(x) <- 100+(1:length(x))
#' firstOfRepeated(x)
#' x[firstOfRepeated(x)$indUniq]          # only unique with names
#' @export  
firstOfRepeated <- function(x, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="firstOfRepeated")
  chNA <- is.na(x)
  dupH <- duplicated(x, fromLast=FALSE)
  dupL <- duplicated(x, fromLast=TRUE)
  out <- list()
  out$indRepeated <- which(!dupH & dupL & !chNA)
  names(out$indRepeated) <- x[out$indRepeated]
  out$indUniq <- which(!dupH & !chNA)
  out$indRedund <- which(dupH | chNA)
  out }
  
