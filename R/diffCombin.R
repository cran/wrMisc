#' Compute Matrix Of Differences For All Pairwise Combinations Of Numeric Vector
#'
#' \code{diffCombin} returns matrix of differences (eg resulting from subsititution) for all pairwise combinations of numeric vector 'x'.
#'
#' @param x numeric vector to compute differences for all combinations
#' @param diagAsNA (logical) return all self-self combinations as NA (otherwise 0)
#' @param prefix (logical) if TRUE, dimnames of output will specify orientation (prefix='from.' and 'to.')
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a numeric matrix of all pairwise differences
#' @seealso \code{\link[base]{diff}} for simple differences
#' @examples
#' diffCombin(c(10,11.1,13.3,16.6))
#' @export
diffCombin <- function(x, diagAsNA=FALSE, prefix=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="diffCombin")
  fxNa <- .composeCallName(callFrom, newNa="combineOverlapInfo")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(!is.numeric(x)) x <- convToNum(x, remove=NULL, sciIncl=TRUE, callFrom=fxNa, silent=silent)
  if(is.null(names(x))) names(x) <- sprintf(paste0("%0",nchar(length(x)),"d"),1:length(x))
  diNa <- if(prefix) list(paste("to",names(x),sep="."),paste("from",names(x),sep=".")) else list(names(x),names(x))
  out <- matrix(rep(x,length(x)), nrow=length(x), dimnames=diNa) -matrix(rep(x,each=length(x)), nrow=length(x))
  if(diagAsNA) diag(out) <- NA
  out }
   
