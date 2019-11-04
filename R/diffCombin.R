#' Compute matrix of differences for all pairwise combinations of numeric vector
#'
#' \code{diffCombin} returns matrix of differences (eg resulting from subsititution) for all pairwise combinations of numeric vector 'x'.
#'
#' @param x numeric vector to compute differences for all combinations
#' @param diagAsNA (logical) return all self-self combinations as NA (otherwise 0)
#' @param prefix (logical) if TRUE, dimnames of output will specify orientation (prefix='from.' and 'to.')
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param silent (logical) suppress messages
#' @return numeric matrix of all pairwise differences
#' @seealso \code{\link[base]{diff}} for simple differences
#' @examples
#' diffCombin(c(10,11.1,13.3,16.6))
#' @export
diffCombin <- function(x,diagAsNA=FALSE,prefix=TRUE,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="diffCombin")
  if(!is.numeric(x)) x <- convToNum(x,remove=NULL,sciIncl=TRUE,callFrom=fxNa,silent=silent)
  if(is.null(names(x))) names(x) <- sprintf(paste("%0",nchar(length(x)),"d",sep=""),1:length(x))
  diNa <- if(prefix) list(paste("to",names(x),sep="."),paste("from",names(x),sep=".")) else list(names(x),names(x))
  out <- matrix(rep(x,length(x)),nrow=length(x),dimnames=diNa) -matrix(rep(x,each=length(x)),nrow=length(x))
  if(diagAsNA) diag(out) <- NA
  out }
   
