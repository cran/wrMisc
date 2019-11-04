#' Organize values into list and sort by names 
#'
#' Sort values of \code{'x'} by its names and organize as list by common names, the names until \code{'sep'} are used for (re)grouping. 
#' Note that typical spearators occuring the initial names may need protection by '\\' (this is automatically taken care of for the case of the dot ('.') separator).
#' @param x (list) main input
#' @param sep (character) separator (note that typcal separators may need to be protected, only automatically added for '.')
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return matrix or data.frame
#' @seealso \code{rbind} in \code{\link[base]{cbind}}
#' @examples
#' listGroupsByNames((1:10)/5)
#' ser1 <- 1:6; names(ser1) <- c("AA","BB","AA.1","CC","AA.b","BB.e")
#' listGroupsByNames(ser1)
#' @export
listGroupsByNames <- function(x,sep=".",silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="listGroupsByNames")
  if(length(sep) <1) sep <- "\\."
  if(length(sep) >1) {if(!silent) message(fxNa,"'sep' should be character of length 1"); sep <- sep[1]}
  if(sep %in% c(".")) sep <- paste("\\",sep,sep="")
  if(length(names(x)) <1) { message(fxNa," no names found in 'x' !!")
    names(x) <- spl <- sapply(strsplit(as.character(x),sep),function(y) y[1])
  } else spl <- strsplit(names(x),sep)
  ex1st <- sapply(spl,function(x) x[1])
  if(length(ex1st) > length(unique(ex1st))) {
    out <- lapply(unique(ex1st),function(y) which(ex1st==y))
    out <- lapply(out,function(y) c(x[y]))
  } else out <- as.list(x)
  if(length(names(out)) <1) names(out) <- sapply(out,function(y) names(y)[1])
  out }
    
