#' Partial unlist of lists of lists
#'
#' \code{partUnlist} does partial unlist for treating list of lists : New (returned) list has one level less of hierarchy 
#' (Highest level list will be appended). In case of conflicting (non-null) listnames a prefix will be added. 
#' Behaviour different to \code{\link[base]{unlist}} when unlisting list of matrixes.
#' @param lst (list) main input, list to be partially unlisted
#' @param sep (character, length=1) separator for names
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list with partially reduced nested structure
#' @seealso \code{\link[base]{unlist}}, \code{\link{asSepList}}
#' @examples
#' partUnlist(list(list(a=11:12,b=21:24), list(c=101:101,d=201:204)))
#' li4 <- list(c=1:3, M2=matrix(1:4,ncol=2), L3=list(L1=11:12, M3=matrix(21:26,ncol=2)))
#' partUnlist(li4)
#' unlist(li4, rec=FALSE)
#' @export
partUnlist <- function(lst, sep="_", silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="partUnlist")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  notL <- !sapply(lst, inherits, "list")  
  if(debug) message(fxNa," pU1")
  if(all(notL)) { if(!silent) message(fxNa,"Input is not list of lists, nothing to do")
    return(lst)
  } else {
    ## lst is list of list(s)
    out <- if(any(notL)) lst[which(notL)] else list()   # the non-list elements
    for(i in which(!notL)) {
      iniLe <- length(out)
      newNa <- names(lst[[i]])      
      if(any(nchar(newNa) >0)) {
        newNa <- if(any(newNa %in% names(out))) paste(names(lst)[i], newNa, sep=sep) else newNa}
      out[length(out) +1:length(lst[[i]])] <- lst[[i]] 
      if(any(nchar(newNa) >0)) names(out)[iniLe + 1:length(lst[[i]])] <- newNa }
    out }}  
     
