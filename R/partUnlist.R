#' Partial unlist of lists of lists
#'
#' \code{partUnlist} does  partial unlist for treating list of lists : New (returned) list has one level less of hierarchy 
#' (Highest level list will be appended). In case of conflicting (non-null) listnames a prefix will be added. 
#' Behaviour different to \code{\link[base]{unlist}} when unlisting list of matrixes.
#' @param lst list to be partailly unlisted
#' @return list with partially reduced nested structure
#' @seealso \code{\link[base]{unlist}}, \code{\link{asSepList}}
#' @examples
#' partUnlist(list(list(a=11:12,b=21:24),list(c=101:101,d=201:204)))
#' li4 <- list(c=1:3,L2=matrix(1:4,ncol=2),li3=list(L1=11:12,L2=matrix(21:26,ncol=2)))
#' partUnlist(li4)
#' unlist(li4,rec=FALSE)
#' @export
partUnlist <- function(lst) {
  notL <- sapply(lapply(lst,class),function(x) !("list" %in% x))
  if(sum(notL) <1) return(lst) else {
    ## lst is list of list(s)
    out <- if(any(notL)) lst[which(notL)] else list()
    for(i in which(!notL)) {
      iniLe <- length(out)
      newNa <- names(lst[[i]])      
      if(any(nchar(newNa) >0)) {
        if(any(newNa %in% names(out))) newNa <- paste(names(lst)[i],newNa,sep="_")}
      out[length(out)+1:length(lst[[i]])] <- lst[[i]] 
      if(any(nchar(newNa) >0)) names(out)[iniLe + 1:length(lst[[i]])] <- newNa }
    out }}  
     
