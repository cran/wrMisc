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
#' partUnlist(list(c=1:3,L2=matrix(1:4,ncol=2),list(L1=11:12,L2=matrix(21:26,ncol=2))))
#' unlist(list(c=1:3,L2=matrix(1:4,ncol=2),list(L1=11:12,L2=matrix(21:26,ncol=2))),rec=FALSE)
#' @export
partUnlist <- function(lst){
  chTy <- rep(NA,length(lst))
  for(i in 1:length(lst)) chTy[i] <- class(lst[[i]])
  notL <- if(any(chTy !="list")) lst[which(chTy !="list")] else NULL
  if(all(chTy !="list")) return(lst)
  lst <- lst[which(chTy =="list")]
  out <- lst[[1]]
  pref <- names(lst)
  if(is.null(pref) | length(pref) < length(lst)) pref <- 1:length(lst)
  if(length(lst) >1) for(i in 2:length(lst)) {
    newInd <- (length(out)+1):(length(out)+length(lst[[i]]))
    out[newInd] <- lst[[i]]
    newNa <- names(lst[[i]])
    if(!is.null(newNa)) {
      if(any(names(out) %in% names(lst[[i]]))) newNa <- paste(pref[i],newNa,"_")
      names(out)[newInd] <- newNa }
  }
  if(length(notL) >0) {out[length(out)+(1:length(notL))] <- notL
    newNa <- names(notL)
    if(any(newNa %in% names(out))) newNa[which(newNa %in% names(out))] <- paste(newNa[which(newNa %in% names(out))],"2",sep="_") # avoid existig names
    names(out)[length(out)-(length(notL):1)+1] <- newNa}
  out }
    
