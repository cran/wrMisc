#' CV of replicate plates (list of matrixes)
#'
#' \code{replPlateCV} gets CVs of replicates from list of 2 or 3-dim arrays (where 2nd dim is replicates, 3rd dim may be channel). 
#' Note : all list-elements of must MUST have SAME dimensions !
#' When treating data from microtiter plates (eg 8x12) data are typically spread over multiple plates, ie initial matrixes that are the organized into arrays.
#' Returns matrix or array (1st dim is intraplate-position, 2nd .. plate-group/type, 3rd .. channels)
#' @param lst list of matrixes : suppose lines are independent elements, colums are replicates of the 1st column. All matrixes must have same dimensions
#' @param callFrom (character) allows easier tracking of messages produced
#' @return matrix or array (1st dim is intraplate-position, 2nd .. plate-group/type, 3rd .. channels)
#' @seealso \code{\link{rowCVs}}, @seealso \code{\link{arrayCV}}
#' @examples
#' set.seed(2016); ra1 <- matrix(rnorm(3*96),nrow=8)
#' pla1 <- list(ra1[,1:12],ra1[,13:24],ra1[,25:36])
#' replPlateCV(pla1)
#' arrL1 <- list(a=array(as.numeric(ra1)[1:192],dim=c(8,12,2)),
#'   b=array(as.numeric(ra1)[97:288],dim=c(8,12,2)))
#' replPlateCV(arrL1)
#' @export
replPlateCV <- function(lst,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="replPlateCV")
  chDim <- sapply(lst,dim)
  msg <- "ALL list-elements of must MUST have SAME dimensions"
  if(is.matrix(chDim)) {if(any(apply(chDim,1,function(x) length(unique(x))>1))) stop(msg)} else stop(msg)
  if(length(dim(lst[[1]])) >2){
    out <- matrix(sapply(lst,arrayCV),nrow=nrow(lst[[1]]))
    tmp <- 1:(ncol(out)/2)
    out <- out[,c((2*tmp-1),2*tmp)]
    out <- array(out,dim=c(nrow(lst[[1]]),length(lst),dim(lst[[1]])[3]),dimnames=list(
      rownames(lst[[1]]),names(lst),dimnames(lst[[1]])[[3]]))
  } else {        
    out <- matrix(sapply(lst,arrayCV),nrow=nrow(lst[[1]]), dimnames=list(
      rownames(lst[[1]]),names(lst))) }
  out }
   
