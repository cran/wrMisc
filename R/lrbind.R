#' rbind on lists
#'
#' rbind-like function to append list-elements containing tables and return one long table. 
#' Accepts also list-entries with data.frames or vectors (of length of no of columns) as long as at least 1 list-entry is a matrix
#' @param lst (list) main input (each list-element should have same number of columns, numeric vectors will be converted to number of columns of other elements)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix or data.frame
#' @seealso \code{rbind} in \code{\link[base]{cbind}}
#' @examples
#' lst1 <- list(matrix(1:9,nc=3,dimnames=list(letters[1:3],c("AA","BB","CC"))),
#'   11:13,matrix(51:56,ncol=3))
#' lrbind(lst1)
#' @export
lrbind <- function(lst,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="lrbind")
  argN <- deparse(substitute(lst))
  chDf <- sapply(lst,function(x) "data.frame" %in% class(x))
  if(any(chDf)) for (i in which(chDf)) lst[[i]] <- as.matrix(lst[[i]])
  chNum <- sapply(lst,function(x) any(c("numeric","integer") %in% class(x)))
  nCol <- if(any(!chNum)) ncol(lst[[which(!chNum)[1]]]) else min(sapply(lst,length),na.rm=TRUE)
  if(any(chNum)) for (i in which(chNum)) lst[[i]] <- matrix(lst[[i]],ncol=nCol)
  ldim <- sapply(lst,dim)
  if(length(unique(ldim[2,])) >1) stop("bad dimension(s): each list-element should have same number of columns")
  outm <- matrix(nrow=sum(ldim[1,]),ncol=ldim[2,1])
  ldim <- rbind(ldim,cumsum(ldim[1,]))
  ldim <- rbind(ldim,c(1,ldim[3,-1*ncol(ldim)]+1))
  for(i in 1:ncol(ldim)) outm[ldim[4,i]:ldim[3,i],] <- lst[[i]]
  lstColn <- unlist(sapply(lst,colnames))
  if(!is.null(lstColn)) if(length(unique(lstColn)) ==ldim[2,1] & !silent) message(fxNa," col-names not homogenous or partially absent in ",argN)
  colnames(outm) <- colnames(lst[[1]])
  lstRown <- unlist(sapply(lst,rownames))
  if(!is.null(lstRown)) if(length(unique(lstRown)) ==ldim[3,ncol(ldim)]) {
    rownames(outm) <- unique(lstRown)} else if(!silent) message(fxNa," row-names not complete")
  outm }
   
