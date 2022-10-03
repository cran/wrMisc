#' Replace Most Distant Values by NA
#'
#' This procedures aims to streighten (clean) the most extreme values of noisy replicates by identifying the most distant points 
#' (among a set of replicates). The input 'x' (matrix or data.frame) is supposed to come from multiple different measures taken 
#' in replicates (eg weight of different individuals as rows taken as multiple replicate measures in subsequent columns).
#' 
#' @details
#' With the argument \code{nOutl} the user chooses the total number of most extreme values to replace by \code{NA}.
#' how many of the most extreme replicates of the whole dataset will replaced by \code{NA}, ie with \code{nOutl=1} 
#' only the single most extreme outlyer will be replaced by \code{NA}. 
#' Outlier points are determined as point(s) with highest distance to (row) center (median and mean choice via argument 'centrMeth').
#' Thus function returns input data with "removed" points set to \code{NA}, or if \code{retOffPos=TRUE} the most extreme/outlier positions.
#'
#' @param x matrix (or data.frame)
#' @param centrMeth (character) method to summarize (mean or median)
#' @param nOutl (integer) determines how many points per line will be set to \code{NA} (with n=1 the worst row of replicates will be 'cleaned')
#' @param retOffPos (logical) if \code{TRUE}, replace the most extreme outlyer only
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix of same dimensions as input \code{x},  data-points which were tagged/removed are set to \code{NA}, or if \code{retOffPos=TRUE} the most extreme/outlier positions
#' @examples
#' mat3 <- matrix(c(19,20,30, 18,19,28, 16,14,35),ncol=3)
#' cleanReplicates(mat3, nOutl=1)
#' @export
cleanReplicates <- function(x, centrMeth="median", nOutl=2, retOffPos=FALSE, silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="cleanReplicates")
  if(!any(is.data.frame(x),is.matrix(x))) stop(" 'x' is assumed as matrix or data.frame")
  if(length(dim(x)) !=2) stop(fxNa," Designed for 2dim 'x'  (here ",if(is.null(ncol(x))) "none" else ncol(x),")")
  if(nOutl >= ncol(x)) stop(fxNa," Argument 'nOutl' should not reach or exceed number of columns of 'x'  (here ",ncol(x)," cols)")
  if(is.data.frame(x)) x <- as.matrix(x)
  if(length(table(table(rownames(x)))) != 1) {
    if(!silent) message(fxNa," rownames of 'x' either NULL or not unique, replacing by row-numbers")
    rownames(x) <- 1:nrow(x) }
  xCV <- rowCVs(x)    
  xCV <- xCV[which(rowSums(!is.na(x)) >0)]
  badCV <- which(xCV >= sort(xCV, decreasing=TRUE, na.last=TRUE)[nOutl])
  if(!silent) message(fxNa,"removing ",length(badCV)," entries in lines ",paste(names(badCV),collapse=","))
  centr <- if(identical(centrMeth, "median")) apply(x, 1, stats::median,na.rm=TRUE) else rowMeans(x, na.rm=TRUE)
  di <- abs(x - matrix(rep(centr, ncol(x)), nrow=nrow(x)))
  di[which(rowSums(!is.na(di)) <1), ] <- 0
  if(!silent & sum(is.infinite(di)) >0) message(fxNa," Attention, ",sum(is.infinite(di))," distance values are infinite !")
  for(i in 1:length(badCV)) {y <- badCV[i]
    x[match(names(y), rownames(x)), which(di[y,] ==max(di[y,], na.rm=TRUE))[1]] <- NA }
  x }
    
