#' Fuse pairs to generate cluster-names
#'
#' Fuse previously identified pairs to 'clusters', return vector with cluster-numbers. 
#' @param datPair 2-column matrix where each line represents 1 pair
#' @param refDatNames (NULL or character) allows placing selected pairs in context of larger data-set (names to match those of 'datPair')
#' @param inclRepLst (logical) if TRUE, return list with 'clu' (clu-numbers, default output) and 'refLst' (list of clustered elements, only n>1)
#' @param maxFuse (integer, default NULL) maximal number of groups/clusters
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns a vector with cluster-numbers
#' @examples
#' daPa <- matrix(c(1:5,8,2:6,9), ncol=2)
#' fusePairs(daPa, maxFuse=4)
#' @export
fusePairs <- function(datPair, refDatNames=NULL, inclRepLst=FALSE, maxFuse=NULL, debug=FALSE, silent=TRUE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="fusePairs")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  msg <- " 'datPair' should be matrix of 2 cols with paired rownames"
  if(length(dim(datPair)) !=2) stop(msg) else if(ncol(datPair) <2) stop(msg)
  if(!is.matrix(datPair)) datPair <- if(nrow(datPair) >1) as.matrix(datPair) else matrix(datPair,nrow=1,dimnames=dimnames(datPair))
  if(is.null(maxFuse)) maxFuse <- nrow(datPair)+1 else {
    if(!silent) message(fxNa," use of 'maxFuse' at ",maxFuse," may increase 'artificially' the number of groups/clusters")}
  if(!is.null(refDatNames)) { chRef <- unique(as.character(datPair[,1:2])) %in% as.character(refDatNames)
    if(sum(!chRef,na.rm=TRUE) >0) { if(!silent) message(fxNa,sum(!chRef,na.rm=TRUE)," names from 'datPair' missing in 'refDatNames', ignore")
    refDatNames <- refDatNames } }
  refDatNa <- if(is.null(refDatNames)) unique(as.character(datPair[,1:2])) else refDatNames
  if(is.null(rownames(datPair))) rownames(datPair) <- 1:nrow(datPair)
  nPt <- length(unique(as.character(datPair[,1:2])))
  similDat <- list(which(refDatNa %in% datPair[1,1:2]))          # similDat .. index of refDatNa per cluster
  if(debug) {message(fxNa," dim(datPair) ",nrow(datPair)," ",ncol(datPair))}
  if(nrow(datPair) >1) for(i in 2:nrow(datPair)) {
    basTest <- datPair[i,1:2] %in% as.character(datPair[1:(i-1),1:2])      # see if (any of) new pair already seen before
    chPr <- sapply(similDat, function(x) sum(datPair[i,1:2] %in% refDatNa[x]))   # for each clu : number of common with cur query
    if(debug) message(fxNa," basTest  ",pasteC(basTest))
    if(any(chPr >0) && length(similDat[[which.max(chPr)]]) < maxFuse) {                                     # add to existing cluster
      if(sum(basTest)==1){
        similDat[[which.max(chPr)]] <- unique(sort(c(similDat[[which.max(chPr)]], which(refDatNa %in% datPair[i,1:2]))))
      } else {                                               # fuse clusters instead of adding
        isMax <- which(chPr==max(chPr))
        if(length(isMax) >1) {
          if(debug) message(fxNa," iter no ",i,"  fuse clusters : max ",pasteC(isMax))
          similDat[[isMax[1]]] <- unique(unlist(similDat[which(chPr==max(chPr))]))
        similDat <- similDat[-isMax[2]]
        } else if(debug) message(fxNa," iter no ",i," : loop, already fused")}        # don't make loops circular
    } else {                                                     # create new cluster
      if(debug) message(fxNa,"Create new cluster no ",length(similDat)+1,"  for ",pasteC(which(refDatNa %in% datPair[i,1:2])))
      similDat[[length(similDat)+1]] <- which(refDatNa %in% datPair[i,1:2])
  }  }
  msg <- "Trouble ahead, some elements are indexed more than 1x in 'similDat' !!!"
  if(any(table(unlist(similDat)) >1) && maxFuse < nrow(datPair) && !silent) message(fxNa,msg)
  out <- rep(1:length(similDat), sapply(similDat,length))
  names(out) <- unlist(lapply(similDat, function(x) refDatNa[x]))
  if(debug) {message(fxNa," fP2")}
  if(length(refDatNa) > length(unlist(similDat))) {
    tmp <- refDatNa[which(!(refDatNa %in% as.character(as.matrix(datPair[,1:2]))))]
    out2 <- (1:length(tmp)) +length(out)
    names(out2) <- tmp
    out <- c(out,out2) }
  out <- out[order(convToNum(.trimFromStart(names(out)), remove=NULL, sciIncl=TRUE))]
  if(inclRepLst) list(clu=out, repLst=similDat) else out }


#' Distances beteenw sorted points of 2-columns
#'
#' This function returns distances beteenw sorted points of 2-column matrix 'x'
#' 
#' @param x (matrix or data.frame, min 2 columns) main input
#' @param asSum (logical) if \code{TRUE} (default) the sum of all distances will be returned, otherwise the individual distances
#' @return This function returns a numeric vector with distances
#' @examples
#' daPa <- matrix(c(1:5,8,2:6,9), ncol=2)
#' .neigbDis(daPa)
#' @export
.neigbDis <- function(x, asSum=TRUE) {    # return (sum of) distances betw sorted points of 2-column matrix 'x'
  if(nrow(x) ==2) sqrt(sum(abs(x[1,] -x[2,]))) else {
    x <- x[order(rowMeans(x)),]
    out <- sqrt(rowSums((x[-nrow(x),]-x[-1,])^2, na.rm=TRUE))
    if(!isFALSE(asSum)) sum(out, na.rm=TRUE) else out } }
      
