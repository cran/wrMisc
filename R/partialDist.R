#' Partial distance matrix (focus on closest)
#'
#' \code{partialDist} calculates distance matrix like \code{dist} for 1- or 2-dim data, but only partially, ie only cases of small distances.
#' This function was made for treating very large data-sets where only very close distances to a given point need to be found,
#' it allows to overcome memory-problems with larger data (and faster execution with > 50 rows of 'dat').
#' 
#' @param dat (matrix of numeric values) main input
#' @param groups (factor) to split using \code{cut} or specific custom grouping (length of dat)
#' @param overLap (logical) if TRUE make groups overlapping by 1 value (ie maintain some context-information)
#' @param method 'character' name of method passed to \code{dist}
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix (not of class 'dist')
#' @seealso \code{\link[stats]{dist}}
#' @examples
#' set.seed(2016); mat3 <- matrix(runif(300),nr=30)
#' round(dist(mat3),1)
#' round(partialDist(mat3,gr=3),1)
#' @export
partialDist <- function(dat,groups,overLap=TRUE,method="euclidean",silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="partialDist")
  dim1 <- length(dim(dat)) <2
  msg <- "names of 'dat' must be unique !"
  if(dim1) {
    if(is.null(names(dat))) names(dat) <- 1:length(dat)
    if(length(unique(names(dat))) < length(dat)) stop(msg)
    dis <- matrix(nrow=length(dat),ncol=length(dat))       # prepare for output
  } else {
    if(is.null(rownames(dat))) rownames(dat) <- paste("li",1:nrow(dat),sep="")
    if(length(unique(rownames(dat))) < nrow(dat)) stop(" row",msg)
    dis <- matrix(nrow=nrow(dat),ncol=nrow(dat))           # prepare for output
  }
  if(length(groups)==1 & is.numeric(groups)) {  if(dim1) {
      dat <- sort(dat)
      daCu <- cut(dat,groups)
    } else {   #  (2dim) sorting too complicated ?
      tmp <- apply(dat,2,function(x) as.numeric(cut(x,groups)))
      daCu <- factor(paste(tmp[,1],tmp[,2],sep="_"))
    }
  } else daCu <- groups
  ## check for orphan-groups & fuse 
  if(any(table(daCu) <2) & !overLap) message(fxNa,sum(table(daCu) <2)," orphan groups (n=1) in grouping ! (overLap=TRUE suggested)")
  dimnames(dis) <- if(dim1) list(names(dat),names(dat)) else list(rownames(dat),rownames(dat)) 
  datL <- list()
  if(overLap) {
    for(i in 1:length(unique(daCu))) {
      tmp <- which(daCu==levels(daCu)[i])
      tmp <- sort(c(tmp, range(tmp) +c(-1,1)))
      if(tmp[1] <1) tmp <- tmp[-1]
      if(max(tmp) > nrow(dis)) tmp <- tmp[which(tmp <= nrow(dis))]
      datL[[i]] <- if(dim1) dat[tmp] else dat[tmp,] }
   ## could check if some groups are now all redundant -> adjust datL & useNo
  } else {
    if(dim1) datL <- split(dat,daCu) else {
      useSep <- "_"                   #make sure this sep doesn't appear in rownames ! !!
      tmp <- apply(dat,1,paste,collapse=useSep)
      names(tmp) <- rownames(dat)
      datL <- split(tmp,daCu)
      datL <- lapply(datL,function(x) {
        y <- matrix(as.numeric(unlist(strsplit(x,useSep))),ncol=ncol(dat))
        rownames(y) <- names(x); y})
  } }
  for(i in 1:length(datL)) {
    useLi <- if(dim1) match(names(datL[[i]]),names(dat)) else match(rownames(datL[[i]]),rownames(dat))
    dis[useLi,useLi] <- as.matrix(stats::dist(datL[[i]],method=method)) }
  dis }

#' @export
.raiseColLowest <- function(mat, raiseTo=NULL,minFa=0.1,silent=FALSE,callFrom=NULL){
  ## independently for each col of mat : raise all values close to lowest value to end up as at value of 'raiseTo'
  ## select all values within range of 'minFa' to determined min (eg 0.1 select until min+10%ofMin)
  ## this version sets all data to common raiseTo (which is min among all cols)
  fxNa <- .composeCallName(callFrom,newNa=".raiseColLowest")
  colMin <- apply(as.matrix(mat),2,min,na.rm=TRUE)
  if(is.null(raiseTo)) {
    raiseTo <- apply(mat,2,function(x) {
      y <- sort(unique(signif(x,3)))
      y[which(y > min(y,na.rm=TRUE) +0.1* abs(min(y,na.rm=TRUE))) [1]] })
    raiseTo <- min(raiseTo,na.rm=TRUE)
    if(!silent) message(fxNa," 'raiseTo' was set to ",raiseTo)}
  if(length(raiseTo) >1) raiseTo <- min(raiseTo,na.rm=TRUE)
  raiseF <- matrix(rep(raiseTo -colMin,each=nrow(mat)),nrow=nrow(mat))
  raiseF[mat > raiseTo + minFa*abs(raiseTo)] <- 0
  out <- mat + raiseF
  out }

#' @export
.findBorderOverlaps <- function(x,rmRedund=FALSE,callFrom=NULL){
  ## find overlap instances among range of values in lines of 'x' (typically give just min & max)
  ## 'x' .. matrix (or all-numeric data.frame), inspect by lines for potential overlap
  ## 'rmRedund' .. report overlaps only in 1st instance (will show up twice otherwise)
  ## return matrix with line for each overlap found, cols 'refLi' (line no), 'targLi' (line no), 'targCol' (col no)
  fxNa <- .composeCallName(callFrom,newNa=".findBorderOverlaps")
  if(any(is.na(x))) {
    message(fxNa," NAs detected, remove all lines with NAs only")
    x <- x[which(rowSums(!is.na(x)) ==ncol(x)),]
    if(any(is.na(x))) { NaLi <- which(rowSums(is.na(x)) >0)
      x[NaLi,] <- apply(x[NaLi],1,min)}}
  tmp <- matrix(NA,nrow=nrow(x),ncol=ncol(x)*(nrow(x)-1))
  for(i in 1:nrow(x)) {      # check each line of x against everything else
    te <- as.numeric(t(x[-i,]))
    tmp[i,] <- max(x[i,]) > te & te > min(x[i,])
  }
  curLi <- 1
  out <- matrix(nrow=sum(tmp),ncol=3,dimnames=list(NULL,c("refLi","targLi","targCol")))
  for(i in which(rowSums(tmp) >0)) {             # extract corresponding line & col info
    ti <- which(tmp[i,])
    if(length(ti) >0)  {
      to <- cbind(ref=rep(i,length(ti)),li=(1:nrow(x))[-i][ti %/% ncol(x)+ ti %% ncol(x)], col=ncol(x)- ti %% ncol(x))
      if(rmRedund & curLi >1) {to <- to[which(!(to[,2] %in% out[,1] & to[,1] %in% out[,2])),]
        if(length(to) >0) {
          if(!is.matrix(to)) to <- matrix(to,ncol=3)
          out[curLi:(curLi+nrow(to)-1),] <- to }
      } else out[curLi:(curLi+nrow(to)-1),] <- to
      curLi <- curLi +length(ti) }
  }
  out <- out[which(rowSums(is.na(out)) <2),]
  out  }
  
