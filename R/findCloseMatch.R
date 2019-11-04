#' Find close numeric values between two vectors
#'
#' \code{findCloseMatch} finds close matches (similar values) between two numeric vectors ('x','y') based on method 'compTy' and threshold 'limit'. 
#'  Return list with close matches of 'x' to given 'y', the numeric value dependes on 'sortMatch' (if FALSE then always value of 'y' otherwise of longest of x&y).  
#'  Note: Speed & memory improvement if 'sortMatch'=TRUE (but result might be inversed!): adopt search of x->y or y->x to searching matches of each longest to each shorter  (ie flip x &y).
#'  Otherwise, if length of 'x' & 'y' are very different, it may be advantagous to use a long(er) 'x'  and short(er) 'y' (with 'sortMatch'=FALSE).  
#'  Note: Names of 'x' & 'y' or (if no names) prefix letters 'x' & 'y' are always added as names to results.
# ' Note: Takes much RAM if x & y are large 
#'
#' @param x numeric vector for comparison
#' @param y numeric vector for comparison
#' @param compTy (character) may be 'diff' or 'ppm', will be used with threshold from argument 'limit'
#' @param limit (numeric) threshold value for retaining values, used with distace-type specified in argument 'compTy'
#' @param asIndex (logical) optionally rather report index of retained values
#' @param maxFitShort (numeric) limit output to max number of elements (avoid returning high number of results if filtering was not enough stringent)
#' @param sortMatch (logical) if TRUE than matching will be preformed as 'match longer (of x & y) to closer', this may process slightly faster (eg 'x' longer: list for each 'y' all 'x' that are close, otherwise list of each 'x'), 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return list with close matches of 'x' to given 'y', the numeric value dependes on 'sortMatch' (if FASLE then always value of 'y' otherwise of longest of x&y)
#' @seealso \code{\link{checkSimValueInSer}} and (from this package) \code{.compareByDiff}, for convient output \code{\link{countCloseToLimits}} 
#' @examples
#' aa <- 11:14 ; bb <- c(13.1,11.5,14.3,20:21)
#' findCloseMatch(aa,bb,com="diff",lim=0.6)
#' findCloseMatch(c(a=5,b=11,c=12,d=18),c(G=2,H=11,I=12,J=13)+0.5,comp="diff",lim=2)
#' findCloseMatch(c(4,5,11,12,18),c(2,11,12,13,33)+0.5,comp="diff",lim=2)
#' findCloseMatch(c(4,5,11,12,18),c(2,11,12,13,33)+0.5,comp="diff",lim=2,sort=FALSE)
#' .compareByDiff(list(c(a=10,b=11,c=12,d=13),c(H=11,I=12,J=13,K=33)+0.5),limit=1)   #'  return matrix
#' 
#' a2 <- c(11:20); names(a2) <- letters[11:20]
#' b2 <- c(25:5)+c(rep(0,5),(1:10)/50000,rep(0,6)); names(b2) <- LETTERS[25:5]
#' which(abs(b2-a2[8]) < a2[8]*1e-6*5)                                          #'  find R=18 : no10
#' findCloseMatch(a2,b2,com="ppm",lim=5)                                        #'  find Q,R,S,T
#' findCloseMatch(a2,b2,com="ppm",lim=5,asI=TRUE)                               #'  find Q,R,S,T
#' findCloseMatch(b2,a2,com="ppm",lim=5,asI=TRUE,sort=FALSE)
#' findCloseMatch(a2,b2,com="ratio",lim=1.000005)                               #'  find Q,R,S,T
#' findCloseMatch(a2,b2,com="diff", lim=0.00005)                                #'  find S,T
#' @export
findCloseMatch <- function(x,y,compTy="ppm",limit=5,asIndex=FALSE,maxFitShort=100,sortMatch=FALSE,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="findCloseMatch")
  compMeth <- c("ppm","diff","ratio")
  msg <- c(paste("argument 'compTy' may be one of",pasteC(compMeth,qu="'",la=" or "))," , trimming to length=1")
  if(length(compTy) <1) stop(msg[1]) else if(length(compTy)>1) {compTy <- compTy[1]; message(msg)}
  if(!compTy %in% compMeth) stop(msg[1])
  if(is.null(names(x))) names(x) <- paste("x",equLenNumber(1:length(x)),sep="")                   # need default names to know which of 'x' in results !
  if(is.null(names(y))) names(y) <- paste("y",equLenNumber(1:length(y)),sep="")                   #
  if(any(names(x) %in% names(y))) message(fxNa," note that some names do overlap (beware not to confuse...) !")
  dat <- list(x,y)
  if(sortMatch) dat <- dat[order(sapply(dat,length),decreasing=FALSE)]
  if(is.character(maxFitShort)) if(length(grep("%$",maxFitShort)) >0) {
    maxFitShort <- ceiling(length(dat[[1]])*as.numeric(sub("%$","",maxFitShort))/100)
    } else stop("can't interpret 'maxFitShort'")
  tm2 <- switch(compTy,
    ppm=.compareByPPM(dat,limit,distVal=!asIndex),
    diff=.compareByDiff(dat,limit,distVal=!asIndex),
    ratio=.compareByLogRatio(dat,limit,distVal=!asIndex))
  cSu <- colSums(!is.na(tm2))
  cPi <- which(cSu >0)
  if(length(cPi) <1) return(NULL) else {
    if(any(cSu > maxFitShort)) {     # case of fitting close to (very) large number of elements -> keep maxFitShort lowest (+ set others to NA)
      if(!silent) message(fxNa,sum(cSu > maxFitShort,na.rm=TRUE)," column elements at too many 'close elements' try to reduce ..")
      for(i in 1:which(cSu >maxFitShort)) {limDis <- sort(tm2[,i],decreasing=TRUE,na.last=TRUE)[maxFitShort]
        tmX <- which(tm2[,i] <= limDis)
        if(nrow(tm2) >maxFitShort & length(tmX) >= min(maxFitShort*1.15,floor(nrow(tm2)*0.96))) tm2[,i] <- NA else tm2[which(tm2[,i] >limDis),i] <- NA}
      cPi <- which(colSums(!is.na(tm2)) >0)        # refresh
    }
    rSu <- rowSums(!is.na(tm2))     
    if(any(rSu > maxFitShort)) {     # case of fitting close to (very) large number of elements -> keep maxFitShort lowest (+ set others to NA)
      if(!silent) message(fxNa,sum(rSu > maxFitShort,na.rm=TRUE)," row elements at too many 'close elements' try to reduce ..")
      for(i in 1:which(rSu >maxFitShort)) {limDis <- sort(tm2[i,],decreasing=TRUE,na.last=TRUE)[maxFitShort]
        tmY <- which(tm2[i,] <= limDis)
        if(ncol(tm2) >maxFitShort & length(tmY) >= min(maxFitShort*1.15,floor(ncol(tm2)*0.96))) tm2[i,] <- NA else tm2[i,which(tm2[i,] >limDis)] <- NA}
      rPi <- which(rowSums(!is.na(tm2)) >0)        # refresh (rows with some distance values)
    } else rPi <- which(rSu >0)
    zz <- if(nrow(tm2) >1) as.matrix(tm2[,cPi])[rPi,] else matrix(tm2[,cPi],nrow=1,dimnames=list(rownames(tm2),colnames(tm2)[cPi]))
    if(length(dim(zz)) <2) zz <- matrix(zz,nrow=length(rPi))
    if(is.null(colnames(zz))| is.null(rownames(zz))) dimnames(zz) <- list(rownames(tm2)[rPi],colnames(tm2)[cPi])
    out <- if(asIndex) {if(length(cPi) >1) apply(!is.na(zz),2,which) else which(!is.na(zz))         #list of indexes   dat,function(z) which(z))
    } else {if(length(cPi) >1) apply(zz,2,naOmit) else naOmit(zz)} 
    if(!is.list(out)) {out <- as.list(out); zn <- rownames(zz)[apply(!is.na(zz),2,which)]; for(j in 1:length(out)) names(out[[j]]) <- zn[j]} 
    if(is.null(names(out))) names(out) <- rep(names(cPi),length(out))[1:length(out)]
  out }}
 
#' @export
.compareByPPM <- function(dat,limit,distVal=FALSE){
  ## compare both vectors from list 'dat' for similar values based on ppm
  ## 'dat' .. list of 2 numerical vectors
  ## 'distVal'.. (logical) to toggle outpout as matrix of numeric (distance values above 'limit', others NA) or matrix of logical
  ## 'limit'.. (numeric, length=1) threshold to be applied
  ## return logical matrix with rows for 1st & cols for 2nd element of dat  (used in findCloseMatch() )
  ref <- matrix(rep(dat[[1]],each=length(dat[[2]])),nrow=length(dat[[2]]),dimnames=list(names(dat[[2]]),names(dat[[1]])))       # matr of short
  que <- matrix(rep(dat[[2]],length(dat[[1]])),nrow=length(dat[[2]]),dimnames=list(names(dat[[2]]),names(dat[[1]])))
  if(distVal) {tm2 <- (2*(que <= ref) -1)*abs(que/ref -1)/1e-6; 
    tm2[which(!abs(que/ref -1) < limit*1e-6)] <- NA
  } else tm2 <- abs(que/ref -1) < limit*1e-6
  tm2 }

#' @export
.compareByLogRatio <- function(dat,limit,distVal=FALSE){
  ## compare both vectors from 'dat' for similar values based on (log)ratio
  ## 'dat' .. list of 2 numerical vectors
  ## 'distVal'.. (logical) to toggle outpout as matrix of numeric (distance values above 'limit', others NA) or matrix of logical
  ## 'limit'.. (numeric, length=1) threshold to be applied
  ## return logical matrix of  (used in findCloseMatch() )
  ref <- matrix(rep(dat[[1]],each=length(dat[[2]])),nrow=length(dat[[2]]),dimnames=list(names(dat[[2]]),names(dat[[1]])))       # matr of short
  que <- matrix(rep(dat[[2]],length(dat[[1]])),nrow=length(dat[[2]]),dimnames=list(names(dat[[2]]),names(dat[[1]])))
  if(distVal) {tm2 <- abs(log2(ref/que)); tm2[which(!abs(log2(ref/que)) <= log2(abs(limit)))] <- NA
  } else tm2 <- abs(log2(ref/que)) <= log2(abs(limit))
  tm2 }

#' @export
.compareByDiff <- function(dat,limit,distVal=FALSE){
  ## compare both vectors from 'dat' for similar values based on (absolute) difference
  ## 'dat' .. list of 2 numerical vectors
  ## 'distVal'.. (logical) to toggle outpout as matrix of numeric (distance values above 'limit', others NA) or matrix of logical
  ## 'limit'.. (numeric, length=1) threshold to be applied
  ## return logical matrix of  (used in findCloseMatch() )
  ref <- matrix(rep(dat[[1]],each=length(dat[[2]])),nrow=length(dat[[2]]),dimnames=list(names(dat[[2]]),names(dat[[1]])))       # matr of short
  que <- matrix(rep(dat[[2]],length(dat[[1]])),nrow=length(dat[[2]]),dimnames=list(names(dat[[2]]),names(dat[[1]])))
  if(distVal) {
    tm2 <- que-ref
    chLi <- abs(tm2) > abs(limit)
    if(any(chLi)) tm2[which(chLi)] <- NA
  } else tm2 <- abs(ref-que) <= abs(limit)
  tm2 }
   
