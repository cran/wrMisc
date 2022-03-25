#' Calculate all ratios between x and y
#'
#' This function calculates all possible pairwise ratios between all individual calues of x and y, or samples up to a maximum number of combinations.
#' 
#' @param x (numeric) vector, numerator for constructing rations
#' @param y (numeric) vector, denominator for constructing rations
#' @param maxLim (integer) allows reducing complexity by drawing for very long x or y
#' @param isLog (logical) adjust ratio calculation to log-data
#' @param silent (logical) suppress (less important) messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a (numeric) vector with all ratios
#' @examples
#' set.seed(2014); ra1 <- c(rnorm(9,2,1),runif(8,1,2))
#' ratioAllComb(ra1[1:9],ra1[10:17])
#' boxplot(list(norm=ra1[1:9], unif=ra1[10:17], rat=ratioAllComb(ra1[1:9],ra1[10:17])))
#' @export
ratioAllComb <- function(x, y, maxLim=1e4, isLog=FALSE, silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="ratioAllComb")
  namesXY <- c(deparse(substitute(x)), deparse(substitute(y)))
  if(!is.numeric(x)) x <- try(if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x), silent=TRUE)
  if(!is.numeric(y)) y <- try(if(is.factor(y)) as.numeric(as.character(y)) else as.numeric(y), silent=TRUE)
  msg <- " 'x' and 'y' must be numeric and length >0. Can't convert !"
  if(inherits(x, "try-error")) stop(fxNa,msg,namesXY[1]," to numeric")
  if(inherits(y, "try-error")) stop(fxNa,msg,namesXY[2]," to numeric")
  if(length(x) > maxLim){
    if(!silent) message(fxNa," reducing x from to ",length(x)," to user-selectend length ",maxLim)
    x <- sample(x,size=maxLim,replace=FALSE) }
  if(length(y) > maxLim){
    if(!silent) message(fxNa," reducing y from to ",length(y)," to user-selectend length ",maxLim)
    y <- sample(y, size=maxLim, replace=FALSE) }
  if(!silent) {nMax <- prod(length(x), length(y))
    if(nMax > 1e8) message(fxNa," calculations may take long time ! (",signif(nMax,4)," combinations !)")}
  rat <- if(isLog) rep(x,each=length(y)) -rep(y,length(x)) else rep(x,each=length(y))/rep(y,length(x))
  rat }

#' @export
.bringToCtr <- function(aa,ctr,ctrFa=0.75){
  ## bring most extreme value of 'aa' (numeric vector) closer to 'ctr'
  ## 'ctrFa' .. (numeric <1) modulate amplitude of effect
  ## return adjusted numeric vector
  di <- abs(aa-ctr)
  sel <- which(di ==max(di,na.rm=TRUE))
  aa[sel] <- ctr +max(di[if(length(aa) <3) which.min(di) else order(di)[length(aa)-1]],
    ctrFa*sum(di,na.rm=TRUE))*(((aa-ctr)/di)[sel])/sum(!is.na(di))
  aa }

#' @export
.breakInSer <- function(x,getFrom="last") {
  ## get series of values after last discontinuity
  ## 'x' as vector of numeric values
  ## 'getFrom' to decide if to extract from beginning or form from end (default)
  diffPos <- which(diff(x) >1)
  extrFrom <- if(length(diffPos) >0) {if(getFrom=="last") max(diffPos)+1 else min(diffPos)
  } else if(getFrom=="last") 1 else length(x)
  extrTo <- if(getFrom=="last") length(x) else 1
  x[sort(extrFrom:extrTo)] }

#' @export
.removeCol <- function(matr,rmCol) {
  ## remove columns indicated by col-number
  ## verifies that dimnames don't get lost and that output is not converted to simple vector (if 1 col remains)
  if(is.null(dim(matr))) stop(" expecting matrix or data.frame")
  if(!is.null(rmCol)) {
    if(max(rmCol) > ncol(matr)) stop(" 'rmCol' must not be higher than nuber of columns of 'matr'")
    iniDimNa <- dimnames(matr)
    matr <- matr[,-1*rmCol]
    if(is.null(dim(matr)))  matr <- as.matrix(matr)
    dimnames(matr) <- list(iniDimNa[[1]],iniDimNa[[2]][-1*rmCol]) }
  matr }

#' @export
.stackArray <- function(arr,byDim=3){
  ## reorganize array by reducing dimension 'byDim'  (similar to stack() for data-frames)
  ## returns array/matrix of 1 dimension less than 'arr', 1st dim has more lines (names as paste with '_')
  if(byDim > length(dim(arr))) stop(" 'byDim' should indicate the dimension-number to be supressed")
  dimNa <- dimnames(arr)
  useDi <- (1:length(dimNa))[-1*byDim]
  arr <- apply(arr,byDim,function(x) x)
  dimnames(arr) <- list(paste(rep(dimNa[[useDi[1]]],length(dimNa[[useDi[2]]])),
    rep(dimNa[[useDi[2]]],each=length(dimNa[[useDi[1]]])),sep="_"),dimNa[[byDim]])
  arr }

#' @export
.checkLegendLoc <- function(legLoc,defLoc="topright",callFrom=NULL){
  ## check argument for Location of legend
  ## if value provided not found as valid, use 'defLoc'
  fxNa <- .composeCallName(callFrom,newNa=".checkLegendLoc")
  if(is.null(legLoc)) legLoc <- defLoc
  if(!is.character(legLoc) | length(legLoc) >1) legLoc <- defLoc
  if(!(tolower(legLoc) %in% c("topleft","topright", "top","bottomright", "bottomleft", "bottom", "left","right","center"))) {
    legLoc <- defLoc
    message(fxNa," can't interpret value of 'legLoc', setting to default '",defLoc,"'") }
  tolower(legLoc) }

#' @export
.corDuplItemsByIncrem <- function(newNa,curNa,extPref="_s") {
  ## avoid duplicating items between 'curNa' and 'newNa' by incrementing digits after 'extPref' (in newNa)
  ## 'newNa' .. character vector for new names (to be adjusted/corrected)
  ## 'curNa' .. character vector for references of names
  ## cannot handle NAs !
  ## return corrected 'newNa' (character vector)
  chSear <- unlist(regexec(paste0(extPref,"[[:digit:]]+$"),newNa))           # for finding extension at very end
  if(all(chSear==-1)) corExt <- newNa else {
    tt <- chSea2 <- unlist(regexec(paste0(extPref,"[[:digit:]]"),newNa))       # for finding extension at first of mult occurance
    whS <- which(chSear >0)
    tt[tt==-1] <- nchar(newNa)[tt==-1] +1
    newExt <- cbind(bas=substr(newNa,1,tt-1),ext=substr(newNa,tt+2,nchar(newNa)))
    chSear <- unlist(regexec(paste0(extPref,"[[:digit:]]+$"),curNa))     # for finding extension at very end
    chSea2 <- unlist(regexec(paste0(extPref,"[[:digit:]]"),curNa))       # for finding extension at first of mult occurance
    whS <- which(chSear >0)
    curExt <- cbind(bas=substr(curNa[whS],1,chSea2[whS]-1),
      ext=substr(curNa[whS],chSear[whS]+2,nchar(curNa)[whS]))
    curMax <- tapply(as.numeric(curExt[,2]),curExt[,1],max)
    for(i in 1:length(curMax)) { uu <- which(newExt[,1]==names(curMax)[i])
      newExt[uu,2] <- 1+ curMax[i]:(curMax[i]+length(uu)-1) }
    corExt <- paste(newExt[,1],newExt[,2],sep=extPref)
    if(any(newExt[,2]=="")) corExt[which(newExt[,2]=="")] <- newExt[which(newExt[,2]==""),1] }
  corExt }
   
