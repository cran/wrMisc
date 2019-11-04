#' Minimum distance/difference between values
#'
#' \code{minDiff} aims to find the min distance (ie closest point) to any other x (numeric value), ie intra 'x' and
#'  returns matrix with 'index','value','dif','ppm','ncur','nbest','best'.
#' At equal distance to lower & upper neighbour point, the upper (following) point is chosen (as single best).
#' In case of multiple ex-aequo distance returns 1st of multiple, may be different at various repeats.
#'
#' @param x (numeric) vector to search minimum difference
#' @param digSig number of significant digits, used for ratio or ppm column
#' @param ppm (logical) display distance as ppm (1e6*diff/refValue, ie normalized difference eg as used in mass spectrometry), otherwise the ratio is given as : value(from 'x') / closestValue (from 'x')
#' @param initOrder (logical) return matrix so that 'x' matches exactely 2nd col of output
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix
#' @seealso  \code{\link[stats]{dist}}
#' @examples
#' set.seed(2017); aa <- 100*c(0.1+round(runif(20),2),0.53,0.53)
#' minDiff(aa); minDiff(aa,initO=TRUE,ppm=FALSE); .minDif(unique(aa))
#' @export
minDiff <- function(x,digSig=3,ppm=TRUE,initOrder=TRUE,callFrom=NULL) {
  ## for comparison of 2 vectors see .clostestNumByRat()  or
  fxNa <- .composeCallName(callFrom,newNa="minDiff")
  y <- as.numeric(x)
  so <- .minDif(y,initOrder=FALSE,rat=TRUE)
  chNegDif <- which(so[,5] <1)    # for orientation
  if(length(chNegDif) >0) so[chNegDif,c(3)] <- -1*so[chNegDif,c(3)]       # neg 'dif' if 'value' is lower than best/closest(~ref)
  so <- cbind(so,ncur=1,nbest=1)
  if(length(digSig) >0 & !ppm) if(is.numeric(digSig)) so[,"rat"] <- signif(so[,"rat"],digits=digSig)  # finalize ratio result
  repInd <- which(c(diff(so[,2]),NA)==0)                                  # index of repeated vals exept last of series
  if(length(repInd) >0) {                                                 # repeated values, need to correct 'so'
    firOf <- repInd[c(1,1+which(diff(repInd) >1))]                        # first of repeated (values for diff incorrect)  ,length(repVa)
    repIn2 <- sort(unique(c(repInd,repInd+1)))                            # index of all repeated
    tab <- table(so[repIn2,2])
    lasOf <- repIn2[cumsum(tab)]                                          # index of last of rep; correct dist is in last of series !
    z2 <- so[firOf-1,1:2]
    z3 <- so[lasOf+1,1:2]
    if(length(dim(z2)) <2) z2 <- matrix(z2,nrow=1,dimnames=list(NULL,names(z2)))
    if(length(dim(z3)) <2) z3 <- matrix(z3,nrow=1,dimnames=list(NULL,names(z3)))
    zz <- rbind(so[firOf,1:2],z2[which(!z2[,2] %in% so[firOf,2]),])
    zz <- rbind(zz,z3[which(!z3[,2] %in% zz[,2]),])
    s3 <- .minDif(zz[,2],initOrder=FALSE)
    s3[,1] <- zz[s3[,1],1]                                                # correct to full range indexes
    s3[,4] <- zz[s3[,4],1]                                                # correct to full range indexes
    s3 <- s3[which(s3[,1] %in% so[repIn2,1]),]
    if(length(dim(s3)) <2) s3 <- matrix(s3,nrow=1,dimnames=list(NULL,names(s3)))
    so[repIn2,3:5] <- matrix(rep(s3[,3:5],rep(tab,3)),ncol=3)
    so[repIn2,6] <- rep(tab,tab) }
  so[,7] <- so[match(so[,4],so[,1]),6]                                                 # change from n.current to n of best
  chNA <- is.na(so[,2])
  if(any(chNA)) so[which(chNA),3:7] <- NA
  if(ppm) {colnames(so)[5] <- "ppm"
    so[,5] <- 1e6*so[,3]/so[,2] }
  if(length(digSig) >0) if(is.numeric(digSig)) {
    so[,3] <- signif(so[,3],digits=digSig)
    so[,5] <- signif(so[,5],digits=digSig) }
  so[if(initOrder) order(so[,1]) else 1:nrow(so),c(1:3,5:7,4)] }
  
#' @export
.minDif <- function(z,initOrder=TRUE,rat=TRUE){
  ## find closest neighbour to numeric vector 'z', return matrix with index,value,dif,best
  ##  note that 'dif' is reported as absolule !
  ## 'sortRet' .. if TRUE return in orig order of values in 2nd col of dat
  ## however, repeated values will NOT be evalualted correctly !!
  z <- cbind(index=1:length(z),value=z)[order(z),]
  if(rat)
  so <- cbind(z, dif=c(diff(z[,2]),Inf),best=c(z[-1,1],NA))
  sw <- cbind(diA=so[,3], diB=c(Inf,so[-nrow(so),3]))
  alt <- which(sw[,1] > sw[,2])
  if(length(alt) >0) {so[alt,4] <- so[alt-1,1]
    so[alt,3] <- so[alt-1,3]}
  if(rat) {so <- cbind(so,rat=so[,2]/so[match(so[,4],so[,1]),2]) }  
  if(initOrder) so[order(so[,1]),] else so }
    
