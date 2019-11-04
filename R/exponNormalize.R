#' Normalize by adjusting exponent
#'
#' This function normalizes 'dat' by optimizing exponent function (ie dat ^exp) to fit best to 'ref' (default: average of each line of 'dat').
#'
#' @param dat matrix or data.frame of numeric data to be normalized
#' @param useExpon (numeric vector or matrix) exponent values to be tested
#' @param dynExp (logical) require 'useExpon' as 2 values (matrix), will gradually increase exponent from 1st to 2nd; may be matrix or data.frame for dynamic, 
#' in this case 1st line for exp for lowest data, 2nd line for highest
#' @param nStep (integer) number of exponent variations (steps) when testing range from-to
#' @param startExp (numeric)
#' @param simMeas (character) similarity metric to be used (so far only "cor"), if rSquare=TRRUE, the r-squared will be returned
#' @param refDat (matrix or data.frame) if null average of each line from 'dat' will be used as reference in similarity measure
#' @param refGrp (factor) designing which col of 'ref' should be used with which col of 'dat' (length equal to number of cols in 'dat').  Note: 'refGrp' not yet coded optimally to extract numeric part of character vector, protential problems when all lines or cols of dat are NA
#' @param refLines (NULL or integer) optional subset of lines to be considered (only) when determining normalization factors
#' @param rSquare (logical) if TRUE, add r-squared
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix of normalized data
#' @seealso more eveolved than \code{\link[wrMisc]{normalizeThis}} with arugment set to 'exponent'
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),nc=10)
#' head(rowGrpCV(dat1,gr=gl(4,3,labels=LETTERS[1:4])[2:11]))
#' set.seed(2016); dat1 <- c(0.1,0.2,0.3,0.5)*rep(c(1,10),each=4)
#' dat1 <- matrix(round(c(sqrt(dat1),dat1^1.5,3*dat1+runif(length(dat1))),2),nc=3)
#' dat2a <- exponNormalize(dat1[,1],useExpon=2,nSte=1,refD=dat1[,3])
#' layout(matrix(1:2,nc=2))
#' plot(dat1[,1],dat1[,3],type="b",main="init",ylab="ref") 
#' plot(dat2a$datNor[,1],dat1[,3],type="b",main="norm",ylab="ref")
#' dat2b <- exponNormalize(dat1[,1],useExpon=c(1.7,2.3),nSte=5,refD=dat1[,3])
#' plot(dat1[,1],dat1[,3],type="b",main="init",ylab="ref")
#' plot(dat2b$datNor[,1],dat1[,3],type="b",main="norm",ylab="ref")
#' 
#' dat2c <- exponNormalize(dat1[,-3],useExpon=matrix(c(1.7,2.3,0.6,0.8),nc=2),nSte=5,refD=dat1[,3]);
#' plot(dat1[,1],dat1[,3],type="b",main="init",ylab="ref ")
#' plot(dat2c$datNor[,1],dat1[,3],type="b",main="norm 1",ylab="ref")
#' plot(dat1[,2],dat1[,3],type="b",main="init",ylab="ref")
#' plot(dat2c$datNor[,2],dat1[,3],type="b",main="norm 2",ylab="ref"); 
#' @export
exponNormalize <- function(dat,useExpon,dynExp=TRUE,nStep=20, startExp=1,simMeas="cor",refDat=NULL,refGrp=NULL,refLines=NULL,rSquare=FALSE,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="exponNormalize")
  dimDat <- dim(dat)
  if(!is.matrix(dat)) dat <- as.matrix(dat)
  if(is.null(refDat)) refDat <- matrix(rowMeans(if(length(refLines) < nrow(dat) & !is.null(refLines)) dat[refLines,] else dat,na.rm=TRUE),
    ncol=1,dimnames=list(NULL,colnames(dat))) else { cat("adj 'refDat to matrix' \n")
    if(length(dim(refDat)) <2) refDat <- as.matrix(as.numeric(refDat)) }
  if(is.null(refGrp)) refGrp <- rep(1,ncol(dat))
  if(length(refGrp) != ncol(dat)) message(fxNa," 'refGrp' (",length(refGrp),") doesn't fit to 'dat' (",ncol(dat)," cols)")
  if(max(as.numeric(refGrp),na.rm=TRUE) > ncol(dat)) {
    message(fxNa," 'refGrp' suggests columns not found in 'dat' !") }
  useExpon <- unique(naOmit(useExpon))
  ## main
  .sw <- function(useExpon,startExp,nStep) {     # function for defining new series of 'useExpon'
    meth <- paste("m",0 +(length(useExpon) >1) + 2*all(startExp <1),sep="")
    switch(meth,
      m0=seq(useExpon,startExp,length.out=nStep),
      m1=seq(useExpon[1],useExpon[2],length.out=nStep),
      m2=seq(startExp,useExpon,length.out=nStep),
      m3=seq(useExpon[1],useExpon[2],length.out=nStep) ) }
  if(length(dim(useExpon)) >1 & dynExp & nStep >1) {    # diff/indep expon series for each col of data
    useExpon <- apply(useExpon,2,.sw,startExp,nStep)
    if(!silent) message(fxNa," column-specific exponent testing of ",nrow(useExpon)," x ",ncol(useExpon)," exponents")
  } else {
    if(nStep >1) useExpon <- .sw(useExpon,startExp,nStep)
    if(!silent) message(fxNa," static exponent series (length ",length(useExpon),")")}
  if(!is.matrix(useExpon)) useExpon <- matrix(rep(useExpon,ncol(dat)),ncol=ncol(dat))
  expoNor <- list()
  for(i in 1:nrow(useExpon)) expoNor[[i]] <- dat^matrix(rep(useExpon[i,],each=nrow(dat)),ncol=ncol(dat))
  corVal <- if(identical(simMeas,"cor")) sapply(expoNor,function(x) apply(x,2,stats::cor,y=refDat,use="complete.obs")) else {
    corVal <- NULL; message(fxNa," PROBLEM : unknown similarity measure !!")}
  if(!is.matrix(corVal)) corVal <- matrix(corVal,ncol=ncol(dat))
  if(all(dim(corVal) == dim(t(useExpon)))) corVal <- t(corVal)
  dimnames(corVal) <- list(paste("exp",useExpon[,1],sep="_"),colnames(dat))
  bestExpPo <- apply(corVal,2,which.max)
  bestExp <- apply(rbind(bestExpPo,useExpon),2,function(x) x[-1][x[1]])
  datNor <- matrix(nrow=nrow(dat),ncol=ncol(dat),dimnames=dimnames(dat))
  for(i in 1:ncol(dat)) datNor[,i] <- dat[,i]^bestExp[i]
  out <- list(bestExp=bestExp, datNor=datNor, allSimil = if(rSquare) corVal^2 else corVal)
  out }
   
