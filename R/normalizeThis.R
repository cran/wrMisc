#' Normalize data in various modes
#'
#' Generic normalization of 'dat' (by columns), multiple methods may be applied.
#' The choice of normalization procedures must be done with care, plotting the data before and after normalization 
#' may be critical to understandig the initial data structure and the effect of the procedure applied. 
#' Inappropriate methods chosen may render interpretation of (further) results incorrect.
#' Normalization using the method \code{vsn} runs \code{\link[vsn]{justvsn}} from \href{https://bioconductor.org/packages/release/bioc/html/vsn.html}{vsn} 
#' (this requires a minimum of 42 rows of input-data).
#' Note : Depending on the procedure chosen, the normalized data may appear on a different scale.
#' @param dat matrix or data.frame
#' @param method (character) may be "mean","median","NULL","none", "trimMean", "slope", "exponent", "slope2Sections", "vsn"; When NULL or 'none' is chosen the input will be returned
#' @param refLines (NULL or numeric) allows to consider only specific lines of 'dat' when determining normalization factors (all data will be normalized)
#' @param refGrp Only the columns indicated will be used as reference, default all columns (integer or colnames)
#' @param trimFa (numeric, length=1) additional parameters for trimmed mean
#' @param quantFa (numeric, length=2) additional parameters for quantiles to use with method='slope'
#' @param expFa (numeric, length=1) additional parameters for method='exponent'
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message produced
#' @return matrix of normalized data
#' @seealso \code{\link{exponNormalize}}, \code{\link{adjBy2ptReg}}, \code{\link[vsn]{justvsn}} 
#' @examples
#' set.seed(2015); rand1 <- round(runif(300)+rnorm(300,0,2),3)
#' dat1 <- cbind(ser1=round(100:1+rand1[1:100]),ser2=round(1.2*(100:1+rand1[101:200])-2),
#'   ser3=round((100:1+rand1[201:300])^1.2-3))
#' dat1 <- cbind(dat1,ser4=round(dat1[,1]^seq(2,5,length.out=100)+rand1[11:110],1))
#' dat1[dat1 <1] <- NA
#'   summary(dat1)
#'   head( .normalize(dat1,"mean",list()))
#'   dat1[c(1:5,50:54,95:100),]
#' no1 <- normalizeThis(dat1,refGrp=1:3,meth="mean")
#' no2 <- normalizeThis(dat1,refGrp=1:3,meth="trimMean",trim=0.4)
#' no3 <- normalizeThis(dat1,refGrp=1:3,meth="median")
#' no4 <- normalizeThis(dat1,refGrp=1:3,meth="slope",quantFa=c(0.2,0.8))
#' dat1[c(1:10,91:100),]
#' cor(dat1[,3],rowMeans(dat1[,1:2],na.rm=TRUE),use="complete.obs")             # high
#' cor(dat1[,4],rowMeans(dat1[,1:2],na.rm=TRUE),use="complete.obs")             # bad
#' cor(dat1[c(1:10,91:100),4],rowMeans(dat1[c(1:10,91:100),1:2],na.rm=TRUE),use="complete.obs")
#' cor(dat1[,3],rowMeans(dat1[,1:2],na.rm=TRUE)^ (1/seq(2,5,length.out=100)),use="complete.obs")
#' @export
normalizeThis <- function(dat,method="mean",refLines=NULL,refGrp=NULL,trimFa=NULL,quantFa=NULL,expFa=NULL,silent=FALSE,callFrom=NULL){
  fxNa <- "normalizeThis"
  out <- NULL  
  chMe <- is.na(method)
  if(sum(!chMe) <1) stop(" argument 'method' seems empty - nothing to do !")
  method <- if(any(chMe)) wrMisc::naOmit(method)[1] else method[1] 
  if(length(dim(dat)) !=2) stop(" expecting matrix or data.frame with >= 2 rows as 'dat' !")
  if(!is.matrix(dat)) dat <- as.matrix(dat)
  if(!is.null(refLines)) if(identical(refLines,1:nrow(dat))) {refLines <- NULL; message(fxNa," omit redundant 'refLines'")}
  ## assemble parameters
  params <- list(refLines=refLines,trimFa=trimFa,useQ=quantFa,useExp=expFa)
  ## method specific elements
  if(is.null(refGrp)) { refGrp <- 1:ncol(dat)
  } else if(min(refGrp) > ncol(dat) | max(refGrp) < 1) stop(fxNa," 'refGrp' should be integer vector indicating which columns to be used as reference")
  if(method %in% "trimMean") {
    params$trimFa <- 0.2
    if(length(trimFa) >0) {if(length(trimFa) ==1) params$trimFa <- trimFa else if(!silent) message(fxNa," invalid 'trimFa', use default 0.2")}
  }
  if(method %in% "slope") {
    params$useQ <- c(0.2,0.8)
    if(length(quantFa) >0) {if(length(quantFa)==2) params$useQ <- quantFa else if(!silent) message(fxNa," invalid 'quantFa', use default c(0.2,0.8)")}
  }
  if(method %in% "slope2Sections") {
    if(length(params$useQ) !=1) params$useQ <- list(signif(stats::quantile(dat,c(0.05,0.15),na.rm=TRUE),3),
      signif(stats::quantile(dat,c(0.05,0.15),na.rm=TRUE),3))
  }
  if(method %in% "exponent") {
     if(length(expFa) <1) { useExp <- c(log(c(10:1)),30,10,3)
       params$useExp <- sort(unique(c(round(1/(1+abs(useExp-useExp[1])),4),round(1+abs(useExp-useExp[1]),3)))) }
  }
  if("vsn" %in% method & (if(length(refLines) >0) length(refLines) <nrow(dat) else FALSE)) {
    pram$refLines <- NULL
    if(!silent) message(fxNa," ignoring content of 'refLines', since 'vsn' can only normalize considering all data")}  
  ## main normalization
  out <- .normalize(dat,method,param=params,callFrom=fxNa)
  if("try-error" %in% class(out)) { message(fxNa," Could not run normalization by '",method,"' which gave an error (return unnormalized)"); out <- dat}
  out }

#' @export
.normalize <- function(dat,meth,param,silent=FALSE,callFrom=NULL){
  ## 'dat' .. matrix (>1 col, >1 li) to be normalized
  ## 'meth' .. method
  ## 'param' .. list with supl parameters (refLines, certain specific for norm methods)
  fxNa <- ".normalize"
  if(identical(meth,"average")) meth <- "mean"
  asRefL <- (length(param$refLines) < nrow(dat) & !is.null(param$refLines))
  datRef <- if(asRefL) {if(length(param$refLines) >1) dat[param$refLines,] else matrix(dat[param$refLines,],nrow=1)} else NULL
  if("vsn" %in% meth & (nrow(if(asRefL) datRef else dat)) <42) message(callFrom,
    " PROBLEM : Too few lines of data to run 'vsn' ! ")
  switch(meth,
    none=dat,  
    mean= sum(dat,na.rm=TRUE)*dat/(matrix(rep(colMeans(if(asRefL) datRef else dat,na.rm=TRUE),
      each=nrow(dat)),nrow=nrow(dat))*sum(!is.na(dat))),
    trimMean=mean(dat,trim=param$trimFa,na.rm=TRUE)* dat/matrix(rep(apply(
      if(asRefL) datRef else dat,2,mean,trim=param$trimFa,na.rm=TRUE),each=nrow(dat)),nrow=nrow(dat)),
    median=stats::median(dat,na.rm=TRUE)*dat/matrix(rep(apply(if(asRefL) datRef else dat,2,stats::median,na.rm=TRUE),each=nrow(dat)),nrow=nrow(dat)),
    slope=.normConstSlope(mat=dat,useQuant=param$useQ,refLines=param$refLines,diagPlot=FALSE),
    exponent=try(exponNormalize(dat,useExpon=param$useExp,refLines=param$refLines)$datNor),
    slope2Sections=try(adjBy2ptReg(dat,lims=param$useQ,refLines=param$refLines)),
    vsn=try(vsn::justvsn(dat)) ) 
  }

#' @export
.normConstSlope <- function(mat,useQuant=c(0.2,0.8),refLines=NULL,diagPlot=TRUE,plotLog="",datName=NULL,silent=FALSE,callFrom=NULL){
  ## normalize columns of 2dim matrix to common linear regression fit within range of 'useQuant'
  ## returns normalized data (2dim matrix)
  ## in case of matrixes, data will be normalized by columns, but only the average of all indiv regression-lines will be shown on graph
  ## 'useQuant' ..defines window of data to be considered for normalizing
  ## 'refLines' ..lines to use for determining normalization factors
  ## 'datName' for use as title in diag plot
  # cat(" mat: "); cat(str(mat),"\n")
  fxNa <- .composeCallName(callFrom,newNa=".normConstSlope")
  msg1 <- " 'useQuant' should be vector of two numeric values between 0 & 1"
  msg2 <- paste(" 'mat' should be matrix with two dimensions. Here it appears as ")
  if(!is.numeric(mat)) stop(fxNa,msg2,class(mat)," with ",mode(mat)," data of ",length(dim(mat))," dims")
  if(sum(is.finite(useQuant)) != 2 | length(useQuant) !=2) {message(fxNa,msg1); useQuant=c(0.2,0.8)}
  if(useQuant[1] < 0 | useQuant[1] > 1) {message(msg1); useQuant=c(0.2,0.8)}
  if(plotLog %in% c("x","xy")) {matOri <- mat; mat <- log(mat); message(fxNa," ..setting data to log")}
  quantVal <- apply(mat,2,stats::quantile,useQuant,na.rm=TRUE)
  tmp <- apply(mat,2,function(x) {x <- naOmit(x); x[x >= stats::quantile(x,min(useQuant),na.rm=TRUE) & x <= stats::quantile(x,max(useQuant),na.rm=TRUE)]})
  if(!is.list(tmp)) tmp <- as.data.frame(tmp)
  regr <- sapply(if(length(refLines) < nrow(mat) & !is.null(refLines)) tmp[refLines,] else tmp,.datSlope,toNinX=TRUE)
  regr[1,] <- regr[1,] + apply(rbind(quantVal[1,],mat),2,function(x) sum(x[-1] < x[1],na.rm=TRUE))    # correct intercept for no of quantile-omitted data
  regrM <- rowMeans(regr,na.rm=FALSE)
  normD <- apply(rbind(regr, mat),2,function(x) (x[-1*(1:2)]-(regrM[1]-x[1])/x[2]) *x[2]/regrM[2])
  if(plotLog %in% c("x","xy")) normD <- exp(normD)
  if(diagPlot) {
    msg2 <- paste(fxNa," Unknow argument content ('",plotLog,"') for 'plotLog'; resetting to default no log",sep="")
    if(length(plotLog) !=1) { message(fxNa,msg2); plotLog <- ""}
    if(!(plotLog %in% c("","x","y","xy"))) {message(fxNa,msg2); plotLog <- ""}
    yLab <- "number of values"
    xLab <- "sorted values"
    xDat <- sort(normD[which(is.finite(normD[,1])),1])
    if(length(refLines) < nrow(mat) & !is.null(refLines)) { graphics::plot(1:sum(is.finite(normD[refLines,1])) ~ sort(normD[which(is.finite(normD[refLines,1])),1]),
      type="s",main=paste("normalizing ",datName),log=plotLog,col=3,xlab=xLab,ylab=yLab)
      graphics::points(sort(normD[which(is.finite(normD[-1*refLines,1])),1]),1:sum(is.finite(normD[-1*refLines,1])),type="s",col=grDevices::grey(0.4))
    } else graphics::plot(1:sum(is.finite(normD[,1])) ~ sort(normD[which(is.finite(normD[,1])),1]),type="s",main=paste("normalizing ",datName),log=plotLog,col=3,xlab=xLab,ylab=yLab)
    for(ii in 2:ncol(normD)) if(length(refLines) < nrow(mat) & !is.null(refLines)) {
      graphics::points(1:sum(is.finite(normD[refLines,ii]))~ sort(normD[which(is.finite(normD[refLines,ii])),ii]),type="s",col=ii+2)
      graphics::points(sort(normD[which(is.finite(normD[-1*refLines,ii])),1]),1:sum(is.finite(normD[-1*refLines,ii])),type="s",col=grDevices::grey(0.4))
    } else graphics::points(1:sum(is.finite(normD[,ii]))~ sort(normD[which(is.finite(normD[,ii])),ii]),type="s",col=ii+2)
    graphics::abline(h=useQuant*sum(!is.na(normD))/ncol(normD),lty=3,col=grDevices::grey(0.6))
    if(plotLog %in% c("x","xy")) graphics::curve(log(regrM[1])*(regrM[2])+regrM[1],lty=2,col=2,add=TRUE) else graphics::abline(regrM[1],regrM[2],lty=2,col=2)
    }
  normD }  
    
