#' Normalize Data In Various Modes
#'
#' Generic normalization of 'dat' (by columns), multiple methods may be applied.
#' The choice of normalization procedures must be done with care, plotting the data before and after normalization
#' may be critical to understandig the initial data structure and the effect of the procedure applied.
#' Inappropriate methods chosen may render interpretation of (further) results incorrect.
#'
#' @details
#' In most cases of treating 'Omics'-data one works with the hypothesis that there are no global changes in the structure of all data/columns
#' Under this htpothesis it is very common to assume the the median (via the argument \code{method}) of all samples (ie columns) should remain constant.
#' For examples samples/columns with less signal will be considered as having received 'accidentally' less material (eg due to the imprecision when transfering very small amounts of liquid samples).
#' In consequence, a sample having received only 95% of input material is assumed to give only 95% signal intensity of what may have been expected.
#' Thus, all measures will be multiplied by 1/0.95 (apr 1.053) to compensate for supposed lack of staring material.
#'
#' With the analysis of 'Omics'-data it is very common to work with data on log-scale.
#' In this case the argument \code{mode} should be set to \code{additive}, since adding a constant factor to log-data corresponds to a multiplicative factor on regular scale
#' Please note that (at this point) the methods 'slope', 'exponent', 'slope2Sections' and 'vsn' don't distinguish between additive and proportional modes, but take take the data 'as is'
#' (you may look at the original documenation for more details, see \code{\link{exponNormalize}}, \code{\link{adjBy2ptReg}}, \code{\link[vsn]{justvsn}}).
#'
#' Normalization using \code{method="rowNormalize"} runs \code{\link{rowNormalize}} from this package.
#' In this case, the working hypothesis is, that all values in each row are expected to be the same.
#' This method could be applied when all series of values (ie columns) are replicate measurements of the same sample.
#' THere is also an option for treating sparse data (see argument \code{sparseLim}), which may, hovere, consume much more comptational ressources,
#' in particular, when the value \code{nCombin} is low (compared to the number of samples/columns).
#'
#' Normalization using  \code{method="vsn"} runs \code{\link[vsn]{justvsn}} from \href{https://bioconductor.org/packages/release/bioc/html/vsn.html}{vsn}
#' (this requires a minimum of 42 rows of input-data and having the Bioconductor package vsn installed).
#' Note : Depending on the procedure chosen, the normalized data may appear on a different scale.
#'
#' @param dat matrix or data.frame of data to get normalized
#' @param method (character) may be "mean","median","NULL","none", "trimMean", "rowNormalize", "slope", "exponent", "slope2Sections", "vsn"; When \code{NULL} or 'none' is chosen the input will be returned
#' @param refLines (NULL or numeric) allows to consider only specific lines of 'dat' when determining normalization factors (all data will be normalized)
#' @param refGrp Only the columns indicated will be used as reference, default all columns (integer or colnames)
#' @param mode (character) may be "proportional", "additive";
#'  decide if normalizatio factors will be applies as multiplicative (proportional) or additive; for log2-omics data \code{mode="aditive"} is suggested
#' @param trimFa (numeric, length=1) additional parameters for trimmed mean
#' @param minQuant (numeric) only used with \code{method='rowNormalize'}: optional filter to set all values below given value as \code{NA}; see also \code{\link{rowNormalize}}
#' @param sparseLim (integer) only used with \code{method='rowNormalize'}: decide at which min content of  \code{NA} values the function should go in sparse-mode; see also \code{\link{rowNormalize}}
#' @param nCombin (NULL or integer) only used with \code{method='rowNormalize'}: used only in sparse-mode (ie if content of \code{NA}s higher than content of \code{sparseLim}):
#'    Number of groups of smller matrixes with this number of columns to be inspected initially;
#'    low values (small groups have higher chances of more common elements); see also \code{\link{rowNormalize}}
#' @param omitNonAlignable (logical) only used with \code{method='rowNormalize'}: allow omitting all columns which can't get aligned due to sparseness; see also \code{\link{rowNormalize}}
#' @param maxFact (numeric, length=2) only used with \code{method='rowNormalize'}:  max normalization factor; see also \code{\link{rowNormalize}}
#' @param quantFa (numeric, length=2) additional parameters for quantiles to use with method='slope'
#' @param expFa (numeric, length=1) additional parameters for method='exponent'
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a matrix of normalized data (same dimensions as input)
#' @seealso  \code{\link{rowNormalize}}, \code{\link{exponNormalize}}, \code{\link{adjBy2ptReg}}, \code{\link[vsn]{justvsn}}
#' @examples
#' set.seed(2015); rand1 <- round(runif(300)+rnorm(300,0,2),3)
#' dat1 <- cbind(ser1=round(100:1+rand1[1:100]), ser2=round(1.2*(100:1+rand1[101:200])-2),
#'   ser3=round((100:1 +rand1[201:300])^1.2-3))
#' dat1 <- cbind(dat1, ser4=round(dat1[,1]^seq(2,5,length.out=100)+rand1[11:110],1))
#' dat1[dat1 <1] <- NA
#'   summary(dat1)
#'   dat1[c(1:5,50:54,95:100),]
#' no1 <- normalizeThis(dat1, refGrp=1:3, meth="mean")
#' no2 <- normalizeThis(dat1, refGrp=1:3, meth="trimMean", trim=0.4)
#' no3 <- normalizeThis(dat1, refGrp=1:3, meth="median")
#' no4 <- normalizeThis(dat1, refGrp=1:3, meth="slope", quantFa=c(0.2,0.8))
#' dat1[c(1:10,91:100),]
#' cor(dat1[,3],rowMeans(dat1[,1:2],na.rm=TRUE), use="complete.obs")             # high
#' cor(dat1[,4],rowMeans(dat1[,1:2],na.rm=TRUE), use="complete.obs")             # bad
#' cor(dat1[c(1:10,91:100),4],rowMeans(dat1[c(1:10,91:100),1:2],na.rm=TRUE),use="complete.obs")
#' cor(dat1[,3],rowMeans(dat1[,1:2],na.rm=TRUE)^ (1/seq(2,5,length.out=100)),use="complete.obs")
#' @export
normalizeThis <- function(dat, method="mean", refLines=NULL, refGrp=NULL, mode="proportional", trimFa=NULL, minQuant=NULL, sparseLim=0.4, nCombin=3, omitNonAlignable=FALSE, maxFact=10, quantFa=NULL, expFa=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="normalizeThis")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(!any(sapply(c("additive","add","a"), identical, mode))) mode <- "proportional"

  out <- NULL
  chMe <- is.na(method)
  if(sum(!chMe) <1) stop(fxNa,"Argument 'method' seems empty - nothing to do !")
  method <- if(any(chMe)) naOmit(method)[1] else method[1]
  if(length(dim(dat)) !=2) stop(fxNa,"Expecting matrix or data.frame with >= 2 rows as 'dat' !")
  if(!is.matrix(dat)) dat <- as.matrix(dat)
  if(!is.null(refLines)) if(identical(refLines, 1:nrow(dat))) {refLines <- NULL; if(!silent) message(fxNa,"Omit redundant 'refLines'")}
  ## assemble parameters
  params <- list(refLines=refLines, trimFa=trimFa, useQ=quantFa, useExp=expFa)
  
  ## method specific elements
  if(is.null(refGrp)) { refGrp <- 1:ncol(dat)
  } else if(min(refGrp) > ncol(dat) | max(refGrp) < 1) stop(fxNa," 'refGrp' should be integer vector indicating which columns to be used as reference")
  if(debug) {message(fxNa,"Assemble parameters ; nt1 ;  using method=",method,"   mode=",mode,"   params=",pasteC(unlist(params[-1])))}
  
  if(method %in% "trimMean") {
    params$trimFa <- 0.2
    if(length(trimFa) >0) {if(length(trimFa) ==1) params$trimFa <- trimFa else if(!silent) message(fxNa," invalid 'trimFa', use default 0.2")}
  }
  if(any(sapply(c("rowNormalize","rowN","row"), identical, method))) {
    if(debug) message(fxNa," recognized as method='rowNormalize'")
    params$minQuant <- minQuant
    params$sparseLim <- sparseLim
    params$nCombin <- nCombin
    params$omitNonAlignable <- omitNonAlignable
    params$maxFact <- maxFact
    method <- "row"
  }
  if(method %in% "slope") {
    params$useQ <- c(0.2,0.8)
    if(length(quantFa) >0) {if(length(quantFa)==2) params$useQ <- quantFa else if(!silent) message(fxNa,"Invalid 'quantFa', use default c(0.2,0.8)")}
  }
  if(method %in% "slope2Sections") {
    if(length(params$useQ) !=1) params$useQ <- list(signif(stats::quantile(dat, c(0.05,0.15), na.rm=TRUE), 3),
      signif(stats::quantile(dat, c(0.05,0.15), na.rm=TRUE), 3))
  }
  if(any(sapply(c("exponent","expo","exp"), identical, method))) {
    if(debug) message(fxNa," recognized as method='exponent'")
    if(length(expFa) <1) { useExp <- c(log(c(10:1)), 30, 10, 3)
      params$useExp <- sort(unique(c(round(1 /(1 +abs(useExp -useExp[1])), 4), round(1 +abs(useExp -useExp[1]), 3)))) }
    method <- "exponent"
  }
  if("vsn" %in% method && !requireNamespace("vsn")) {
    method <- "mean"
    if(!silent) message(fxNa,"NOTE: Please install package 'vsn' first from CRAN, resetting argument 'method' to default 'mean'")
  }
  if("vsn" %in% method && (if(length(refLines) >0) length(refLines) < nrow(dat) else FALSE)) {
    if(debug) message(fxNa,"Recognized as method='vsn'")
    params$refLines <- NULL
    if(!silent) message(fxNa,"Ignoring content of 'refLines', since 'vsn' can only normalize considering all data")}
  if(debug) {message(fxNa,"Ready to start .normalize() ; nt2 ;  using method=",method,"   mode=",mode,"   params=",pasteC(unlist(params)))}
  
  ## main normalization
  out <- .normalize(dat, method, mode=mode, param=params, silent=silent, debug=debug, callFrom=fxNa)
  if(inherits(out, "try-error")) { message(fxNa,"Could not run normalization by '",method,"' which gave an error (returning unnormalized)"); out <- dat}
  out }

#' Main Normalization function
#'
#' This function aims to normalize a matrix or data.frame by columns. 
#' It assumes all checks have been done before calling this function.
#' 
#' @param dat matrix or data.frame of data to get normalized
#' @param meth (character) may be "mean","median","NULL","none", "trimMean", "rowNormalize", "slope", "exponent", "slope2Sections", "vsn"; When \code{NULL} or 'none' is chosen the input will be returned
#' @param mode (character) may be "proportional", "additive";
#'  decide if normalizatio factors will be applies as multiplicative (proportional) or additive; for log2-omics data \code{mode="aditive"} is suggested
#' @param param (list) additional parameters
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a numeric vector
#' @seealso \code{\link{normalizeThis}}
#' @examples
#' aa <- matrix(1:12, ncol=3)
#' .normalize(aa,"median",mode="proportional",param=NULL)
#' @export
.normalize <- function(dat, meth, mode, param, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## 'dat' .. matrix (>1 col, >1 li) to be normalized
  ## 'meth' .. method
  ## 'param' .. list with supl parameters (refLines, certain specific for norm methods)
  fxNa <- .composeCallName(callFrom, newNa=".normalize")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  mode <- if(identical(mode,"additive")) "Add" else "Prop"       # reduce to short name
  if(identical(meth,"average")) meth <- "mean"
  asRefL <- (length(param$refLines) < nrow(dat) && !is.null(param$refLines))
  datRef <- if(asRefL) {if(length(param$refLines) >1) dat[param$refLines,] else matrix(dat[param$refLines,],nrow=1)} else NULL
  if("vsn" %in% meth) {
    chPa <- requireNamespace("vsn", quietly=TRUE)
    if(!chPa) { meth <- "mean"
      if(!silent) message(fxNa, "NOTE: Package 'vsn' not found ! Please install first from Bioconductor; resetting argument 'meth' to default 'mean'")
    }
    if(nrow(if(asRefL) datRef else dat) <42) message(callFrom," PROBLEM : Too few lines of data to run 'vsn' ! ")}
  ## Some methods have not yet been adopted/declined to additive/proportional mode
  if(meth %in% c("none", "slope", "exponent", "slope2Sections", "vsn")) mode <- NULL
  meth <- paste0(meth,mode)

  switch(meth,
    none=dat,
    meanAdd= mean(if(asRefL) datRef else dat, na.rm=TRUE) + dat - rep(colMeans(if(asRefL) datRef else dat, na.rm=TRUE), each=nrow(dat)),
    meanProp= mean(if(asRefL) datRef else dat, na.rm=TRUE) * dat / rep(colMeans(if(asRefL) datRef else dat, na.rm=TRUE), each=nrow(dat)),
    trimMeanAdd=mean(if(asRefL) datRef else dat, trim=param$trimFa, na.rm=TRUE) + dat - rep(apply(
      if(asRefL) datRef else dat, 2, mean, trim=param$trimFa, na.rm=TRUE), each=nrow(dat)),
    trimMeanProp=mean(if(asRefL) datRef else dat, trim=param$trimFa, na.rm=TRUE) * dat / rep(apply(
      if(asRefL) datRef else dat, 2, mean, trim=param$trimFa, na.rm=TRUE), each=nrow(dat)),
    medianAdd=stats::median(if(asRefL) datRef else dat, na.rm=TRUE) + dat - rep(apply(
      if(asRefL) datRef else dat, 2, stats::median, na.rm=TRUE), each=nrow(dat)),
    medianProp=stats::median(if(asRefL) datRef else dat,na.rm=TRUE) * dat / rep(apply(
      if(asRefL) datRef else dat, 2, stats::median, na.rm=TRUE), each=nrow(dat)),
    rowAdd= rowNormalize(dat=dat, method=meth, refLines=param$refLines, refGrp=param$refGrp, proportMode="additive", minQuant=param$minQuant,
      sparseLim=param$sparseLim, nCombin=param$nCombin, omitNonAlignable=param$omitNonAlignable, maxFact=param$maxFact, silent=silent, debug=debug, callFrom=fxNa),
    rowProp=rowNormalize(dat=dat, method=meth, refLines=param$refLines, refGrp=param$refGrp, proportMode="proportional", minQuant=param$minQuant,
      sparseLim=param$sparseLim, nCombin=param$nCombin, omitNonAlignable=param$omitNonAlignable, maxFact=param$maxFact, silent=silent, debug=debug, callFrom=fxNa),
    slope=.normConstSlope(mat=dat, useQuant=param$useQ, refLines=param$refLines, diagPlot=FALSE),
    exponent=try(exponNormalize(dat, useExpon=param$useExp, refLines=param$refLines)$datNor, silent=TRUE),
    slope2Sections=try(adjBy2ptReg(dat, lims=param$useQ, refLines=param$refLines), silent=TRUE),
    vsn=if(requireNamespace("vsn")) try(vsn::justvsn(dat), silent=TRUE) else NULL ) 
  }

#' Normalize columns of 2dim matrix to common linear regression fit
#'
#' This function aims to normalize columns of 2dim matrix to common linear regression fit within range of 'useQuant'
#' 
#' @param mat matrix or data.frame of data to get normalized
#' @param useQuant (numeric) quantiles to use
#' @param refLines (NULL or numeric) allows to consider only specific lines of 'dat' when determining normalization factors (all data will be normalized)
#' @param diagPlot (logical) draw diagnistic plot
#' @param plotLog (character) indicate which axis shousl be diplayed on log-scale, may be 'x', 'xy' or 'y'
#' @param datName  (character) use as title in diag plot
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a numeric vector
#' @examples
#' aa <- matrix(1:12, ncol=3)
#' @seealso \code{\link{normalizeThis}}
##' @export
.normConstSlope <- function(mat, useQuant=c(0.2,0.8), refLines=NULL, diagPlot=TRUE, plotLog="", datName=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## normalize columns of 2dim matrix to common linear regression fit within range of 'useQuant'
  ## returns normalized data (2dim matrix)
  ## in case of matrixes, data will be normalized by columns, but only the average of all indiv regression-lines will be shown on graph
  ## 'useQuant' ..defines window of data to be considered for normalizing
  ## 'refLines' ..lines to use for determining normalization factors
  ## 'datName' for use as title in diag plot
  fxNa <- .composeCallName(callFrom,newNa=".normConstSlope")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  msg1 <- "'useQuant' should be vector of two numeric values between 0 & 1"
  msg2 <- paste("'mat' should be matrix with two dimensions. Here it appears as ")
  if(!is.numeric(mat)) stop(fxNa,msg2,class(mat)," with ",mode(mat)," data of ",length(dim(mat))," dims")
  if(sum(is.finite(useQuant)) != 2 || length(useQuant) !=2) {message(fxNa,msg1); useQuant=c(0.2,0.8)}
  if(useQuant[1] < 0 || useQuant[1] > 1) {message(msg1); useQuant=c(0.2,0.8)}
  if(plotLog %in% c("x","xy")) {matOri <- mat; mat <- log(mat); message(fxNa," ..setting data to log")}
  quantVal <- apply(mat, 2, stats::quantile, useQuant,na.rm=TRUE)
  tmp <- apply(mat, 2, function(x) {x <- naOmit(x); x[x >= stats::quantile(x,min(useQuant),na.rm=TRUE) & x <= stats::quantile(x,max(useQuant),na.rm=TRUE)]})
  if(!is.list(tmp)) tmp <- as.data.frame(tmp)
  regr <- sapply(if(length(refLines) < nrow(mat) && !is.null(refLines)) tmp[refLines,] else tmp, .datSlope, toNinX=TRUE)
  regr[1,] <- regr[1,] + apply(rbind(quantVal[1,],mat), 2, function(x) sum(x[-1] < x[1], na.rm=TRUE))    # correct intercept for no of quantile-omitted data
  regrM <- rowMeans(regr, na.rm=FALSE)
  normD <- apply(rbind(regr, mat), 2, function(x) (x[-1*(1:2)] -(regrM[1]-x[1])/x[2]) *x[2]/regrM[2])
  if(plotLog %in% c("x","xy")) normD <- exp(normD)
  if(diagPlot) {
    msg2 <- paste(fxNa,"Unknow argument content ('",plotLog,"') for 'plotLog'; resetting to default no log",sep="")
    if(length(plotLog) !=1) { message(fxNa,msg2); plotLog <- ""}
    if(!(plotLog %in% c("","x","y","xy"))) {message(fxNa,msg2); plotLog <- ""}
    yLab <- "number of values"
    xLab <- "sorted values"
    xDat <- sort(normD[which(is.finite(normD[,1])),1])
    if(length(refLines) < nrow(mat) && !is.null(refLines)) { chG <- try(graphics::plot(1:sum(is.finite(normD[refLines,1])) ~ sort(normD[which(is.finite(normD[refLines,1])),1]),
      type="s", main=paste("normalizing ",datName),log=plotLog,col=3,xlab=xLab,ylab=yLab), silent=TRUE)
      if(inherits(chG, "try-error")) message(fxNa,"UNABLE to draw figure !") else { 
        graphics::points(sort(normD[which(is.finite(normD[-1*refLines,1])),1]), 1:sum(is.finite(normD[-1*refLines,1])), type="s",col=grDevices::grey(0.4))}
    } else chG <- try(graphics::plot(1:sum(is.finite(normD[,1])) ~ sort(normD[which(is.finite(normD[,1])),1]), type="s",main=paste("normalizing ",datName), log=plotLog,col=3,xlab=xLab,ylab=yLab), silent=TRUE)
    if(inherits(chG, "try-error")) message(fxNa,"UNABLE to draw figure !") else {    
      for(ii in 2:ncol(normD)) if(length(refLines) < nrow(mat) && !is.null(refLines)) {
        graphics::points(1:sum(is.finite(normD[refLines,ii]))~ sort(normD[which(is.finite(normD[refLines,ii])),ii]),type="s",col=ii+2)
        graphics::points(sort(normD[which(is.finite(normD[-1*refLines,ii])),1]), 1:sum(is.finite(normD[-1*refLines,ii])),type="s",col=grDevices::grey(0.4))
      } else graphics::points(1:sum(is.finite(normD[,ii]))~ sort(normD[which(is.finite(normD[,ii])),ii]), type="s",col=ii+2)
      graphics::abline(h=useQuant*sum(!is.na(normD))/ncol(normD), lty=3, col=grDevices::grey(0.6))
      if(plotLog %in% c("x","xy")) graphics::curve(log(regrM[1])*(regrM[2]) +regrM[1], lty=2,col=2,add=TRUE) else graphics::abline(regrM[1],regrM[2],lty=2,col=2)
    } }
  normD }

