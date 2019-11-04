#' Linear rescaling of numeric vertor or matrix
#'
#' \code{adjBy2ptReg} takes data within window defined by 'lims' and determines linear transformation so that these points get the regression characteristics 'regrTo', 
#' all other points (ie beyond the limits) will follow the same transformation.
#' In other words, this function performs 'linear rescaling', by adjusting (normalizing) the vector 'dat' by linear regression so that points falling in 'lims'
#' (list with upper & lower boundaries) will end up as 'regrTo'.
#'
#' @param dat numeric vector, matrix or data.frame
#' @param lims (list, length=2) should be list giving limits (list(lo=c(min,max),hi=c(min,max)) in data allowing identifying which points will be used for determining slope & offset
#' @param regrTo (numeric, length=2) to which characteristics data should be regressed
#' @param refLines (NULL or integer) optional subselection of lines of dat (will be used internal as refDat)
#' @param callFrom (character) for better tracking of use of functions
#' @return matrix with normalized values
#' @seealso \code{\link{normalizeThis}}
#' @examples
#' set.seed(2016); dat1 <- round(runif(50,0,100),1)
#' ## extreme values will be further away :
#' adjBy2ptReg(dat1,lims=list(c(5,9),c(60,90)))
#' plot(dat1,adjBy2ptReg(dat1,lims=list(c(5,9),c(60,90))))
#' @export
adjBy2ptReg <- function(dat,lims,regrTo=c(0.1,0.9),refLines=NULL,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="adjBy2ptReg")
  if(length(refLines) >0) {
    if(is.character(refLines) & !is.null(rownames(dat))) refLines <- match(refLines,names(dat)) else {
    refLines <- as.integer(refLines)
    refLines <- refLines[which(refLines >0 & refLines < length(dat))]}}
  dat1 <- if(length(refLines) >0) dat[refLines] else dat
  if(is.null(names(dat1))) names(dat1) <- 1:length(dat1)
  msg <- "'lims' should be list giving limits (list(lo=c(min,max),hi=c(min,max))"
  if(!is.list(lims)) lims <- as.list(lims)
  if(length(lims) != 2) stop(msg)
  lim2 <- if(all(sapply(lims,length)==1)) {
    list(lo=which(dat1 == lims[[1]]), hi=which(dat1 == lims[[1]]))
  } else {
    if(all(sapply(lims,length)==2)) { list(
      lo=which(dat1 >= min(lims[[1]],na.rm=TRUE) & dat1 <= max(lims[[1]],na.rm=TRUE)),
      hi=which(dat1 >= min(lims[[2]],na.rm=TRUE) & dat1 <= max(lims[[2]],na.rm=TRUE)))
    } else stop(" cannot figure out 'lims' !\n ",msg)}
  chLe <- sapply(lim2,length)
  if(any(chLe <1)) message(fxNa,"limits seem too tight : lo ",chLe[1],"  & hi ",chLe[2],"  entries found !")
  lim3 <- sapply(lim2,function(x) sum(dat1[x],na.rm=TRUE)/length(x))               # mean value at lo & hi indexing
  normFact <- if(length(lims) ==1) (regrTo[1]/lim3[1]) else (regrTo[2]-regrTo[1])/(lim3[2]-lim3[1])       # slope
  normFact <- c(k=normFact, d= if(length(lims) ==1) 0 else regrTo[1] -normFact*lim3[1])
  norD <- dat1*normFact[1] + normFact[2]
  dat[which(is.finite(dat))] <- as.numeric(norD)
  dat }
 
#' @export
.datSlope <- function(dat,typeOfPlot="sort",toNinX=FALSE,plotData=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa=".datSlope")
  if(length(typeOfPlot) >1) {
    message(fxNa,"invalid entry for typeOfPlot ",paste(typeOfPlot,collapse=", ")," (use 1st)")
    typeOfPlot <- typeOfPlot[1] }
  dat <- as.numeric(naOmit(sort(dat)))
  y <- if(tolower(typeOfPlot) %in% "cumsum") cumsum(sort(dat)) else sort(dat)
  y <- data.frame(out=y,n=1:length(dat))
  out <- if(toNinX) stats::lm(n ~ out, data=y)$coefficients else  stats::lm(out ~ n, data=y)$coefficients
  if(plotData) {if(toNinX) {
    graphics::plot(y,type="s",ylab=if(tolower(typeOfPlot) %in% "cumsum") "cumsum" else "sorted")
    for(i in 2:ncol(out)) graphics::points(y); graphics::abline(out,lty=2,col=2)
  } else {
    graphics::plot(1:nrow(y),y[,1],type="s",ylab=if(tolower(typeOfPlot) %in% "cumsum") "cumsum" else "sorted")
    graphics::points(1:nrow(y),y[,1]); graphics::abline(out,lty=2,col=2) } }
  names(out) <- c("intercept","slope")
  out }

#' @export
.checkRegrArguments <- function(inData,refList,regreTo,callFrom=NULL){
  ## argument checking for regrBy1or2point() & regrMultBy1or2point()
  ## 'inData' should be
  ## returns corrected/adjusted 'refList' and 'regreTo' (as list)
  fxNa <- .composeCallName(callFrom,newNa=".checkRegrArguments")
  if(!is.list(refList)) refList <- list(refList)
  if(is.list(inData) & !is.data.frame(inData)) message(fxNa,"Potential problem : not expecting list (length=",length(inData),") at place of 'inDat' !!")
  if(min(sapply(refList,length)) <2) {
    warning(fxNa,sum(sapply(refList,length) <2)," entries of 'refLst' contain less than 2 array-positions (well-names)")
    refList <- refList[which(sapply(refList,length) >1)] }
  datIsMa <- !is.null(dim(inData)) 
  checkRefFields <- unlist(refList) %in% (if(datIsMa) rownames(inData) else names(inData))
  if(sum(checkRefFields) <1) {
    message(fxNa," 'refList'  ",paste(unlist(refList),collapse=" "))
    message(" head(names(inDat) ",paste(if(datIsMa) utils::head(rownames(inData)) else utils::head(names(inData)),collapse=" ")," ...")
    stop(" none of the fields given in 'refList' appear in names of 'inDat' !") }
  if(sum(checkRefFields) < sum(sapply(refList,length))) message(fxNa," some of the fields given in 'refLst' do not appear in names of 'inDat' !")
  if(length(refList) > length(regreTo)) message(fxNa," need as many values 'regrTo' as types (of samples) given in 'refLst' !")
  list(refLst=refList,regrTo=regreTo) }
   
