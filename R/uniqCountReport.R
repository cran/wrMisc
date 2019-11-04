#' Report number of unique and redundant elements (optional figure)
#'
#' Make report about number of unique and redundant elements of vector 'dat'.
#' Note : fairly slow for long vectors !!
#' @param dat (charcter or numeric vector) main input where number of unique (and redunant) should be determined
#' @param frL (logical) optional (re-)introducing results from \code{duplicated} to shorten time of execution 
#' @param plotDispl (logical) decide if pie-type plot should be produced
#' @param tit (character) optional title in plot
#' @param col (character) custom colors in pie
#' @param radius (numeric) radius passed to  \code{pie} 
#' @param sizeTo (numeric or charcter) optional reference group for size-population relative adjusting overall surface of pie 
#' @param clockwise (logical) argument passed to pie 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return vector with counts of n (total), nUnique (wo any repeated), nHasRepeated (first of repeated), nRedundant), optional figure
#' @seealso \code{\link{correctToUnique}}, \code{\link[base]{unique}}
#' @examples
#' layout(1:2)
#' uniqCountReport(rep(1:7,1:7),plot=TRUE)
#' uniqCountReport(rep(1:3,1:3),plot=TRUE,sizeTo=rep(1:7,1:7))      
#' @export
uniqCountReport <- function(dat,frL=NULL,plotDispl=FALSE,tit=NULL,col=NULL,radius=0.9,sizeTo=NULL,clockwise=FALSE,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="uniqCountReport")
  if(is.null(frL)) frL <- duplicated(dat,fromLast=TRUE)
  nUniQ <- length(unique(dat[which(frL)]))
  out <- c(n=length(dat), nUnique=length(unique(dat))-nUniQ, nHasRepeated=nUniQ, nRedundant=NA )
  out[4] <- out[1] -sum(out[2:3],na.rm=TRUE)
  if(plotDispl) .plotCountPie(out,tit=tit,col=col,radius=radius,sizeTo=sizeTo,clockwise=clockwise,silent=silent,callFrom=fxNa)
  out }
  
#' @export
.plotCountPie <- function(count,tit=NULL,col=NULL,radius=0.9,sizeTo=NULL,clockwise=FALSE,silent=FALSE,callFrom=NULL) {
  ## plotting for .plotCountPie()
  fxNa <- .composeCallName(callFrom,newNa=".plotCountPie")
  if(is.null(col)) col <- c(3,4,2) 
  if(length(sizeTo) >0) {if(is.character(sizeTo) | length(sizeTo) >1) sizeTo <- length(sizeTo)
    radius <- radius*sqrt(count[1]/sizeTo)
    msg <- "Note: reference is soo high that size-adoped pie won't fit into plotting region, consider lowering 'radius' for all plots" 
    if(radius > 1.09 & !silent) message(fxNa,msg)}
  graphics::pie(count[c(2,3,4)],col=col,init.angle=90,radius=radius,main=tit,labels=c("unique","hasRepeated","redundant"),cex=sort(c(0.55,0.3+radius,1))[2],clockwise=clockwise)
  graphics::mtext(paste("n=",count[1]),cex=0.8,adj=0,line=-0.3)
}  
  
