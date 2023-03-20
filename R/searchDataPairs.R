#' Search duplicated data over multiple columns, ie pairs of data
#'
#' \code{searchDataPairs} searches matrix for columns of similar data, ie 'duplicate' values in separate columns or very similar columns if \code{realDupsOnly=FALSE}.
#' Initial distance measures will be normalized either to diagonale (\code{normRange=TRUE)} of 'window' or to the real max distance observed (equal or less than diagonale).
#' Return data.frame with names for sample-pair, percent of identical values (100 for complete identical pair) and relative (Euclidean) distance (ie max dist observed =1.0).
#' Note, that low distance values do not necessarily imply correlating data.
#'
#' @param dat matrix or data.frame (main input)
#' @param disThr (numeric) threshold to decide when to report similar data (applied on normalized distances, low val fewer reported), applied on normalized distances (norm to diagonale of all data for best relative 'unbiased' view)
#' @param byColumn (logical) rotates main input by 90 degrees (using \code{\link[base]{t}}), thus allows to read by rows instead of by columns
#' @param normRange (logical) normize each columns separately if \code{TRUE}
#' @param altNa (character, default \code{NULL}) vector with alternative names (for display)
#' @param realDupsOnly (logical) if \code{TRUE} will consider equal values only, otherwise will also consider very close values (based on argument \code{disThr})
#' @param silent (logical) suppres messages
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a data.frame with names of sample-pairs, percent of identical values (100 for complete identical pair) and rel (Euclidean) distance (ie max dist observed =1.0)
#' @seealso \code{\link[base]{duplicated}}
#' @examples
#' mat <- round(matrix(c(11:40,runif(20)+12,11:19,17,runif(20)+18,11:20), nrow=10), 1)
#' colnames(mat) <- 1:9
#' searchDataPairs(mat,disThr=0.05)
#' @export
searchDataPairs <- function(dat, disThr=0.01, byColumn=TRUE, normRange=TRUE, altNa=NULL, realDupsOnly=TRUE, silent=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="searchDataPairs")
  if(!isTRUE(silent)) silent <- FALSE
  if(byColumn) dat <- t(dat)
  if(!is.null(altNa)){
    msg <- "invalid content of argument 'altNa' (should be non-redudant vector of length of number of columns of 'dat'"
    if(length(altNa) != ncol(dat)) {altNa <- NULL; if(!silent) message(fxNa,msg)}
    if(length(altNa) > length(unique(dat))) {altNa <- NULL; if(!silent) message(fxNa,msg)} }
  if(!is.null(altNa)) rownames(dat) <- altNa else {
    if(is.null(rownames(dat))) rownames(dat) <- 1:nrow(dat) }
  ma0 <- matrix(1:nrow(dat), nrow=nrow(dat), ncol=nrow(dat))
  diCoor <- cbind(li=ma0[lower.tri(ma0)], co=t(ma0)[lower.tri(ma0)])
  di <- stats::dist(dat)
  diNor <- if(normRange) stats::dist(apply(dat, 2, range)) else max(di,na.rm=TRUE) 
  di <- di/diNor
  chk <- diCoor[which(di < disThr),]
  if(!is.matrix(chk)) chk <- matrix(chk, ncol=2)
  chN <- if(nrow(chk) <1) matrix(nrow=0, ncol=4, dimnames=list(NULL,c("samp1","samp2","pcIdent","relDist"))) else {
    data.frame(samp1=rownames(dat)[chk[,2]], samp2=rownames(dat)[chk[,1]],
    pcIdent=round(100*apply(chk, 1, function(x) sum(dat[x[1],]==dat[x[2],],na.rm=TRUE)/ncol(dat)),1),
    relDist=signif(as.numeric(di)[which(di < disThr)],4))}
  if(nrow(chN) >0 && realDupsOnly) chN <- chN[chN$pcIdent ==100,]
  chN }
    
