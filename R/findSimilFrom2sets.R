#' Find similar numeric values from two vectors/matrixes
#'
#' \code{findSimilFrom2sets} compares to vectors or matrixes and returns combined view including only all close (by \code{\link[wrMisc]{findCloseMatch}}).
#' Return matrix (predMatr) with add'l columns for index to and 'grp' (group of similar values (1-to-many)), 'nGrp' (n of grp), 'isBest' or 'nBest', 'disToMeas' 
#' (distance/difference between pair) &  'ppmToPred' (distance in ppm).
#' Note: too wide 'limitComp' will result in large window and many 'good' hits will compete (and be mutually exlcuded) if selection 'bestOnly' is selected
#' @param predMatr (matrix or numeric vector) dataset number 1, referred to as 'predicted', the colum speified in argument \code{colPre} points to the data to be used
#' @param measMatr (matrix or numeric vector) dataset number 2, referred to as 'measured', the colum speified in argument \code{colMeas} points to the data to be used
#' @param colMeas (integer) which column number of 'measMatr' to consider
#' @param colPre (integer) which column number of 'predMatr' to consider
#' @param compareTy (character) 'diff' (difference) 'ppm' (relative difference)
#' @param limitComp (numeric) limit used by 'compareTy'
#' @param bestOnly (logical) allows to filter only hits with min distance (defined by 'compareTy'), 3rd last col will be 'nBest' - otherwise 3rd last col 'isBest'
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) for bug-tracking: more/enhanced messages
#' @return This function returns a matrix (predMatr) with add'l columns for index to and 'grp' (group of similar values (1-to-many)), 'nGrp' (n of grp), 'isBest' or 'nBest', 'disToMeas' (distance/difference between pair) &  'ppmToPred' (distance in ppm)
#' @seealso \code{\link{checkSimValueInSer}} \code{\link{findCloseMatch}} \code{\link{closeMatchMatrix}}  
#' @examples
#' aA <- c(11:17); bB <- c(12.001,13.999); cC <- c(16.2,8,9,12.5,12.6,15.9,14.1)
#' aZ <-  matrix(c(aA,aA+20),ncol=2,dimnames=list(letters[1:length(aA)],c("aaA","aZ")))
#' cZ <-  matrix(c(cC,cC+20),ncol=2,dimnames=list(letters[1:length(cC)],c("ccC","cZ")))
#' findCloseMatch(cC,aA,com="diff",lim=0.5,sor=FALSE)
#' findSimilFrom2sets(aA,cC)
#' findSimilFrom2sets(cC,aA)
#' findSimilFrom2sets(aA,cC,best=FALSE)
#' findSimilFrom2sets(aA,cC,comp="ppm",lim=5e4,deb=TRUE)
#' findSimilFrom2sets(aA,cC,comp="ppm",lim=9e4,bestO=FALSE)
#' # below: find fewer 'best matches' since search window larger (ie more good hits compete !)
#' findSimilFrom2sets(aA,cC,comp="ppm",lim=9e4,bestO=TRUE)      
#' @export
findSimilFrom2sets <- function(predMatr, measMatr, colMeas=1, colPre=1, compareTy="diff", limitComp=0.5,bestOnly=FALSE,silent=FALSE,callFrom=NULL,debug=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="findSimilFrom2sets")
  namesXY <- c(deparse(substitute(predMatr)), deparse(substitute(measMatr)), deparse(substitute(colPre)), deparse(substitute(colMeas)))
  ## check input
  predMatr <- .vector2Matr(predMatr,colNa=if(length(dim(predMatr)) >1) colnames(predMatr) else namesXY[1])
  measMatr <- .vector2Matr(measMatr,colNa=if(length(dim(measMatr)) >1) colnames(measMatr) else namesXY[2])
  badColNa <- c("dif","ppm","nInRange","isBest","nBest")
  fxCh <- function(colN, matr, argN=NULL) { if(is.character(colN)) {      # check argument 'colN' as (character or number) to matrix 'matr', return numeric index
    msg <- c("Problem to ","find colname from ","locate number from "," reset to default=1")
    ch <- which(colN==colnames(matr))
    if(length(ch) >0) ch[1] else {message(msg[1:2],argN,msg[4]); 1}
  } else {colN <- as.integer(colN); if(colN <1 || colN >ncol(matr)) {colN <- 1; message(msg[c(1,3)],argN,msg[4])} else colN}}
  colMeas <- fxCh(colMeas, measMatr, namesXY[4])
  colPre <- fxCh(colPre, predMatr, namesXY[3])
  if(length(badColNa) >0) if(any(badColNa %in% colnames(measMatr))) message(fxNa,"Problem with colnames of ",namesXY[2])
  ## main
  prefMatch <- c("^x","^y")                  # fixed, since findCloseMatch() called wo initial names
  closeMatch <- findCloseMatch(as.numeric(predMatr[,colPre]), as.numeric(measMatr[,colMeas]), compTy=compareTy, limit=limitComp, sortMatch=FALSE, silent=silent, debug=debug, callFrom=fxNa)
  if(debug) {message(fxNa,"xxfindSimilFrom2sets2\n")}
  out <- closeMatchMatrix(closeMatch=closeMatch, predMatr=predMatr, measMatr=measMatr, prefMatch=prefMatch,
    colMeas=colMeas, colPred=colPre, limitToBest=bestOnly, callFrom=fxNa,debug=debug,silent=silent)
  if(!is.null(out)) { 
    colnames(out)[1:ncol(predMatr)] <- colnames(predMatr)
    colnames(out)[1:ncol(measMatr) +ncol(predMatr) +1] <- colnames(measMatr)
    out <- out[order(out[,colPre]),]}             # order by predicted
  out }
  
