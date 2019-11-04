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
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param debug (logical) for bug-tracking: more/enhanced messages
#' @return matrix (predMatr) with add'l columns for index to and 'grp' (group of similar values (1-to-many)), 'nGrp' (n of grp), 'isBest' or 'nBest', 'disToMeas' (distance/difference between pair) &  'ppmToPred' (distance in ppm)
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
findSimilFrom2sets <- function(predMatr,measMatr,colMeas=1,colPre=1,compareTy="diff",limitComp=0.5,bestOnly=FALSE,silent=FALSE,callFrom=NULL,debug=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="findSimilFrom2sets")
  namesXY <- c(deparse(substitute(predMatr)),deparse(substitute(measMatr)),deparse(substitute(colPre)),deparse(substitute(colMeas)))
  ## check input
  predMatr <- .vector2Matr(predMatr,colNa=if(length(dim(predMatr)) >1) colnames(predMatr) else namesXY[1])
  measMatr <- .vector2Matr(measMatr,colNa=if(length(dim(measMatr)) >1) colnames(measMatr) else namesXY[2])
  badColNa <- c("dif","ppm","nInRange","isBest","nBest")
  fxCh <- function(colN,matr,argN=NULL) { if(is.character(colN)) {      # check argument 'colN' as (character or number) to matrix 'matr', return numeric index
    msg <- c("problem to ","find colname from ","locate number from "," reset to default=1")
    ch <- which(colN==colnames(matr))
    if(length(ch) >0) ch[1] else {message(msg[1:2],argN,msg[4]); 1}
  } else {colN <- as.integer(colN); if(colN <1 | colN >ncol(matr)) {colN <- 1; message(msg[c(1,3)],argN,msg[4])} else colN}}
  colMeas <- fxCh(colMeas,measMatr,namesXY[4])
  colPre <- fxCh(colPre,predMatr,namesXY[3])
  if(length(badColNa) >0) if(any(badColNa %in% colnames(measMatr))) message(fxNa," Problem with colnames of ",namesXY[2])
  ## main
  prefMatch <- c("^x","^y")                  # fixed, since findCloseMatch() called wo initial names
  closeMatch <- findCloseMatch(as.numeric(predMatr[,colPre]),as.numeric(measMatr[,colMeas]),compTy=compareTy,limit=limitComp,sortMatch=FALSE,callFrom=fxNa)
  if(debug) {cat("xxfindSimilFrom2sets2\n")}
  out <- closeMatchMatrix(closeMatch=closeMatch,predMatr=predMatr[,],measMatr=measMatr,prefMatch=prefMatch,
    colMeas=colMeas,colPred=colPre,limitToBest=bestOnly,callFrom=fxNa,debug=debug,silent=silent)
  if(!is.null(out)) { 
    colnames(out)[1:ncol(predMatr)] <- colnames(predMatr)
    colnames(out)[1:ncol(measMatr)+ncol(predMatr)+1] <- colnames(measMatr)
    out <- out[order(out[,colPre]),]}             # order by predicted
  out }

#' @export
.identToMatr2 <- function(closeMatch,predMatr,measMatr,prefMatch=c("^x","^y"),colMeas=1,colPred=1,limitToBest=TRUE,resultAsMatr=FALSE,silent=FALSE,callFrom=NULL,debug=FALSE){
  ## filter/refine results from simple comparison to close values by findCloseMatch(): make matrix with data used as 'x'
  ## 'closeMatch' .. output from findCloseMatch(), ie list with hits for each 'x' (1st argument) : named vectors of value & x index in name
  ## 'predMatr' .. matrix of predicted values with col 'colPred' use for matching in findCloseMatch()
  ## 'measMatr' .. matrix of measured values with col 'colMeas' use for matching in findCloseMatch()
  ## '' .. numeric
  ## 'limitToBest' .. (logical) find/refine to best of multiple hits within range (otherwise 1st reported hit), may consume time
  ## 'resultAsMatr'.. (logical) convert matrix of results to data.frame (may slightly slow down big results)
  ## note : important to run findCloseMatch() with sortMatch=FALSE !
  ## note : results oriented towards 'predMatr', so if multiple 'measMatr' are at equal distance, only 1st of 'measMatr' given (ie predMatr lines not repeated)
  ##  output col 'disToMeas' & 'ppmToPred' have neg value if measured is lower than predicted (and pos values if higher than predicted)
  ## return filtered version of 'predMatr' with new col (last) column 'nExper' (counts no of experimental values matching predicted of 'predMatr')
  ## note : returns NULL when nothing within given limits
  fxNa <- .composeCallName(callFrom,newNa=".identToMatr2")
  if(debug) silent <- FALSE
  predMatr <- .vector2Matr(predMatr)
  measMatr <- .vector2Matr(measMatr)
  if(is.character(colPred)) colPred <- which(colnames(predMatr)==colPred)
  if(is.character(colMeas)) colMeas <- which(colnames(measMatr)==colMeas)
  if(any(c("nExper","measMatr","measLi") %in% colnames(predMatr))) message(fxNa," Problem with colnames of 'predMatr'")
  if(is.null(colnames(predMatr))) colnames(predMatr) <- "predMatr"
  measNo <- sub(prefMatch[2],"",sapply(closeMatch,function(x) names(x[1])))         # pick first 'y' (if multiple measured) to each 'x'
  if(debug) {cat("..xxidentToMatr2aa\n")}
  if(length(grep("[[:alpha:]]",measNo)) >0) {
    ## this part may need re-checking, had no recent cases where this route is being taken
    closeM2 <- closeMatch
    names(closeM2) <- NULL
    useLi <- as.numeric(sub(prefMatch[1],"",names(unlist(closeM2))))                               # the x-part of all closeMatch
    measNo <- rep(as.numeric(sub(prefMatch[2],"",names(closeMatch))),sapply(closeMatch,length))
     if(debug) {cat("..xxidentToMatr2b\n")}
  } else {
    measNo <- as.numeric(sub(prefMatch[2],"", unlist(sapply(closeMatch,names))))
    useLi <- rep(as.numeric(sub(prefMatch[1],"",names(closeMatch))),sapply(closeMatch,length)) }
  if(debug) {cat("..xxidentToMatr2a\n")}
  tm1 <- .vector2Matr(predMatr[as.numeric(useLi),],rowsKeep=FALSE)
  tm2 <- .vector2Matr(measMatr[as.numeric(measNo),],rowsKeep=TRUE)
  if(identical(colnames(measMatr),"measMatr")) colnames(tm2) <- "measured"
  out <- cbind(tm1,measLi=useLi,tm2,predLi=measNo)
  naPredMat <- colnames(predMatr)
  naMeasMat <- colnames(measMatr)
  colnames(out)[1:ncol(predMatr)] <- if(is.null(naPredMat)) "predMatr" else naPredMat            # need in case of 1-col matrix
  colnames(out)[1+ncol(predMatr)+1:ncol(measMatr)] <- if(is.null(naMeasMat)) "measMatr" else naMeasMat            # need in case of 1-col matrix
  chCol <- colnames(out)==""
  if(any(chCol)) out <- if(nrow(out) >1) out[,-1*which(chCol)] else {                                                # remove duplicated column with measured values (mass)   
    matrix(out[,-1*which(chCol)],nrow=nrow(out),dimnames=list(rownames(out),colnames(out)[-1*which(chCol)]))}               
  if(debug) {cat("..xxidentToMatr2c\n")}
  if(nrow(out) <1) {if(!silent) message(fxNa," nothing left"); return(NULL)} else {
    out <- combineByEitherFactor(out,which(colnames(out)==colnames(measMatr)[colMeas]),which(colnames(out)==colnames(predMatr)[colPred]),nByGrp=TRUE,convergeMax=TRUE)        # add col 'grp' & 'nGrp'
    if(debug) {cat("..xxidentToMatr2d\n")}
    measCol <- match(c(if(ncol(predMatr) >1) colnames(predMatr)[colPred] else "predMatr",if(ncol(measMatr) >1) colnames(measMatr)[colMeas] else "measMatr"),colnames(out))    # pred,meas
    if(any(is.na(measCol))) {message(fxNa," Problem finding column-names for predicted & measured : ",measCol[1]," & ",measCol[2])
      if(is.na(measCol[1])) measCol[1] <- which(colnames(out) =="predLi")-1
      if(is.na(measCol[2])) measCol[2] <- which(colnames(out) =="measLi")-1
      }
    di <- cbind(me=as.numeric(out[,measCol[2]]),pr=as.numeric(out[,measCol[1]]))
    out <- cbind(out,disToPred=di[,2]-di[,1],ppmToPred=XYToDiffPpm(di[,2],di[,1],nSign=5,callFrom=fxNa))    # note : ppm as normalized difference 1e6*(meas - ref)/ref
     if(debug) {cat("..xxidentToMatr2e\n")}
    ## inspect groups for multiple values
    chMi <- tapply(out[,ncol(out)-1],out[,ncol(out)-3],function(x) {x <- as.numeric(x); rbind(x,abs(x)==min(abs(x)))})     # (so far min 'disToPred') grab value of distToPred & FALSE/TRUE if min
    chMi2 <- lapply(chMi,function(x) x[2,]==0)                              # which multi group has non min values
    if(any(unlist(chMi2))) {                                                # non-min values exit, need to readjust number by group, number of best fits,...
      grpWnonMin <- which(sapply(chMi2,sum) >0)
      nonMinVa <- lapply(chMi[grpWnonMin],function(x) x[1,which(x[2,] <1)])       # grab value of distToPred & FALSE/TRUE if min
      useNa <- rep(sub("\\.x$","",names(nonMinVa)),sapply(nonMinVa,length))
      nonMinVa <- unlist(nonMinVa)                                          # actual values that are not at min
      names(nonMinVa) <- useNa
      nonMinLi <- sapply(names(grpWnonMin),function(x) which(out[,ncol(out)-3]==x & out[,ncol(out)-1] %in% nonMinVa[which(names(nonMinVa)==x)]) )
      if(is.list(nonMinLi)) nonMinLi <- unique(as.numeric(unlist(nonMinLi)))
      if(length(nonMinLi) ==nrow(out)) message(fxNa," MAJOR Problem when filtering for best distances only - nothing left !!")
      if(limitToBest){                                                            # filter & add col for nBest
        if(!silent) message(fxNa," filter for best distances only : reducing from ",nrow(out)," to ",nrow(out)-length(nonMinLi))
        out <- out[-as.numeric(nonMinLi),]
        mTa <- table(out[,ncol(out)-3])
        out <- cbind(out,nBest=mTa[match(out[,ncol(out)-3],names(mTa))])
      } else { out <- cbind(out,isBest=1)
        out[as.numeric(nonMinLi),ncol(out)] <- 0 }
      out <- out[,c(1:(ncol(out)-3),ncol(out),(ncol(out)-2):(ncol(out)-1))]  # order : .. measLi grp nByGrp nBest disToMeas ppmToPred
    } else {                                                                 # all values/lines are 'best hist'
      out <- if(limitToBest) cbind(out,nBest=1) else cbind(out,isBest=1)
    }
    if(resultAsMatr) out else convMatr2df(out)[,-1]}}
   
