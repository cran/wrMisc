#' Reorganize results of search for close (similar) values in matrix-view 
#'
#' \code{closeMatchMatrix} reorganizes/refines results from simple search of similar values of 2 sets of data by \code{\link[wrMisc]{findCloseMatch}} (as list for one-to many relations) to more human friendly/readable matrix.
#' This function returns results combining two sets of data which were initially compared (eg measured and threoretical values) as matrix-view using output of \code{\link[wrMisc]{findCloseMatch}} and both original datastes
#' Additional information (covariables, annotation, ...) may be included as optional columns for either 'predMatr' or 'measMatr'.
#' Note : It is important to run \code{\link[wrMisc]{findCloseMatch}} with \code{sortMatch=FALSE} !
#' Note : Results presented based on view of 'predMatr', so if multiple 'measMatr' are at within tolared distance, lines of 'measMatr' will be repeated;
#' Note : Distances  'disToMeas' and 'ppmToPred' are oriented : neg value if measured is lower than predicted (and pos values if higher than predicted);
#' Note : Returns \code{NULL} when nothing within given limits of comparison;  
#' @param closeMatch (list) output from \code{\link[wrMisc]{findCloseMatch}}, ie list with hits for each 'x' (1st argument) : named vectors of value & x index in name; run with 'sortMatch'=F
#' @param predMatr (vector or matrix) predicted values, the column 'colPred' indicates which column is used for matching from \code{\link[wrMisc]{findCloseMatch}}; if column 'id' present this column will be used as identifier for matching
#' @param measMatr (vector or matrix) measured values, the column 'colMeas' indicates which column is used for matching from \code{\link[wrMisc]{findCloseMatch}}; if column 'id' present this column will be used as identifier for matching
#' @param prefMatch (character, length=2) prefixes ('^x' and/or '^y') thay may have been added by \code{findCloseMatch} 
#' @param colPred (integer or text, length=1) column of 'predMatr' with main values of comparison
#' @param colMeas (integer or text, length=1) column of 'measMatr' with main measures of comparison
#' @param limitToBest (integer) column of 'measMatr' with main measures of comparison
#' @param asDataFrame (logical) convert results to data.frame if non-numeric matrix produced (may slightly slow down big results)
#' @param origNa (logical) will try to use original names of objects 'predMatr','measMatr', if they are not multi-column and not conflicting other output-names (otherwise 'predMatr','measMatr' will appear)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @param debug (logical) for bug-tracking: more/enhanced messages
#' @return results as matrix-view based on initial results from \code{\link[wrMisc]{findCloseMatch}}, including optional columns of suppelemental data for both sets of data for comparison. Returns \code{NULL} when nothing within limits
#' @seealso  \code{\link[wrMisc]{findCloseMatch}}, \code{\link[wrMisc]{checkSimValueInSer}}  
#' @examples
#' aA <- c(11:17); bB <- c(12.001,13.999); cC <- c(16.2,8,9,12.5,15.9,13.5,15.7,14.1,5)
#' (cloMa <- findCloseMatch(aA,cC,com="diff",lim=0.5,sor=FALSE))       
#' # all matches (of 2d arg) to/within limit for each of 1st arg ('x'); 'y' ..to 2nd arg = cC
#' (maAa <- closeMatchMatrix(cloMa,aA,cC,lim=TRUE))  #
#' (maAa <- closeMatchMatrix(cloMa,aA,cC,lim=FALSE,origN=TRUE))  #
#' (maAa <- closeMatchMatrix(cloMa,cbind(valA=81:87,aA),cbind(valC=91:99,cC),colM=2,
#'   colP=2,lim=FALSE))
#' (maAa <- closeMatchMatrix(cloMa,cbind(aA,valA=81:87),cC,lim=FALSE,deb=TRUE))  #
#' a2 <- aA; names(a2) <- letters[1:length(a2)];  c2 <- cC; names(c2) <- letters[10+1:length(c2)]
#' (cloM2 <- findCloseMatch(x=a2,y=c2,com="diff",lim=0.5,sor=FALSE)) 
#' (maA2 <- closeMatchMatrix(cloM2,predM=cbind(valA=81:87,a2),measM=cbind(valC=91:99,c2),
#'   colM=2,colP=2,lim=FALSE,asData=TRUE)) 
#' (maA2 <- closeMatchMatrix(cloM2,cbind(id=names(a2),valA=81:87,a2),cbind(id=names(c2),
#'   valC=91:99,c2),colM=3,colP=3,lim=FALSE,deb=FALSE)) 
#' @export
closeMatchMatrix <- function(closeMatch, predMatr, measMatr, prefMatch=c("^x","^y"), colPred=1, colMeas=1, limitToBest=TRUE, asDataFrame=FALSE, origNa=TRUE,silent=FALSE,callFrom=NULL,debug=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="closeMatchMatrix")
  namesMXY <- c(deparse(substitute(closeMatch)), deparse(substitute(predMatr)), deparse(substitute(measMatr)))
  if(debug) silent <- FALSE
  if(debug) {message(fxNa,".. xxidentToMatr2a\n")}
  if(length(closeMatch) <1) message(fxNa," INPUT EMPTY, NOTHING TO DO !!")
  if(length(dim(predMatr)) <2) {
    predMatr <- cbind(id=if(is.null(names(predMatr))) 1:length(predMatr) else names(predMatr), predMatr)
    if(is.character(colPred)) colnames(predMatr)[2] <- colPred
    colPred <- 2
  } else if(!is.matrix(predMatr)) predMatr <- as.matrix(predMatr)
  if(is.character(colPred)) { chCol <- colnames(predMatr)==colPred
    if(sum(chCol) <1) { if(!silent) message(fxNa," PROBLEM : '",colPred,"' not found in ",namesMXY[2]," !!")
      colPred <- 2
    } else colPred <- which(chCol)}
  if("id" %in% colnames(predMatr)) {
    if(colnames(predMatr)[1] != "id") {tmp <- colnames(predMatr)=="id"
      predMatr <- predMatr[,c(which(tmp),which(!tmp))]} 
  } else { colPred <- colPred +1 
    predMatr <- cbind(id=if(is.null(rownames(predMatr))) 1:nrow(predMatr) else rownames(predMatr), predMatr)}  
  if(length(dim(measMatr)) <2) {
    measMatr <- cbind(id=if(is.null(names(measMatr))) 1:length(measMatr) else names(measMatr), measMatr)
    if(is.character(colMeas)) colnames(measMatr)[2] <- colMeas
    colMeas <- 2 
  } else if(!is.matrix(measMatr)) measMatr <- as.matrix(measMatr)
  if(is.character(colMeas)) colMeas <- which(colnames(measMatr)==colMeas)
  if("id" %in% colnames(measMatr)) {
    if(colnames(measMatr)[1] != "id") {tmp <- colnames(measMatr)=="id"
      measMatr <- measMatr[,c(which(tmp),which(!tmp))]} 
  } else { colMeas <- colMeas +1
    measMatr <- cbind(id=if(is.null(rownames(measMatr))) 1:nrow(measMatr) else rownames(measMatr), measMatr)  }
  txt <- c(" TROUBLE AHEAD :  ", "elements of 'closeMatch' have no name !!")
  chCloseM <- sub(prefMatch[1],"", names(closeMatch)) %in% predMatr[,"id"]
  
  if(all(!chCloseM, na.rm=TRUE)) stop("None of (list-) names of 'closeMatch' fits to 'predMatr'")
  if(any(!chCloseM, na.rm=TRUE) && !silent) message(fxNa,txt[1],sum(chCloseM)," out of ",length(chCloseM)," elements from 'closeMatch' not found in 'predMatr'")  
  chCloMaNa <- nchar(names(closeMatch))                                # check for empty names
  if(any(chCloMaNa <1, na.rm=TRUE)) message(fxNa,txt[1],sum(chCloMaNa <1)," out of ",length(closeMatch)," (list-)",txt[2])
  chCloMaN2 <- nchar(unlist(sapply(closeMatch, names))) 
  if(any(chCloMaN2 <1, na.rm=TRUE)) message(fxNa,txt[1],sum(chCloMaN2 <1)," out of ",sum(sapply(closeMatch,length))," indiv ",txt[2])   ##if(is.null(colnames(predMatr))) colnames(predMatr) <- "predMatr"
  nMatch <- sapply(closeMatch,length)
  if(debug) {message(fxNa,".. xxidentToMatr2c\n")}
  ## main joining of predMatr & measMatr
  measNa <- sub(prefMatch[2],"", unlist(lapply(closeMatch, names)))         # names of measured (y) to each 'y'
  if(length(grep("[[:alpha:]]", measNa)) <1) measNa <- as.integer(measNa)        
  prediNa <- sub(prefMatch[1],"", names(closeMatch))                       # ..'x', ie predMatr
  if(length(grep("[[:alpha:]]", prediNa)) <1) prediNa <- as.integer(prediNa)        
  txt <- c(fxNa," TROUBLE ahead, trouble matching names or NAs already in 'closeMatch'  (see ")  
  if(any(is.na(prediNa))) message(txt,"'x'/predicted) !!")
  if(any(is.na(measNa))) message(txt,"'y'/measured) !!")
  chP <- match(rep(prediNa, nMatch), predMatr[,1])
  partP <- if(length(chP)==1) matrix(predMatr[chP,], nrow=1, dimnames=list(rownames(predMatr)[chP], colnames(predMatr))) else predMatr[chP,]
  partM <- if(length(measNa)==1) matrix(measMatr[measNa,], nrow=1, dimnames=list(rownames(measMatr)[measNa], colnames(measMatr))) else measMatr[measNa,]
  if(origNa && length(grep(" = ",namesMXY[2:3])) >0) { origNa <- FALSE
    if(!silent) message(fxNa,"Reset argument 'origNa' to FALSE since names of 'predMatr' and/or 'measMatr' result of formula and would be too long")}
  if(origNa) {
    colnames(partP)[1] <- paste("id",namesMXY[2],sep=".")
    colnames(partM)[1] <- paste("id",namesMXY[3],sep=".")
    if(ncol(partP)==2) colnames(partP)[colPred] <- namesMXY[2] else {
      colnames(partP)[-1] <- paste(namesMXY[2],colnames(partP)[-1],sep=".")}
    if(ncol(partM)==2) colnames(partM)[colMeas] <- namesMXY[3] else {
      colnames(partM)[-1] <- paste(namesMXY[3],colnames(partM)[-1],sep=".")}    
  } else {
    dupNa <- duplicated(c(colnames(partP), colnames(partM)))
    if(any(dupNa)) {
      dupNa <- c(colnames(partP), colnames(partM))[dupNa]
      chNaP <- colnames(partP) %in% dupNa
      chNaM <- colnames(partM) %in% dupNa
      colnames(partP)[which(chNaP)] <- paste(colnames(partP)[which(chNaP)],"pred",sep=".")
      colnames(partM)[which(chNaM)] <- paste(colnames(partM)[which(chNaM)],"meas",sep=".") }
  }
  out <- cbind(partP, partM)
  colMeas <- colMeas + ncol(partP) 
  if(debug) {message(fxNa,".. xxidentToMatr2d\n")}   
  chOrd <- order(as.character(out[,colMeas]))             # grouping done by measured mass; tapply will always sort by this alphabetically (so instead of repeated re-sorting...)
  if(!identical(chOrd, 1:nrow(out))) out <- out[chOrd,]  
  ## add suppelemental info  (eg distance, if dist is closest, ...)
  if(nrow(out) <1) {if(!silent) message(fxNa," nothing left"); return(NULL)} else {
    di <- cbind(me=as.numeric(out[,colMeas]), pr=if(length(colPred) >0) as.numeric(out[,colPred]))
    out <- cbind(out, disToPred=di[,2] -di[,1], ppmToPred=XYToDiffPpm(di[,2], di[,1], nSign=5, callFrom=fxNa), nByGrp=NA, isMin=NA, nBest=NA)    # note : ppm as normalized difference 1e6*(meas - ref)/ref
    if(debug) {message(fxNa,".. xxidentToMatr2e\n")}
    ## inspect groups for multiple values
    ch1 <- table(out[,colPred])
    out[,ncol(out) -2] <- rep(ch1, ch1)                                   # nByGrp
    tmp <- tapply(as.numeric(out[,"disToPred"]), out[,colPred], function(x) {if(length(unique(x))==1) rep(1,length(x)) else 0+(abs(x)==min(abs(x)))} )
    out[,"isMin"] <- unlist(tmp)
    tmp <- tapply(as.integer(out[,"isMin"]), out[,colPred], sum)            # prepare nBest
    out[,ncol(out)] <- rep(tmp, ch1)          # nBest
    ch3 <- out[,ncol(out) -1] =="0"
    if(any(ch3)) out[which(ch3), ncol(out)] <- 0          # reset lines which are not best dist as nBest=0
    chMax <- out[,ncol(out) -1] >0
    if(limitToBest && any(!chMax)) out <- out[which(chMax),]                                                            # filter & add col for nBest      
    if(debug) { message(fxNa,".. xxidentToMatr2f \n")}
    ## re-sort 
    if(nrow(out) >1) {chOrd <- as.integer(order(out[,1]))    #out[,"id.predMatr"]
      out <- out[chOrd,]}   
    if(is.character(out) & asDataFrame) out <- convMatr2df(out)[,-1]
    out }}
   
