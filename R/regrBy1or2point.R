#' Rescaling according to reference data using linear regression.
#'
#' \code{regrBy1or2point} does rescaling: linear transform simple vector 'inDat' that (mean of) elements of names cited in 'refLst' will end up as values 'regrTo'. 
#' Regress single vector according to 'refLst' (describing names of inDat).
#' If 'refLst' contains 2 groups, the 1st group will be set to the 1st value of 'regrTo' (and the 2nd group of 'refLst' to the 2nd 'regtTo')
#' @param inDat matrix or data.frame
#' @param refLst list of names existing in inDat (one group of names for each value in 'regrTo'), to be transformed in values precised in 'regTo'; if no matches to names of 'inDat' found, the 2 lowest and/or highest highest values will be chosen
#' @param regrTo (numeric,length=2) range (at scale 0-1) of target-values for mean of elements cited in 'refLst'
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return normalized matrix
#' @seealso \code{\link{adjBy2ptReg}}, \code{\link{regrMultBy1or2point}}
#' @examples
#' set.seed(2016); dat1 <- 1:50 +(1:50)*round(runif(50),1)
#' names(dat1) <- 1:length(dat1)
#' reg1 <- regrBy1or2point(dat1,refLst=c("2","49"))
#' plot(reg1,dat1) 
#' @export
regrBy1or2point <- function(inDat,refLst,regrTo=c(1,0.5),silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="regrBy1or2point")
  if(length(names(inDat)) <0) {
    names(inDat) <- 1:length(inDat)
    message(fxNa," no (valid) names found in 'inDat', making default numbering")}
  chLst <- sapply(refLst,function(x) !all(x %in% names(inDat)))
  if(any(chLst)) {
    if(chLst[1]) refLst[[1]] <- names(inDat)[order(inDat)][1:2]
    if(chLst[2]) refLst[[2]] <- names(inDat)[order(inDat)][length(inDat):(length(inDat)-1)]
    if(!silent) message(fxNa," no (valid) names found in 'refLst', choosing noth most extreme values")}
  checkEntr <- .checkRegrArguments(inDat,refLst,regrTo)
  refLst <- checkEntr$refLst
  regrTo <- checkEntr$regrTo
  tmp <- sapply(refLst,function(x) inDat[x])
  contrAve <- if(is.matrix(tmp)) colMeans(tmp) else sapply(tmp,mean,na.rm=TRUE)
  normFact <- if(length(refLst) ==1) (regrTo[1]/contrAve[1]) else (regrTo[2]-regrTo[1])/(contrAve[2]-contrAve[1])  
  normFact <- c(k=normFact, d= if(length(refLst) ==1) 0 else regrTo[1] -normFact*contrAve[1])
  inDat*normFact[1] + normFact[2] }
    
