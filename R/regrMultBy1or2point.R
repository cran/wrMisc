#' Rescaling of multiple data-sets according to reference data using regression
#'
#' \code{regrMultBy1or2point} regresses each col of matrix according to 'refLst'(describing rownames of inDat). 
#' If 'refLst' conatins 2 groups, the 1st group will be set to the 1st value of 'regrTo' (and the 2nd group of 'refLst' to the 2nd 'regtTo')
#'
#' @param inDat matrix or data.frame
#' @param refLst list of names existing in inDat (one group of names for each value in 'regrTo'), to be transformed in values precised in 'regTo'; if no matches to names of 'inDat' found, the 2 lowest and/or highest highest values will be chosen
#' @param regrTo (numeric,length=2) range (at scale 0-1) of target-values for mean of elements cited in 'refLst'
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return  normalized matrix
#' @seealso \code{\link{adjBy2ptReg}}, \code{\link{regrBy1or2point}}
#' @examples
#' set.seed(2016); dat2 <- round(cbind(1:50 +(1:50)*runif(50),2.2*(1:50) +rnorm(50,0,3)),1)
#' rownames(dat2) <- 1:nrow(dat2)
#' reg1 <- regrBy1or2point(dat2[,1],refLst=list(as.character(5:7),as.character(44:45)))
#' reg2 <- regrMultBy1or2point(dat2,refLst=list(as.character(5:7),as.character(44:45)))
#' plot(dat2[,1],reg2[,1])
#' identical(reg1,reg2[,1])
#' identical(dat2[,1],reg2[,1])
#' @export
regrMultBy1or2point <- function(inDat,refLst,regrTo=c(1,0.5),silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="regrMultBy1or2point")
  checkEntr <- .checkRegrArguments(inDat,refLst,regrTo)
  refLst <- checkEntr$refLst; regrTo <- checkEntr$regrTo
  contrAve <- sapply(lapply(refLst,function(x) as.matrix(inDat[x,])),colMeans,na.rm=TRUE)
  if(is.null(dim(contrAve))) contrAve <- matrix(contrAve,ncol=length(contrAve))                   
  normFact <- if(length(refLst) ==1) (regrTo[1]/contrAve[,1]) else (regrTo[2]-regrTo[1])/(contrAve[,2]-contrAve[,1])   
  normFact <- cbind(k=normFact, d= if(length(refLst) ==1) rep(0,ncol(inDat)) else regrTo[1]-normFact*contrAve[,1])
  matrix(rep(normFact[,1],each=nrow(inDat)),nrow=nrow(inDat))*inDat + matrix(rep(normFact[,2],each=nrow(inDat)),nrow=nrow(inDat)) }
    
