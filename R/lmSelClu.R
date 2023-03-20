#' Run lm on segmented data (from clustering)
#'
#' \code{lmSelClu} runs linear regression on data segmented previously (eg by clustering).  
#' This functio offers various types of (2-coefficient) linear regression on 2 columns of 'dat' (matrix with 3rd col named 'clu' or 'cluID', numeric elements for cluster-number).
#' If argument \code{'clu'} is (default) 'max', the column 'clu' will be inspected to take most frequent value of 'clu', otherwise a numeric entry specifying the cluster to extract is expected.
#' Note: this function was initially made for use with results from diagCheck()
#' Note: this function lacks means of judging godness of fit of the regression preformed & means for plotting
#' @param dat matrix or data.frame
#' @param useCol (integer or charcter) specify which 2 columns of 'dat' to use for linear regression
#' @param clu (character) name of cluster to be extracted and treatad
#' @param regTy (character) change type used for linear regression :  'lin' for 1st col ~ 2nd col, 'res' for residue ~ 2nd col, 'norRes' for residue/2nd col ~2nd col or 'sqNorRes','inv' for 1st col ~ 1/(2nd col), 'invRes' for residue ~ 1/(2nd col)
#' @param filt1 (logical or numerical) filter criteria for 1st of 'useCol' , if numeric then select all lines of dat less than max of filt1
#' @param filt2 (logical or numerical) filter criteria for 2nd of 'useCol' , if numeric then select all lines of dat less than max of filt2
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return lm object (or NULL if no data left)
#' @seealso \code{\link[stats]{lm}}
#' @examples
#' set.seed(2016); ran1 <- runif(220)
#' mat1 <- round(rbind(matrix(c(1:100+ran1[1:100],rep(1,50)),ncol=3), 
#'   matrix(c(1:60,68:9+ran1[101:160],rep(2,60)),nc=3)),1)
#' colnames(mat1) <- c("a","BB","clu")
#' lmSelClu(mat1)
#' plot(mat1[which(mat1[,3]=="2"),1:2],col=grey(0.6))
#' abline(lmSelClu(mat1),lty=2,lwd=2)
#' # 
#' mat2 <- round(rbind(matrix(c(1:100+ran1[1:100],rep(1,50)),ncol=3), 
#'   matrix(c(1:60,(2:61+ran1[101:160])^2,rep(2,60)),nc=3)),1)
#' colnames(mat2) <- c("a","BB","clu")
#' (reg2 <- lmSelClu(mat2,regTy="sqNor"))
#' plot(function(x) coef(reg2)[2]+ (coef(reg2)[2]*x^2),xlim=c(1,70))
#' points(mat2[which(mat2[,3]=="2"),1:2],col=2)
#' @export
lmSelClu <- function(dat, useCol=1:2, clu="max", regTy="lin", filt1=NULL, filt2=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="lmSelClu")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  regTyOpt <- c("lin","sqNor","res", "inv","invRes","norRes","sqNorRes","parb","parbRes")
  msgE <- " Argument 'dat' should matrix or data.frame with at least 3 numeric columns (including column called 'clu')"
  if(length(dim(dat)) != 2) stop(msgE)
  if(ncol(dat) <3) stop(msgE)
  cluCol <- which(colnames(dat) %in% c("clu","cluID"))
  if(length(cluCol) <1) stop(" Can't find column named 'clu' or 'cluID' (specifying sub-group/cluster)")
  if(length(clu) >1) { message(fxNa," 'clu' should be of length 1 but is ",length(clu),", truncating...")
    clu <- clu[1]}
  if(identical(clu,"max")) {
    clu <- as.numeric(names(which.max(table(dat[,cluCol[1]])))) }
  msg2 <- paste(" Argument 'regTy' should be either ",pasteC(regTyOpt))
  if(length(regTy) !=1 || sum(regTy %in% regTyOpt) !=1) stop(msg2)
  lmColNa <- colnames(dat)[-1*cluCol][useCol]
  if(!is.null(filt1)) dat <-  dat[if(is.logical(filt1)) filt1 else {
    which(dat[,lmColNa[1]] > min(filt1) & dat[,lmColNa[1]] < max(filt1,na.rm=TRUE))},]
  if(!is.null(filt2)) dat <-  dat[if(is.logical(filt2)) filt2 else {
    which(dat[,lmColNa[2]] > min(filt2) & dat[,lmColNa[2]] < max(filt2,na.rm=TRUE))},]
  dat <- dat[which(dat[,cluCol]==clu),]
  if(length(dat) <1) { message(fxNa,":  No data left for linear modelling !")
    return(NULL)                           
  } else {
    message(fxNa,"  ready for linear modeling of ",nrow(dat)," lines of data, ie cluster '",clu,"'")
    dat <- data.frame(dat, res=dat[,lmColNa[1]]-dat[,lmColNa[2]])
    dat$normRes <- dat$res/dat[,lmColNa[1]]
    dat$sqNormRes <- dat$res/(dat[,lmColNa[1]]^2)
    colnames(dat)[which(colnames(dat)==lmColNa[2])] <- "slope"
    dat$invSlope <- 1/dat$slope
    switch(regTy,   
      lin = stats::lm(get(lmColNa[1]) ~ slope, data=as.data.frame(dat)),        
      sqNor = stats::lm(get(lmColNa[1])^2 ~ slope, data=as.data.frame(dat)),    
      inv = stats::lm(get(lmColNa[1]) ~ invSlope, data=as.data.frame(dat)),     
      parb= stats::lm(get(lmColNa[1]) ~ invSlope^2, data=as.data.frame(dat)),   
      res = stats::lm(res ~ slope, data=as.data.frame(dat)),                    
      invRes = stats::lm(res ~ invSlope, data=as.data.frame(dat)),              
      parbRes= stats::lm(normRes ~ invSlope^2, data=as.data.frame(dat)),        
      norRes = stats::lm(normRes ~ slope, data=as.data.frame(dat)),             
      sqNorRes = stats::lm(sqNormRes ~ slope, data=as.data.frame(dat)))         
  }}
   
