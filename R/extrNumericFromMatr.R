#' Extract numeric part of matrix or data.frame
#'
#' \code{extrNumericFromMatr} extracts numeric part of matrix or data.frame, removing remaining non-numeric elements if \code{trimToData} is set to \code{TRUE}.
#' Note, that cropping entire lines where a (single) text element appeared may quickly reduce the overal content of the input data.
#'
#' @param dat matrix (or data.frame) for extracting numeric parts
#' @param trimToData (logical) default to remove (crop) lines and cols contributing to NA, non-numeric data is transfomed to NA
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return  matrix of numeric data
#' @examples
#' mat <- matrix(c(letters[1:7],14:16,LETTERS[1:6]),nrow=4,dimnames=list(1:4,letters[1:4]))
#' mat; extrNumericFromMatr(mat)
#' mat <- matrix(c(letters[1:4],1,"e",12:19,LETTERS[1:6]),nr=5,dimnames=list(11:15,letters[1:4]))
#' mat; extrNumericFromMatr(mat)
#' @export
extrNumericFromMatr <- function(dat,trimToData=TRUE,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="extrNumericFromMatr")
  dimNa <- dimnames(dat)
  chDat <- is.data.frame(dat)
  if(is.data.frame(dat)) {
    facCol <- sapply(1:ncol(dat),function(x) is.factor(dat[,x]))
    if(sum(facCol,na.rm=TRUE) >0) for(i in which(facCol)) dat[,i] <- as.character(dat[,i])  
    dat <- gsub("\x5E[[:blank:]]+","",as.matrix(dat)) }  
  out <- matrix(NA,nrow=nrow(dat),ncol=ncol(dat))
  for(i in 1:ncol(dat)) {x <- .mayBeNum(dat[,i]); if(length(x)>0) out[x,i] <- dat[x,i]}
  out <- matrix(as.numeric(out),nrow=nrow(dat),ncol=ncol(dat),dimnames=dimnames(dat))
  if(trimToData) {
    useLi <- table(unlist(apply(out,2,function(x) (1:length(x))[!is.na(x)])))
    useLi <- which(useLi==max(useLi))
    useCo <- table(unlist(apply(out,1,function(x) (1:length(x))[!is.na(x)])))
    useCo <- which(useCo==max(useCo))
    ## primary cropping
    ou2 <- out[useLi,useCo]                                                      
    if(length(dim(ou2)) <1) ou2 <- matrix(ou2,nrow=length(useLi),dimnames=list(rownames(dat)[useLi],colnames(dat)[useCo]))
    ## final/add'l cropping of lines with NAs
    out <- ou2[which(rowSums(is.na(ou2)) <1),]                                    
    if(length(dim(out)) <1) out <- matrix(out,ncol=ncol(ou2),dimnames=list(rownames(ou2)[which(rowSums(is.na(ou2)) <1)],colnames(ou2)))
    txt <- c("Nothing remains when cropping for numeric part !","Removed "," out of "," columns and "," rows")
    if(!silent) message(fxNa, if(length(out) <1) txt[1] else c(txt[2],ncol(dat)-ncol(out),
      txt[3],ncol(dat),txt[4],nrow(dat)-nrow(out),txt[3],nrow(dat),txt[5]))
    }         
  out }    

