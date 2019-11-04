#' Rename columns
#'
#' \code{renameColumns} renames columns of 'refMatr' using 2-column matrix (or data.frame) indicating old and new names (for replacement). 
#' @param refMatr matrix (or data.frame) where column-names should be changed
#' @param newName (matrix of character) giving correspondence of old to new names (number of lines must match number of columns of 'refMatr')
#' @param silent (logical) suppres messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return  matrix (or data.frame) with renamed columns
#' @examples
#' ma <- matrix(1:8,ncol=4,dimnames=list(1:2,LETTERS[1:4]))
#' replBy1 <- cbind(new=c("dd","bb","z_"),old=c("D","B","zz"))
#' replBy2 <- matrix(c("D","B","zz","dd","bb","z_"),ncol=2)
#' replBy3 <- matrix(c("X","Y","zz","xx","yy","z_"),ncol=2)
#' renameColumns(ma,replBy1)
#' renameColumns(ma,replBy2)
#' renameColumns(ma,replBy3)
#' @export
renameColumns <- function(refMatr,newName,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="renameColumns")
  if(length(dim(refMatr)) <2) stop("expecting matrix or data.frame")
  msg <- "expecting matrix with 2 cols ('old','new')"
  if(length(dim(newName)) !=2) stop(msg) else if(ncol(newName) <2) stop(msg)
  if(is.null(colnames(newName))) { colNe <- 1:2
  } else { colNe <- match(c("old","new"),colnames(newName)) #;  cat(" colNe ini ",colNe,"\n")
    if(is.na(colNe[1])) colNe[1] <- (1:ncol(newName))[if(is.na(colNe[2])) -2 else -1*colNe[2]][1]
    if(is.na(colNe[2])) colNe[2] <- (1:ncol(newName))[-1*colNe[1]][1] }
  newName <- newName[,colNe]
  replLi <- naOmit(match(colnames(refMatr),newName[,1]))
  if(length(replLi) <1) { if(!silent) message(fxNa," no names matching for replacing dat, nothing to do !")
  } else {
    colnames(refMatr)[match(newName[replLi,1],colnames(refMatr))] <- newName[replLi,2] }
  refMatr }

#' @export
.keepFiniteCol <- function(dat,silent=FALSE,msgStart=NULL,callFrom=NULL){
  ## remove all columns where all data are not finite
  fxNa <- .composeCallName(callFrom,newNa=".keepFiniteCol")
  tmp <- colSums(is.finite(dat))
  if(any(tmp <1)) {
    if(!silent) message(fxNa," removing ",sum(tmp <1)," columns without valid (finite) numbers")
    chC <- tmp >0
    dat <- if(sum(chC) ==1) matrix(dat[,which(chC)],ncol=1,dimnames=list(rownames(dat),colnames(dat[chC]))) else dat[,which(chC)]}
  dat }

#' @export
.removeEmptyCol <- function(dat,fromBackOnly=TRUE,searchFields=c(""," ","NA.",NA),silent=FALSE,callFrom=NULL){
  ## search for (empty) columns conaining only entries defined in 'searchFields' and remove such columns
  ## if 'fromBackOnly' =TRUE .. only tailing empty columns will be removed (other columns with "empty" entries in middle will be kept)
  ## if ''=TRUE columns containing all NAs will be excluded as well
  ## will also remove columns containing (exculsively) mixtures of the various 'searchFields'
  fxNa <- .composeCallName(callFrom,newNa=".removeEmptyCol")
  if(length(dim(dat)) <1) dat <- matrix(dat,ncol=1,dimnames=list(names(dat),NULL))
  iniDimNa <-  dimnames(dat)
  isEmpty <- which(apply(dat,2,function(x) sum(x %in% searchFields)==length(x)))
  if(fromBackOnly & length(isEmpty) >0) {
    isEmpty <- if(max(isEmpty) != ncol(dat)) NULL else {
      if(length(isEmpty) >1) .breakInSer(isEmpty) else isEmpty } }
  if(length(isEmpty) >1) {
    dat <- .removeCol(dat,isEmpty)
    if(!silent) message(fxNa," columns no ",paste(isEmpty,collapse=", ")," were considered empty and removed")
    }
  dat }
  
