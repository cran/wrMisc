#' Check length of vector
#'
#' \code{checkVectLength} checks argument 'x' for expected length 'expeL' and return either message or error when expectation not met. 
#' Used for parameter ('sanity') checking in other user front-end functions.
#' @param x (numeric or charcter vector) input to check length
#' @param expeL (numeric) expected length
#' @param stopOnProblem (logical) continue on problems with message or stop (as error message)
#' @param silent (logical) suppress messages if \code{TRUE}
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return \code{NULL} (produces only optional message if length is OK or error-message if length is not OK)
#' @examples
#' aa <- 1:5; checkVectLength(aa,exp=3)
#' @export
checkVectLength <- function(x,expeL=1,stopOnProblem=FALSE,silent=FALSE,callFrom=NULL){
  argN <- deparse(substitute(x))
  fxNa <- .composeCallName(callFrom,newNa="checkVectLength")
  msg <- " argument 'expeL' should be numeric of length 1 ; resetting to default =1"
  if(!is.finite(expeL)) {expeL <- 1; message(fxNa,msg)}
  if(length(x) != expeL) {
    msg <- paste(" Argument '",argN,"' doesn't fit to expected length of ",expeL,sep="")
    if(stopOnProblem) stop(fxNa,msg) else {if(!silent) message(fxNa,msg)} }
  }

#' @export
.vector2Matr <- function(x,colNa=NULL,rowsKeep=TRUE) {
  ## take (numeric) vector and return matrix, if 'colNa' given will be used as colname
  nameX <- deparse(substitute(x))
  if(length(dim(x)) <2) x <- if(rowsKeep) matrix(x,ncol=1,dimnames=list(names(x), if(length(colNa) >0) colNa[1] else nameX)) else {
    matrix(x,nrow=1,dimnames=list( if(length(colNa) >0) colNa[1] else nameX),names(x))}
  x }

#' @export
.convertMatrToNum <- function(matr,useCol=NULL){
  ## convert all (or selected by 'useCol') colums of matrix (or data.frame) to matrix of numeric data
  ## by default ('useCol' as NULL) all columns will be used
  ## columns with text data will be returned as NAs
  ## columns not covered/mentioned in useCol will only be transformed to matrix
  ## for more elaborate function see convMatr2df()
  if(is.null(useCol)) {
    suplMa <- NULL
    useCol <- 1:ncol(matr)
  } else {                 # start with testing validity of values given
    msg1 <- "'useCol' should design the columns of 'matr' to be used for conversion.  Invalid entry"
    if(is.numeric(useCol)) {
      if(sum(is.finite(useCol)) < length(useCol) | max(useCol) > length(matr)) stop(msg1)
    } else {               # if real col-name given, convert to number
      useCol <- naOmit(match(as.character(useCol),colnames(matr)))
      if(length(useCol) <1) stop(" no valid columns-names found") }
    suplMa <- matr[,-1*useCol]
  }
  dimNa <- dimnames(matr)
  out <- matrix(as.numeric(as.character(as.matrix(matr[,useCol]))),nrow=length(dimNa[[1]]),ncol=length(useCol))    # robust to factors
  if(!is.null(suplMa)) out <- cbind(as.matrix(suplMa),out)
  dimnames(out) <- dimNa
  out }

#' @export
.checkFactor <- function(fac,facNa=NULL,minLev=2,silent=FALSE,callFrom=NULL){
  ## checking of factors (used for for limma model.matrix)
  objNa <- deparse(substitute(fac))
  fxNa <- .composeCallName(callFrom,newNa=".checkFactor")
  if(is.null(facNa) | length(facNa) <1) facNa <- objNa
  if(length(facNa) >1) facNa <- facNa[1]
  if(!is.factor(fac)) {fac <- as.factor(fac)
    if(!silent) message(fxNa," transforming ",facNa," to ",length(levels(fac))," level-factor  (",
      paste(utils::head(levels(fac)),collapse=", "),if(length(levels(fac)) >6) " ..",")")}
  if(length(levels(fac)) < minLev & !silent) message(fxNa," NOTE : factor ",facNa," contains not required number of levels")
  fac }

#' @export
.plusLowerCaps <- function(x) unique(c(x,tolower(x)))   # return original content of 'x' plus all in lower caps (non-redundant)

#' @export
.replSpecChar <- function(x,findSp=c("\\(","\\)","\\$"),replBy="_") {
  ## in character vector 'x' replace special character 'findSp' and replace by 'replBy'
  ## note that spec characters mut pe presented with protection for grep & sub
  if(length(findSp) > 1 & length(replBy)==1) replBy <- rep(replBy,length(findSp))
  for(i in 1:length(findSp)) { xx <- grep(findSp[i],x)
    if(length(xx) >0) x <- gsub(findSp[i],replBy[i],x)}
  x}

#' @export
.checkArgNa <- function(x,argNa,lazyEval=TRUE) {
   ## check character vector 'x' if any of its elements may be (lazy avaluation) argument names of vector 'argNa'
   ## potentially calling too many conflicts : eg will call conflict for single character even when saveral 'argNa' start with this letter (ie invalid for calling fx)
   ## return logical vector at length & names of 'x'
   isArgNa <- sapply(x,nchar) <1
   zz <- naOmit(match(argNa,x))
   if(length(zz) >0) isArgNa[zz] <- TRUE
   if(lazyEval) {
     tmp <- .seqCutStr(argNa[1],startFr=1,reverse=TRUE)[-1]
     tmp <- sapply(x,function(z) z %in% tmp)
     if(any(tmp)) isArgNa[which(tmp)] <- TRUE
   } else {zz <- naOmit(match(argNa,x)); if(length(zz)>0) isArgNa[zz] <- T}
  isArgNa }

#' @export
.transfTF2coord <- function(matr,silent=FALSE,callFrom=NULL){
  ## transform matrix 'matr' of TRUE/FALSE (boolean) to matrix of row/col coordinates
  ## return matrix with cols 'li' & 'col' for coordinates of TRUE in 'matr'
  ## curently not used anywhere ?
  fxNa <- .composeCallName(callFrom,newNa=".transfTF2coord")
  txt <- " 'matr' should be matrix of TRUE or FALSE (boolean)"
  if(length(dim(matr)) !=2) stop(txt," - bad dim") else if(any(dim(matr)==0)) stop(txt," with >0 rows & cols")
  chM <- is.logical(matr)
  if(!chM) {if(is.logical(as.logical(matr))) {
    if(!silent) message(fxNa,"converting to logical")
    matr <- matrix(as.logical(matr),ncol=ncol(matr))} else stop(txt," - can't convert")}
  wh <- which(matr)
  out <- cbind(li=wh %% nrow(matr),co=1+wh %/% nrow(matr))
  if(any(out[,1] <1)) {
    out[which(out[,1] <1),] <- matrix(c(rep(nrow(matr),sum(out[,1] <1)),out[which(out[,1] <1),2]-1),ncol=2)}
  out}
    
