#' Check length of vector
#'
#' \code{checkVectLength} checks argument 'x' for expected length 'expeL' and return either message or error when expectation not met. 
#' May be used for parameter ('sanity') checking in other user front-end functions.
#' @param x (numeric or charcter vector) input to check length
#' @param expeL (numeric) expected length
#' @param stopOnProblem (logical) continue on problems with message or stop (as error message)
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns \code{NULL}; it produces either error-message if length is not OK or optional message if length is OK 
#' @examples
#' aa <- 1:5; checkVectLength(aa,exp=3)
#' @export
checkVectLength <- function(x, expeL=1, stopOnProblem=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="checkVectLength")
  argN <- deparse(substitute(x))
  msg <- " argument 'expeL' should be numeric of length 1 ; resetting to default =1"
  if(!is.finite(expeL)) {expeL <- 1; message(fxNa,msg)}
  if(length(x) != expeL) {
    msg <- paste(" Argument '",argN,"' doesn't fit to expected length of ",expeL,sep="")
    if(stopOnProblem) stop(fxNa,msg) else {if(!silent) message(fxNa,msg)} }
  }

#' Convert numeric vector to matrix
#'
#' Take (numeric) vector and return matrix, if 'colNa' given will be used as colname
#' 
#' @param x (numeric or character) main input
#' @param colNa (integer) design the comumn-name to be given
#' @param rowsKeep (logical) is \code{TRUE} make matrix of 1 column, otherwise of 1 row
#' @return matrix
#' @seealso  \code{\link[base]{matrix}}
#' @examples
#' .vector2Matr(c(3:6))
#' @export
.vector2Matr <- function(x, colNa=NULL, rowsKeep=TRUE) {
  ## take (numeric) vector and return matrix, if 'colNa' given will be used as colname
  nameX <- deparse(substitute(x))
  if(is.factor(x)) x <- as.character(x)
  if(length(dim(x)) <2) x <- if(rowsKeep) matrix(x, ncol=1, dimnames=list(names(x), if(length(colNa) >0) colNa[1] else nameX)) else {
   matrix(x, nrow=1, dimnames=list( if(length(colNa) >0) colNa[1] else nameX), names(x))}
  x }

#' Convert numeric matrix to numeric
#'
#' Take matrix and return vector
#' 
#' @param matr (matrix) main input
#' @param useCol (integer) design the comumns to be used
#' @return numeric vector
#' @seealso  \code{\link[base]{matrix}}
#' @examples
#' .convertMatrToNum(matrix(1:6, ncol=2))
#' @export
.convertMatrToNum <- function(matr, useCol=NULL){
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
      if(sum(is.finite(useCol)) < length(useCol) || max(useCol) > length(matr)) stop(msg1)
    } else {               # if real col-name given, convert to number
      useCol <- naOmit(match(as.character(useCol), colnames(matr)))
      if(length(useCol) <1) stop(" no valid columns-names found") }
    suplMa <- matr[,-1*useCol]
  }
  dimNa <- dimnames(matr)
  out <- matrix(as.numeric(as.character(as.matrix(matr[,useCol]))), nrow=length(dimNa[[1]]), ncol=length(useCol))    # robust to factors
  if(!is.null(suplMa)) out <- cbind(as.matrix(suplMa), out)
  dimnames(out) <- dimNa
  out }

#' Check Factor
#'
#' This function was designed to check a factor object
#' 
#' @param fac (factor) main input
#' @param facNa (character) level-names
#' @param minLev (integer) minium number of levels
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return This function returns a corrceted/adjusted factor
#' @seealso  \code{\link[base]{factor}}
#' @examples
#' .checkFactor(gl(3,2))
#' @export
.checkFactor <- function(fac, facNa=NULL, minLev=2, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## checking of factors (used for for limma model.matrix)
  objNa <- deparse(substitute(fac))
  fxNa <- .composeCallName(callFrom, newNa=".checkFactor")
  if(is.null(facNa) | length(facNa) <1) facNa <- objNa
  if(length(facNa) >1) facNa <- facNa[1]
  if(!is.factor(fac)) {fac <- as.factor(fac)
    if(!silent) message(fxNa," transforming ",facNa," to ",length(levels(fac))," level-factor  (",
      paste(utils::head(levels(fac)),collapse=", "),if(length(levels(fac)) >6) " ..",")")}
  if(length(levels(fac)) < minLev & !silent) message(fxNa," NOTE : factor ",facNa," contains not required number of levels")
  fac }


#' Add lower caps to character vector
#'
#' This function allows adding all content as lower caps to/of character vector
#' 
#' @param x (character) main input
#' @return This function returns a elongated character vector
#' @seealso  \code{\link[base]{chartr}}
#' @examples
#' .plusLowerCaps(c("Abc","BCD"))
#' @export
.plusLowerCaps <- function(x) unique(c(x,tolower(x)))   # return original content of 'x' plus all in lower caps (non-redundant)


#' Replace Special Characters
#'
#' This function allows replacing special characters
#' Note that (most) special characters must be presented with protection for \code{grep} and \code{sub}.
#' 
#' @param x (character) main input
#' @param findSp (character) special characters to replace (may have to be given as protected)
#' @param replBy (character) replace by
#' @return This function returns a corrceted/adjusted factor
#' @seealso  \code{\link[base]{factor}}
#' @examples
#' .replSpecChar(c("jhjh(ab)","abc"))
#' @export
.replSpecChar <- function(x, findSp=c("\\(","\\)","\\$"), replBy="_") {
  ## in character vector 'x' replace special character 'findSp' and replace by 'replBy'
  ## note that spec characters must pe presented with protection for grep & sub
  if(length(findSp) > 1 & length(replBy)==1) replBy <- rep(replBy, length(findSp))
  for(i in 1:length(findSp)) { xx <- grep(findSp[i],x)
    if(length(xx) >0) x <- gsub(findSp[i],replBy[i],x)}
  x}


#' Check argument names
#'
#' This function allows checking of argument names 
#' 
#' @param x (character) main input
#' @param argNa (character) argument name
#' @param lazyEval (logical) decide if argument should be avaluated with abbreviated names, too
#' @return This function returns a elongated character vector
#' @seealso  \code{\link[base]{chartr}}
#' @examples
#' .checkArgNa("Abc",c("ab","Ab","BCD"))
#' @export
.checkArgNa <- function(x, argNa, lazyEval=TRUE) {
   ## check character vector 'x' if any of its elements may be (lazy avaluation) argument names of vector 'argNa'
   ## potentially calling too many conflicts : eg will call conflict for single character even when saveral 'argNa' start with this letter (ie invalid for calling fx)
   ## return logical vector at length & names of 'x'
   isArgNa <- sapply(x, nchar) <1
   zz <- naOmit(match(argNa, x))
   if(length(zz) >0) isArgNa[zz] <- TRUE
   if(lazyEval) {
     tmp <- .cutStr(argNa[1], startFr=1, reverse=TRUE)[-1]
     tmp <- sapply(x, function(z) z %in% tmp)
     if(any(tmp, na.rm=TRUE)) isArgNa[which(tmp)] <- TRUE
   } else {zz <- naOmit(match(argNa,x)); if(length(zz)>0) isArgNa[zz] <- TRUE}
  isArgNa }
   
