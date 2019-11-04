#' Transform matrix to non-ambiguous matrix (in respect to given column)
#'
#' \code{nonAmbiguousMat} makes values of matrix 'mat' in col 'byCol' unique. 
#' @param mat numeric or character matrix (or data.frame), column specified by 'byCol' must be/will be used as.numeric, 1st column of 'mat' will be considered like index & used for adding prefix 'nameMod' (unless byCol=1, then 2nd col will be used)
#' @param byCol (character or integer-index) column by which ambiguousity will be tested
#' @param uniqOnly (logical) if =TRUE return unique only, if =FALSE return unique and single representative of non-unique values (with '' added to name), selection of representative of repeated: first (of sorted) or middle if >2 instances
#' @param asList (logical) return result as list
#' @param nameMod (character) prefix added to 1st column of 'mat' (expect 'by') for indicating non-unique/ambiguous values
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return sorted non-ambigous numeric vector (or list if 'asList'=TRUE and 'uniqOnly'=FALSE)
#' @seealso for non-numeric use \code{\link{firstOfRepeated}} - but 1000x much slower !; \code{\link{get1stOfRepeatedByCol}}
#' @examples
#' set.seed(2017); mat2 <- matrix(c(1:100,round(rnorm(200),2)),ncol=3,
#'   dimnames=list(1:100,LETTERS[1:3]));
#' head(mat2U <- nonAmbiguousMat(mat2,by="B",na="_",uniqO=FALSE),n=15)
#' head(get1stOfRepeatedByCol(mat2,sortB="B",sortS="B"))
#' @export
nonAmbiguousMat <- function(mat,byCol,uniqOnly=FALSE,asList=FALSE,nameMod="amb_",callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="nonAmbiguousMat")
  if(is.character(byCol)) byCol <- naOmit(match(byCol,colnames(mat))) else {               # transform 'byCol' to index using match
    if(is.factor(byCol)) byCol <- as.numeric(as.character(byCol))}
  msg <- "Invalid 'byCol': Argument 'byCol' allows selecting one column of numeric data from 'mat', either as column-name or index"
  if(length(byCol) <1) stop(msg) else if(length(byCol) >1) {byCol <- byCol[1]; message(msg)}
  mat <- mat[order(as.numeric(mat[,byCol]),decreasing=FALSE),]
  y <- convToNum(mat[,byCol],remove=NULL,sciIncl=TRUE,callFrom=fxNa)
  ab <- which(diff(y)==0)
  onlySingleLast <- FALSE
  if(length(ab) <1) return(if(asList) list(unique=mat) else mat) else {
    if(onlySingleLast) { ac <- diff(ab)
      ac <- ab[c(which(ac >1),if(ac[length(ac)] >1) length(ab) else NULL)]}   # not yet used: only single (last) instance of repeated
    nrName <- if(is.null(rownames(mat))) rownames(mat) else mat[,if(byCol==1) 2 else 1]
    nrName[c(ab,ab+1)] <- paste(nameMod,nrName[c(ab,ab+1)],sep="")
    if(is.null(rownames(mat))) mat[,if(byCol==1) 2 else 1] <- nrName else rownames(mat) <- nrName
    out <- if(uniqOnly | asList) mat[-1*unique(sort(c(ab,ab+1))),] else NULL
    out <- if(asList) {if(uniqOnly) list(unique=out) else list(unique=out,ambig=mat[ab,])} else {if(uniqOnly) out else mat[-ab,]}
    out } }
         
