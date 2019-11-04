#' Combine/reduce redundant lines based on specified column 
#'
#' This function works similar to \code{unique}, but it takes a matrix as input and considers one specified column to find unique instances.
#' It identifies 'repeated' lines of the input-matrix (or data.frame) 'mat' based on (repeated) elements in/of column with name 'colNa' (or column-number).
#' Redundant lines (ie repeated lines) will disappear in output.
#' Eg used with extracted annotation where 1 gene has many lines for different GO annotation.
#'
#' @param mat input matrix or data.frame
#' @param colNa character vector (length 1) macting 1 column name (if mult only 1st will be used), in case of mult matches only 1st used
#' @param sep (character) separator (default=",")
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix containing the input matrix without lines considered repeated (unique-like)
#' @seealso \code{\link{findRepeated}}, \code{\link{firstOfRepLines}}, \code{\link{organizeAsListOfRepl}}
#' @examples
#' matr <- matrix(c(letters[1:6],"h","h","f","e",LETTERS[1:5]),ncol=3,
#'   dimnames=list(letters[11:15],c("xA","xB","xC")))
#' combineRedBasedOnCol(matr,colN="xB")
#' combineRedBasedOnCol(rbind(matr[1,],matr),colN="xB")
#' @export
combineRedBasedOnCol <- function(mat,colNa,sep=",",silent=FALSE,callFrom=NULL){
  ## combine lines of matrix (or data.frame) 'mat' based on (repeated) elements in/of column with name 'colNa' (or column-number).
  ## 'colNa' ... 
  ## 'sep' ...  character vector (length 1) as separator when concatenating text-fields
  ## redundant lines (ie repeated lines) will disappear in output
  ## eg used with extracted annotation where 1 gene has many lines for different GO annot
  fxNa <- .composeCallName(callFrom,newNa="combineBasedOnCol")
  argN <- deparse(substitute(mat))
  msg <- paste(" expecting matrix or data.frame with min 2 columns in '",argN,"'")
  if(length(dim(mat)) <2) stop(msg) else if(ncol(mat) <2) stop(msg)
  if(is.data.frame(mat)) mat <- as.matrix(mat)
  msg <- c(fxNa,"argument 'colNa'"," longer than 1"," not found","using only 1st element")
  if(length(colNa) >1) {if(!silent) message(msg[-4]); colNa <- colNa[1]}
  if(is.numeric(colNa)) { if(colNa > ncol(mat)) stop else colNa <- which(colnames(mat) ==colNa)[1]
  } else if(!colNa %in% colnames(mat)) {colNa <- which(colnames(mat) ==colNa)[1]; message(msg[-3])}
  if(length(sep) <1) {sep <- ","} else if(length(sep) >1) {sep <- sep[1]}
  chCol <- which(colnames(mat) %in% colNa)
  if(length(chCol) <0) stop(" no columns in '",argN,"' corresponding to '",colNa,"' found")
  refCo <- chCol[1]
  chRef <- table(mat[,refCo])
  if(any(is.na(mat[,refCo])) &!silent) {message(fxNa," trouble ahead : column ",colnames(mat)[chRef]," in '",argN,"' contains NAs !!")}
  if(length(chRef) <nrow(mat)) {                             # has repeated elements
    matLst <- by(mat,as.numeric(as.factor(mat[,refCo])),as.matrix)
    out <- t(sapply(matLst,apply,2,function(y) paste(unique(y),collapse=",")))
    out <- out[match(unique(mat[,refCo]),unique(out[,refCo])),]       # bring in order of (non-red) initial
    if(length(dim(out)) <2) out <- matrix(out,nrow=1,dimnames=list("1",names(out)))
  } else out <- mat
  out }
   
