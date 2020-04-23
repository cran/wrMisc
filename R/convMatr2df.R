#' Convert matrix (eg with redundant) row-names to data.frame 
#'
#' \code{convMatr2df} provides flexible converting of matrix to data.frame. 
#' For example repeated/redundant rownames are not allowed in data.frame(), thus the corresponding column-names have to be renamed using a counter-suffix.
#' In case of non-redundant rownames, a new column 'addIniNa' will be introduced at beginning to document the initial (redundant) rownames, 
#' non-redundant rownames will be created.
#' Finally, this functions converts the corrected matrix to data.frame and checks/converts columns for transforming character to numeric. 
#' If the input is a data.frame containing factors, they will be converted to character before potential conversion.
#' Note: for simpler version (only text to numeric) see from this package \code{.convertMatrToNum} .
#' @param mat matrix (or data.frame) to be converted
#' @param addIniNa (logical) if \code{TRUE} an additional column ('ID') with rownames will be added at beginning
#' @param duplTxtSep (character) separator for enumerating replicated names
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return data.frame
#' @seealso for simpler version (only text to numeric) see from this package \code{.convertMatrToNum}
#' @examples
#' dat1 <- matrix(1:10,ncol=2)
#' rownames(dat1) <- letters[c(1:3,2,5)]
#' ## as.data.frame(dat1)  ...  would result in an error
#' convMatr2df(dat1)
#' convMatr2df(data.frame(a=as.character((1:3)/2),b=LETTERS[1:3],c=1:3))
#' tmp <- data.frame(a=as.character((1:3)/2),b=LETTERS[1:3],c=1:3,stringsAsFactors=FALSE)
#' convMatr2df(tmp)
#' tmp <- data.frame(a=as.character((1:3)/2),b=1:3,stringsAsFactors=FALSE)
#' convMatr2df(tmp) 
#' @export
convMatr2df <- function(mat,addIniNa=TRUE,duplTxtSep="_",silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="convMatr2df")
  iniNa <- rownames(mat)
  chNR <- length(iniNa) > length(unique(iniNa)) & length(iniNa) >0              # non-redundant ?
  if(is.data.frame(mat)) {y <- rep(F,ncol(mat))
    for(i in 1:ncol(mat)) y[i] <- class(mat[,i])[1] 
    if(any("factor" %in% y)) for(i in which(y =="factor")) mat[,i] <- as.character(mat[,i])}
  se <- "(^([0-9]+)|(^[+-][0-9]+)|(^\\.[0-9]+))((\\.[0-9]+)?)(([eE][+-]?[0-9]+)?)$"
  chNum <- apply(mat,2,function(x) length(.mayBeNum(as.character(x),pattern=se)))==nrow(mat)
  if(chNR){
    nrNa <- treatTxtDuplicates(iniNa,onlyCorrectToUnique=TRUE,sep=duplTxtSep,silent=silent,callFrom=fxNa)
    rownames(mat) <- NULL
    out <- data.frame(ID=iniNa,mat,stringsAsFactors=FALSE)
    rownames(out) <- nrNa
  } else {
    out <- data.frame(ID=if(is.null(iniNa)) rep(NA,nrow(mat)) else iniNa,mat,stringsAsFactors=FALSE) }
  for(i in 2:ncol(out)) class(out[,i]) <- if(chNum[i-1]) "numeric" else "character"
  if(!addIniNa) out <- out[,-1]
  out }

#' @export
.mayBeNum <- function(x,pattern=NULL) {
  ## test if values of (simple) char vector may be numeric; return index of suitable values
  ## 'pattern' .. pattern of regular expressions (default for pos & neg values incl '.' as decimal)
  ## won't work with scientif annotations or heading or tailing spaces !!
  if(is.factor(x)) x <- as.character(x)
  if(is.null(pattern)) pattern <- "(^([0-9]+)|(^[+-][0-9]+)|(^\\.[0-9]+))((\\.[0-9]+)?)(([eE][+-]?[0-9]+)?)$"
  grep(pattern,x) }
   
