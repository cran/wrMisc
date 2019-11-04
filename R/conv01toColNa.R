#' Convert matrix of integer to matrix of x-times repeated column-names
#'
#' \code{conv01toColNa} transforms matrix of integers (eg 0 and 1) to repeated & concatenated text from 'colNa', 
#' the character string for 0 occurances 'zeroTex' may be customized.
#' Used eg when specifying (and concatenating) various counted elements (eg properties) along a vector like variable peptide modifications in proteomics. 
#'
#' @param mat input matrix (with integer values)
#' @param colNa alternative (column-)names to the ones from 'mat' (default colnames of mat)
#' @param zeroTex text to display if 0 (default "")
#' @param pasteCol (logical) allows to collapse all columns to single chain of characters in output 
#' @return character vector
#' @examples
#' (ma1 <- matrix(sample(0:3,40,repl=TRUE),ncol=4,dimnames=list(NULL,letters[11:14])))
#' conv01toColNa(ma1)
#' conv01toColNa(ma1,colNa=LETTERS[1:4],ze=".")
#' conv01toColNa(ma1,colNa=LETTERS[1:4],pasteCol=TRUE)
#' @export
conv01toColNa <- function(mat,colNa=NULL,zeroTex="",pasteCol=FALSE){
  txt <- "'mat' should be matrix"
  collTx <- ""
  if(length(dim(mat)) !=2) stop(txt) else if(any(dim(mat)==0)) stop(txt," with >0 rows & cols")
  if(is.data.frame(mat)) {ch <- as.numeric(as.matrix(mat))
    if(!is.numeric(ch)) stop(txt," cannot transform to matrix of numeric")
    mat <- matrix(ch,nrow=nrow(mat),dimnames=dimnames(mat))}
  if(is.null(colNa)) colNa <- colnames(mat) else {if(length(colNa) != ncol(mat)) colNa <- colnames(mat)}
  out <- matrix(zeroTex,nrow=nrow(mat),ncol=ncol(mat),dimnames=dimnames(mat))
  for(j in 1:ncol(mat)) { tmp <- which(mat[,j] >0)
    if(length(tmp) >0) out[tmp,j] <- if(max(mat[tmp,j]) >1) sapply(mat[tmp,j],function(x) paste(rep(colNa[j],x),collapse=collTx)) else colNa[j]}
  if(pasteCol & ncol(mat) >1) { sepPa <- ""           
    tmp <- if(ncol(mat)==2) paste(out[,1],out[,2],sep=sepPa) else paste(out[,1],out[,2],out[,3],sep=sepPa)
    if(ncol(mat) >3) for(j in 4:ncol(mat)) tmp <- paste(tmp,out[,j],sep=sepPa)
    out <- as.matrix(tmp)}
  out }
    
