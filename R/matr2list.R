#' Transform matrix to list of vectors
#'
#' convert matrix to list of vectors: each column of 'mat' as vector of list 
#' @param mat (matrix) main input
#' @param concSym (character) symbol for concatenating: concatenation of named vectors in list names as colname(s)+'concSym'+rowname
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix or array (1st dim is intraplate-position, 2nd .. plate-group/type, 3rd .. channels)
#' @seealso \code{\link{convToNum}}
#' @examples
#' mat1 <- matrix(1:12,ncol=3,dimnames=list(letters[1:4],LETTERS[1:3]))
#' mat2 <- matrix(LETTERS[11:22],ncol=3,dimnames=list(letters[1:4],LETTERS[1:3]))
#' matr2list(mat1);  matr2list(mat2)
#' @export
matr2list <- function(mat,concSym=".",silent=FALSE,callFrom=NULL) {
  if(!is.matrix(mat)) mat <- as.matrix(mat)
  isNum <- is.numeric(mat)
  outNa <- rownames(mat)
  out <- as.list(as.data.frame(rbind(colnames(mat),mat)))
  out <- lapply(out,function(x) { x <- as.character(x); nam <- x[1];     # 1st element was introduced for transmitting name of current column
    useSep <- nam=="" | outNa==""                                        # check if any of name-parts for concatenation empty -> omit concSym
    x <- if(isNum) as.numeric(x[-1]) else x[-1]                          # trim to input & reset to numeric
    names(x) <- paste(nam,ifelse(useSep,"",concSym),outNa,sep=""); x})
  out }
   
