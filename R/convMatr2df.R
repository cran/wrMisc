#' Convert matrix (eg with redundant) row-names to data.frame 
#'
#' This function provides flexible converting of matrix to data.frame. 
#' For example repeated/redundant rownames are not allowed in data.frame(), thus the corresponding column-names have to be renamed using a counter-suffix.
#' In case of non-redundant rownames, a new column 'addIniNa' will be introduced at beginning to document the initial (redundant) rownames, 
#' non-redundant rownames will be created.
#' Finally, this functions converts the corrected matrix to data.frame and checks/converts columns for transforming character to numeric if possible. 
#' If the input is a data.frame containing factors, they will be converted to character before potential conversion.
#' Note: for simpler version (only text to numeric) see from this package \code{.convertMatrToNum} .
#' @param mat matrix (or data.frame) to be converted
#' @param addIniNa (logical) if \code{TRUE} an additional column ('ID') with rownames will be added at beginning
#' @param duplTxtSep (character) separator for enumerating replicated names
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This functions returns a data.frame equivalent to the input matrix, an additional column named 'ID' will be added for initial rownames
#' @seealso \code{\link[base]{numeric}}, for simpler version (only text to numeric) see from this package \code{.convertMatrToNum}
#' @examples
#' dat1 <- matrix(1:10, ncol=2)
#' rownames(dat1) <- letters[c(1:3,2,5)]
#' ## as.data.frame(dat1)  ...  would result in an error
#' convMatr2df(dat1)
#' 
#' df1 <- data.frame(a=as.character((1:3)/2), b=LETTERS[1:3], c=1:3)
#' str(convMatr2df(df1))
#' 
#' df2 <- df1; df2$b <- as.factor(df2$b)
#' str(convMatr2df(df2))
#' @export
convMatr2df <- function(mat, addIniNa=TRUE, duplTxtSep="_", silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="convMatr2df")
  iniNa <- rownames(mat)
  chNR <- any(duplicated(iniNa)) & length(iniNa) >0              # non-redundant ?
  out <- NULL
  ## check&treat duplicated rownames
  if(is.matrix(mat)) {
    if(chNR){
      nrNa <- treatTxtDuplicates(iniNa, onlyCorrectToUnique=TRUE, sep=duplTxtSep, silent=silent,callFrom=fxNa)
      rownames(mat) <- NULL
      mat <- data.frame(ID=iniNa, mat, stringsAsFactors=FALSE)
      rownames(mat) <- nrNa
    } else {
      mat <- data.frame(ID=if(is.null(iniNa)) rep(NA,nrow(mat)) else iniNa, mat, stringsAsFactors=FALSE) }
  }
  ##  try converting columns from factor or text to numeric
  if(is.data.frame(mat)) {   
    chFa <- sapply(mat, inherits, "factor")
    if(any(chFa)) for(i in which(chFa)) mat[,i] <- as.character(mat[,i])
    chNum <- sapply(mat, inherits, c("integer","numeric")) 
    chTx <- sapply(mat, inherits, "character")
 
    if(colnames(mat)[1]=="ID") chTx[1] <- FALSE
    ## limit trying to columns conataining digits in all elements
    if(any(chTx)) chTx[which(chTx)] <- if(sum(chTx)==1) all(grepl("[[:digit:]]",mat[,which(chTx)])) else grepl("[[:digit:]]",mat[,which(chTx)]) 
    if(any(chTx)) for(i in which(chTx)) { tmp <- try(suppressWarnings(as.numeric(mat[,i])), silent=TRUE)
      if(!inherits(tmp, "try-error")) if(!all(is.na(tmp))) mat[,i] <- tmp }
  } else { mat <- NULL
    if(!silent) message(fxNa,"Can't convert object of class ",pasteC(class(mat), quoteC="'")," to data.frame, returning NULL") }
  mat }

#' @export
.mayBeNum <- function(x, pattern=NULL) {
  ## test if values of (simple) char vector may be numeric; return index of suitable values
  ## 'pattern' .. pattern of regular expressions (default for pos & neg values incl '.' as decimal)
  ## won't work with scientif annotations or heading or tailing spaces !!
  if(is.factor(x)) x <- as.character(x)
  if(is.null(pattern)) pattern <- "(^([0-9]+)|(^[+-][0-9]+)|(^\\.[0-9]+)|(^0\\.[0-9]+))((\\.[0-9]+)?)(([eE][+-]?[0-9]+)?)$" #"(^([0-9]+)|(^[+-][0-9]+)|(^\\.[0-9]+))((\\.[0-9]+)?)(([eE][+-]?[0-9]+)?)$"
  all(grepl(pattern, x)) }
   
