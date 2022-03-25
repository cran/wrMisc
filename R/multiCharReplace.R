#' Multiple replacement of entire character elements in simple vector, matrix or data.frame
#' 
#' This functions allows multiple types of replacements of entire character elements in simple vector, matrix or data.frame. 
#' In addtion, the result may be optionally directly transformed to logical or numeric
#' 
#' @param mat (character vector, matrix or data.frame) main data
#' @param repl (matrix or list) tells what to replace by what: If matrix the 1st oolumn will be considered as 'old' and the 2nd as 'replaceBy'; if named list, the names of the list-elements will be consdered as 'replaceBy'
#' @param convTo (character) optional conversion of content to 'numeric' or 'logical' 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns an object of same dimension as input (with replaced content) 
#' @seealso  \code{\link[base]{grep}}
#' @examples
#' x1 <- c("ab","bc","cd","efg","ghj")
#' multiCharReplace(x1, cbind(old=c("bc","efg"), new=c("BBCC","EF")))
#' 
#' x2 <- c("High","n/a","High","High","Low")
#' multiCharReplace(x2, cbind(old=c("n/a","Low","High"), new=c(NA,FALSE,TRUE)),convTo="logical")
#' 
#' # works also to replace numeric content : 
#' x3 <- matrix(11:16, ncol=2)
#' multiCharReplace(x3, cbind(12:13,112:113))
#' @export
multiCharReplace <- function(mat, repl, convTo=NULL, silent=FALSE, callFrom=NULL){
  ## multiple replacement of entire character elements in simple vector, matrix or data.frame
  ## repl .. matrix with 2 colums for old and new or list where list-names design new
  fxNa <- .composeCallName(callFrom, newNa="multiCharReplace")
  if(length(mat) <1 | (is.list(mat) & !is.data.frame(mat))) stop("Invalid argument 'mat'; must be simple vector, matrix or data.frame of length >0")
  if(is.data.frame(mat)) mat <- as.matrix(mat)
  if(is.numeric(mat) & length(convTo) <1) convTo <- "numeric"
  if(all(length(dim(repl)) ==2, dim(repl) > 0:1)) {
    ## 'repl' is matrix with 2 cols
    for(i in 1:nrow(repl)) {ch <- which(mat==repl[i,1]); if(length(ch) >0) mat[ch] <- repl[i,2]}
  } else if(is.list(repl) & length(repl) >0 & !is.null(names(repl))) {
    ## 'repl' is named list 
    for(i in 1:length(repl)) {
      ch <- which(mat %in% repl[[i]])
      if(length(ch) >0) mat[ch] <- names(repl)[i]} 
  }
  msg <- "Unable to convert to "
  if(length(convTo)==1) { if("numeric" %in% convTo & !is.numeric(mat)) {                                 
    tmp <- try(as.numeric(mat), silent=TRUE)
    if(inherits(tmp, "try-error")) {mat <- if(length(dim(mat)) <2) tmp else matrix(tmp, ncol=ncol(mat), dimnames=dimnames(mat))
    } else if(!silent) message(fxNa,msg,"numeric")
  } else if("logical" %in% convTo & !is.logical(mat)) {
    tmp <- try(as.logical(mat), silent=TRUE)
    if(!inherits(tmp, "try-error")) {mat <- if(length(dim(mat)) <2) tmp else matrix(tmp, ncol=ncol(mat), dimnames=dimnames(mat))
    } else if(!silent) message(fxNa,msg,"logical") }}
  mat }
   
