#' Reduce to first occurance of repeated lines
#'
#' This function concatenattes all columns of input-matrix and then searches like \code{unique} for unique elements, optionally the indexes of unique elements may get returned.
#' Note: This function reats input as character (thus won't understand \code{10==10.0} ).
#' Returns simplified/non-redundant vector/matrix (ie fewer lines), or respective index.
#' faster than  \code{\link{firstOfRepeated}} 
#' @param mat initial matrix to treat 
#' @param outTy for output type: 'ind'.. index to 1st occurance (non-red),'orig'..non-red lines of mat, 'conc'.. non-red concateneted values, 'num'.. index to which group/category the lines belong
#' @param useCol (integer) custom choice of which columns to paste/concatenate
#' @param callFrom (character) allows easier tracking of messages produced
#' @return simplified/non-redundant vector/matrix (ie fewer lines for matrix), or respective index
#' @seealso  \code{\link[base]{unique}}, \code{\link{nonAmbiguousNum}}, faster than \code{\link{firstOfRepeated}} which gives more detail in output (lines/elements/indexes of omitted)
#' @examples
#' mat <- matrix(c("e","n","a","n","z","z","n","z","z","b", 
#'   "","n","c","n","","","n","","","z"),ncol=2)
#' firstOfRepLines(mat,out="conc")
#' @export
firstOfRepLines <- function(mat,outTy="ind",useCol=NULL,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="firstOfRepLines")
  if(length(dim(mat)) !=2) stop(" expecting matrix or data.frame with >1 lines")
  if(!outTy %in% c("ind","orig","conc","num","all")) outTy <- "ind"
  if(is.null(useCol)) useCol <- 1:ncol(mat)
  ch <- if(nrow(mat) >1) .pasteCols(mat[,useCol],sep="") else paste(mat,collapse="")
  dup <- duplicated(ch,fromLast=FALSE)
  switch(outTy,
    ind=which(!dup),                                                            # index to 1st occurance
    orig=mat[which(!dup),],                                                     # non-red lines of mat
    conc=ch[!dup],                                                              # 1st occurance of concatenated
    num=if(any(dup)) match(ch,ch[!dup]) else 1:length(ch),
    all=list(ind=which(!dup),conc=ch[!dup],num=if(any(dup)) match(ch,ch[!dup]) else 1:length(ch)))}

#' @export
.extrNAneighb <- function(x,grp){
  ## extract values of numeric vector 'x' when NA in same group 'grp'
  ##  (used for estimatating/replacing NA by low values)
  out <- NULL
  y <- 1:length(x)
  NAgr <- (grp)[which(is.na(x))]
  for(i in unique(NAgr)) out <- c(out,naOmit(x[which(grp==i)]))
  out }

#' @export
.pasteCols <- function(mat,sep=""){
  ## paste all columns
  if(!is.matrix(mat)) mat <- as.matrix(mat)
  if(ncol(mat)==1) return(mat)
  if(nrow(mat) > ncol(mat)){
    out <- paste(mat[,1],mat[,2], if(ncol(mat) >2) mat[,3], if(ncol(mat) >3) mat[,4],sep=sep)
    if(ncol(mat) >4) for(i in 5:ncol(mat)) out <- paste(out,mat[,i])
  } else out <- apply(mat,1,paste,collapse=sep)
  names(out) <- rownames(mat)
  out }
    
