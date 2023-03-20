#' Sort matrix by two categorical and one integer columns 
#'
#' This function sorts matrix 'mat' subsequently by categorical and numerical columns of 'mat',
#'  ie lines with identical values for categor are sorted by numeric value.
#' @param mat matrix (or data.frame) from which by 2 columns will be selected for sorting
#' @param categCol (integer or character) which columns of 'mat' to be used as categorical columns
#' @param numCol (integer or character) which column of 'mat' to be used as integer columns
#' @param findNeighb (logical) if 'findNeighb' neighbour cols according to 'numCol' will be identified as groups & marked in new col 'neiGr', orphans marked as NA
#' @param decreasing (logical) order of sort
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a sorted matrix (same dimensions as 'mat')
#' @examples
#' mat <- cbind(aa=letters[c(3,rep(7:8,3:4),4,4:6,7)],bb=LETTERS[rep(1:5,c(1,3,4,4,1))],
#'   nu=c(23:21,23,21,22,18:12))
#' mat[c(3:5,1:2,6:9,13:10),]
#' sortBy2CategorAnd1IntCol(mat,cate=c("bb","aa"),num="nu",findN=FALSE,decr=TRUE)
#' sortBy2CategorAnd1IntCol(mat,cate=c("bb","aa"),num="nu",findN=TRUE,decr=FALSE)
#' @export
sortBy2CategorAnd1IntCol <- function(mat, categCol, numCol, findNeighb=TRUE, decreasing=FALSE, silent=FALSE, debug=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="sortBy2CategorAnd1IntCol")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(length(dim(mat)) <2) stop("'mat' should be matrix (or data.frame) with >2 cols and >1 line")
  if(length(numCol) <1) stop("'numCol' invalid") else if(length(numCol) >1) numCol <- numCol[1]
  if(is.numeric(numCol)) { numCol <- as.integer(numCol)
    if(numCol <1 || numCol >ncol(mat)) stop("'numCol' out of range of 'mat'")
  } else if(numCol %in% colnames(mat)) numCol <- match(numCol,colnames(mat)) else stop("cCan't find 'numCol' in colnames of 'mat'")
  dimIni <- dim(mat)
  ## main
  num <- as.numeric(mat[,numCol[1]])
  mat <-  mat[sort.list(num,decreasing=decreasing),]
  mat <- mat[sort.list(if(length(categCol)==2) paste(mat[,categCol[1]],mat[,categCol[2]]) else mat[,categCol[1]],decreasing=decreasing),] # combined sort for categor
  if(findNeighb){
    num <- as.numeric(mat[,numCol[1]])
    if(length(categCol)==2) {
      tmp <- paste(mat[,categCol[1]],mat[,categCol[2]])
      isNei <- tmp[-1]==tmp[-nrow(mat)]       
    } else isNei <- mat[-1,categCol[1]]==mat[-nrow(mat),categCol[1]]
    isNei <- c(FALSE,isNei) & c(0,num[-1] -num[-nrow(mat)])== (if(decreasing) -1 else 1)
    gr <- cumsum(!isNei)
    dup <- (duplicated(gr, fromLast=TRUE) | duplicated(gr, fromLast=FALSE))
    if(any(!dup)) gr[which(!dup)] <- NA
    mat <- cbind(mat[,1:dimIni[2]],neiGr=gr) }
  mat }
    
