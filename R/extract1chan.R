#' Extract just one series, ie channel, of list of arrays
#'
#' This function was designed for handeling measurements stored as list of multiple arrays, like eg compound-screens using microtiter-plates where multiple parameters ('channels') 
#' were recorded for each well (element).
#' The elements (eg compounds screened) are typcally stored in the 1st dimension of the arrays, the replicated in the secon dimension and different measure types/parameters in the 3rd chanel. 
#' In order to keep the structure of of individual microtiter-plates, typically each plate forms a separate array (of same dimensions) in a list.  
#' The this function allows extracting a single channel of the list of arrays (3rd dim of each array) and return row-appended matrix. 
#' @param arrLst (list) list of arrays (typically 1st and 2nd dim for specific genes/objects, 3rd for different measures associated with)
#' @param cha (integer) channel number
#' @param na.rm (logical) default =TRUE to remove NAs
#' @param rowSep (character) separator for rows
#' @return list with just single channel extracted
#' @seealso \code{\link[wrMisc]{organizeAsListOfRepl}}
#' @examples
#' arr1 <- array(1:24,dim=c(4,3,2),dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""),c("ch1","ch2")))
#' arr2 <- array(74:51,dim=c(4,3,2),dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""),c("ch1","ch2")))
#' arrL1 <- list(pl1=arr1,pl2=arr2)
#' extr1chan(arrL1,ch=2)
#' @export
extr1chan <- function(arrLst,cha,na.rm=TRUE,rowSep="__"){
  ## extract single channel of list of arrays (3rd dim of each array) and return row-appended matrix
  msg <- "'cha' should be numeric of length 1"
  for(pl in 1:length(arrLst)) out <- if(pl ==1) arrLst[[pl]][,,cha] else rbind(out,arrLst[[pl]][,,cha])
  plNa <- names(arrLst)
  if(is.null(plNa)) plNa <- as.character(1:length(arrLst))
  if(!is.null(rowSep)) rownames(out) <- paste(rep(plNa,sapply(arrLst,nrow)), rownames(out),sep=rowSep)
  if(na.rm) out <- out[ which(rowSums(is.na(out)) < ncol(arrLst[[1]])),]
  out }
   
