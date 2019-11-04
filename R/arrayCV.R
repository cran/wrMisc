#' CV of array
#'
#' \code{arrayCV} gets CVs for replicates in 2 or 3 dim array and returns CVs as matrix.
#' This function may be used to calculate CVs from replicate microtiter plates (eg 8x12) where replicates are typically done as multiple plates,
#' ie initial matrixes that are the organized into arrays.
#' @param arr (3-dim) array of numeric data like where replicates are along one dimesion of the array
#' @param byDim (integer) over which dimension repliates are found
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message produced
#' @return matrix of CV values
#' @seealso \code{\link[wrMisc]{rowCVs}}, \code{\link[wrMisc]{rowGrpCV}}, \code{\link[wrMisc]{replPlateCV}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' head(arrayCV(dat1,byDim=2))
#' @export
arrayCV <- function(arr,byDim=3,silent=TRUE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="arrayCV")
  curDiNa <- dimnames(arr)
  if(byDim > length(dim(arr))) { byDim <- length(dim(arr))
    message(fxNa," dimension number for argument 'byDim' too high, adjusting to ",byDim) }
  basDim <- if(byDim != 1) 1 else 2
  arrCV <- matrix(nrow=dim(arr)[basDim],ncol=dim(arr)[byDim],dimnames=curDiNa[c(basDim,byDim)] )           # (1:3)[!(1:3 %in% c(basDim,byDim))]
  if(!silent) message(fxNa," CV output in matrix/array of ",nrow(arrCV)," x ",ncol(arrCV)," ")
  if(length(dim(arr)) ==3) {
    for(i in 1:dim(arr)[byDim]) {arrCV[,i] <- if(byDim==3) rowCVs(arr[,,i]) else {
      if(byDim ==2) rowCVs(arr[,i,]) else rowCVs(arr[i,,]) }}
  } else arrCV <- matrix(rowCVs(arr),dimnames=list(rownames(arr),NULL))
  arrCV }
 
