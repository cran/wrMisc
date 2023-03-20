#' Filter three-dimensional array of numeric data
#'
#' Filtering of matrix or (3-dim) array \code{x} : filter column according to \code{filtCrit} (eg 'inf') and threshold \code{filtVal}
#'
#' and extract/display all col matching 'displCrit'.
#'
#' @param x array (3-dim) of numeric data
#' @param filtVal (numeric, length=1) for testing inferior/superor/equal condition
#' @param filtTy  (character, length=1) which type of testing to perform (may be 'eq','inf','infeq','sup','supeq', '>', '<', '>=', '<=', '==')
#' @param filtCrit (character, length=1) which column-name consider when filtering filter with 'filtVal' and 'filtTy'
#' @param displCrit (character) column-name(s) to display
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @seealso \code{\link{filterList}}; \code{\link{filterLiColDeList}};
#' @return This function returns a list of filtered matrixes (by 3rd dim)
#' @examples
#' arr1 <- array(11:34, dim=c(4,3,2), dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""), c("ch1","ch2")))
#' filt3dimArr(arr1,displCrit=c("col1","col2"),filtCrit="col2",filtVal=7)
#' @export
filt3dimArr <- function(x, filtVal, filtTy=">", filtCrit=NULL, displCrit=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="filt3dimArr")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(dim(x)) != 3) stop("Expecting 3-dim array as input for 'x'")
  if(debug) message(fxNa,"f3d1")
  if(is.character(displCrit) && length(displCrit) >0) { chCrit <- match(displCrit, colnames(x))
    if(any(is.na(chCrit))) stop("Can't find (",sum(is.na(chCrit)),") 'displCrit' in colnames of 'x'") else displCrit <- chCrit}
  if(is.character(filtCrit) && length(filtCrit) >0) { chCrit <- match(filtCrit, colnames(x))
    if(any(is.na(chCrit))) stop("Can't find (",sum(is.na(chCrit)),") 'filtCrit' in colnames of 'x'") else filtCrit <- chCrit}
  if(length(filtCrit) <1) filtCrit <- 1:ncol(x)
  if(length(displCrit) <1) displCrit <- 1:ncol(x)
  if(length(displCrit) <1) displCrit <- colnames(x)
  if(length(filtTy) <1) filtTy <- ">" else filtTy <- filtTy[1]
  if(!(filtTy %in% c("inf","infeq","sup","supeq","eq",">",">=","<","<=","=="))) {
    stop("cannot identify type of filter specified") }
  ##
  out <- list()
  out[[1]] <- x[.filterSw(x[,filtCrit,1], fiTy=filtTy, checkVa=filtVal, indexRet=TRUE), displCrit,1]  # start with 1st di
  if(dim(x)[3] >1) for(i in 2:(dim(x)[3])) out[[i]] <- x[.filterSw(x[,filtCrit,i], fiTy=filtTy, checkVa=filtVal, indexRet=TRUE), displCrit, i]
  out }


#' Filter 3-dim array of numeric data (main)
#'
#' Filtering of matrix or array \code{x} (may be 3-dim array) according to \code{fiTy} and \code{checkVa}
#'
#'
#' @param x array (3-dim) of numeric data
#' @param fiTy  (character) which type of testing to perform ('eq','inf','infeq','sup','supeq', '>', '<', '>=', '<=', '==')
#' @param checkVa (logical) s
#' @param indexRet (logical) if \code{TRUE} (default) rather return index numbers than filtered values
#' @seealso  \code{\link{filt3dimArr}}; \code{\link{filterList}}; \code{\link{filterLiColDeList}};
#' @return This function returns either index (position within 'x') or concrete (filtered) result
#' @examples
#' arr1 <- array(11:34, dim=c(4,3,2), dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""),c("ch1","ch2")))
#' filt3dimArr(arr1,displCrit=c("col1","col2"),filtCrit="col2",filtVal=7)
#' .filterSw(arr1, fiTy="inf", checkVa=7)
#' @export
.filterSw <- function(x, fiTy, checkVa, indexRet=TRUE){
  ## filterswitch function : filter values of 'x' to satisfy 'check' (vector of FALSE or TRUE)
  ## 'indexRet' .. if TRUE rather return index numbers than filtered values
  ## returns either index (position within 'x') or concrete (filtered) result
  if(fiTy == "==") fiTy <- "eq"
  if(fiTy == "<") fiTy <- "inf"
  if(fiTy == "<=") fiTy <- "infeq"
  if(fiTy == ">") fiTy <- "sup"
  if(fiTy == ">=") fiTy <- "supeq"
  if(!(fiTy %in% c("inf","infeq","sup","supeq","eq",">",">=","<","<=","=="))) {
    stop("cannot identify type of filter specified") }
  if(indexRet)  {
    switch(fiTy,
      eq = which(x == checkVa),
      sup = which(x > checkVa),
      supeq = which(x >= checkVa),
      inf = which(x < checkVa),
      infeq = which(x <= checkVa) )
  } else {
    switch(fiTy,
      eq = x[which(x == checkVa)],
      sup = x[which(x > checkVa)],
      supeq = x[which(x >= checkVa)],
      inf = x[which(x < checkVa)],
      infeq = x[which(x <= checkVa)] )
  } }
  
