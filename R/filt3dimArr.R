#' Filter a three-dimensional array of numeric data
#'
#' Filtering of 3-dim array ('x') : filter column 'filtCrit' as 'larger as' (according to 'filtTy') 'filtVal' 
#' and extract/display all col matching 'displCrit'.
#'
#' @param x array (3-dim) of numeric data
#' @param displCrit (character) column-name(s) to display
#' @param filtCrit (character, length=1) which column-name consider when filtering filter with 'filtVal' and 'filtTy'
#' @param filtVal (numeric) for testing inferior/superor/equal condition 
#' @param filtTy  (character) which type of testing to perform ('eq','inf','infeq','sup','supeq', ">", '<', '>=', "<=", "==")
#' @return list of filtered matrixes (by 3rd dim)
#' @examples
#' arr1 <- array(1:24,dim=c(4,3,2),dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""),c("ch1","ch2")))
#' filt3dimArr(arr1,displCrit=c("col1","col2"),filtCrit="col2",filtVal=7)
#' @export
filt3dimArr <- function(x,displCrit,filtCrit,filtVal,filtTy=">"){
  if(length(dim(x)) != 3) stop("expecting 3 array as input for x")
  if(is.character(displCrit)) if(any(!displCrit %in% colnames(x))) stop(" can't find 'displCrit' in colnames of 'x'")
  if(is.character(filtCrit)) if(any(!filtCrit %in% colnames(x))) stop(" can't find 'filtCrit' in colnames of 'x'")
  ##
  out <- list()
  out[[1]] <- x[.filterSw(x[,filtCrit,1],">",filtVal,indexRet=TRUE),displCrit,1]
  if(dim(x)[3] >1) for(i in 2:(dim(x)[3])) out[[i]] <- x[.filterSw(x[,filtCrit,i],">",filtVal,indexRet=TRUE),displCrit,i]
  out }

#' @export
.filterSw <- function(x,fiTy,checkVa,indexRet=TRUE){
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
   
