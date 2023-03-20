#' Extract pair of numeric values from vector or column-names
#'
#' This function extracts a pair of numeric values out of a vector or colnames (from a matrix). 
#' This is useful when pairwise comparisons are concatenated like '10c-100c', return matrix with 'index'=selComp, log2rat and both numeric.
#' Additional white space or character text can be removed via the argument \code{stripTxt}.
#' Of course, the separator \code{sep} needs to be specified and should not be included to 'stripTxt'.
#'
#' @param dat (matrix or data.frame) main input
#' @param selComp (character) the column index selected 
#' @param stripTxt (character, max length=2) text to ignore, if NULL heading letter and punctuation characters will be removed; default will remove all letters (and following spaces)
#' @param sep (character, length=1) separator between pair of numeric values to extract
#' @param columLabel (character) column labels in output
#' @param sortByAbsRatio (logical) optional sorting of output by (absolute) log-ratios (most extreme ratios on top)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return This function returns a matrix
#' @seealso \code{\link[base]{strsplit}} and help on regex 
#' @examples
#' ##  composed column names
#' mat1 <- matrix(1:8, nrow=2, dimnames=list(NULL, paste0(1:4,"-",6:9)))
#' numPairDeColNames(mat1)
#' numPairDeColNames(colnames(mat1))
#' ##  works also with simple numeric column names
#' mat2 <- matrix(1:8, nrow=2, dimnames=list(NULL, paste0("a",6:9)))
#' numPairDeColNames(mat2)
#' @export
numPairDeColNames <- function(dat, selComp=NULL, stripTxt=NULL, sep="-", columLabel="conc", sortByAbsRatio=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ##  extract pair of numeric content of colnames from pairwise comparisons, return matrix with 'index'=selComp, log2rat and both numeric 
  ## dat (matrix or data.frame) testing results (only the colnames will be used)
  ## selComp (integer) the column index selected
  ## stripTxt (character) text to be removed, if NULL letter and punctuation characters will be removed; default will remove all letters (and following spaces)
  ## sep (character) separator (for separating multiple numeric parts out of character-string)
  ## columLabel (character) used for column labels in output
  ## sortByAbsRatio (logical) sort output by (absolute) log-ratios
  fxNa <- .composeCallName(callFrom, newNa="numPartDeColNames")
  if(length(dim(dat)) >1) dat <- colnames(dat)  
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(length(dat) <1) stop("Invalid entry of 'dat'")
  if(length(selComp) >0) {
    if(is.numeric(selComp)) dat <- dat[as.integer(selComp)] else {
      if(any(selComp %in% dat)) dat[selComp] else message(fxNa," invalid entry for 'selComp', ignoring")}
  } else selComp <- 1:length(dat)
  if(length(stripTxt) <1) stripTxt <- "[[:alpha:]]+[[:space:]]*[[:alpha:]]*|[[:space:]]+[[:alpha:]]+[[:space:]]*[[:alpha:]]*|[[:space:]]+"
  chSep <- nchar(sub(stripTxt,"",paste0("A",sep,"B")))
  if(chSep !=3) message(fxNa,"PROBLEM ? : 'stripTxt' does REMOVE the separator 'sep' ! Select a different separator or 'stripTxt' strategy to resolve pairwise combinations !") 
  dat <- if(length(stripTxt) >1) sub(stripTxt[2],"",sub(stripTxt[1],"",dat)) else gsub(stripTxt,"",dat)
  chSep <- grep(sep, dat)
  if(length(chSep) <1) {
    if(!silent) message(fxNa,"Could not find any instance of separator 'sep'; (maybe removed with 'stripTxt' ?)")
    logRat <- try(as.integer(dat), silent=TRUE)       # try remove text at start or end
    if(inherits(logRat, "try-error")) stop(fxNa," Did not succed to extract (single) numeric content")
    logRat <- cbind(index=selComp, logRat=NA, logRat)
    colnames(logRat)[3] <- columLabel 
  } else {
    logRat <- try(as.integer( unlist(strsplit(dat, sep))), silent=TRUE)
    if(inherits(logRat, "try-error")) stop(fxNa," Did not succed to extract numeric content (by splitting)")
    logRat <- matrix(logRat, ncol=2, byrow=TRUE, dimnames=list(NULL,paste0(columLabel,1:2)))
    chRat <- logRat[,1] > logRat[,2]
    if(any(chRat)) logRat[which(chRat),] <- logRat[which(chRat), 2:1]
    logRat <- cbind(index=selComp, log2rat=signif(log2(logRat[,2]/logRat[,1]),4), logRat) }
  if(sortByAbsRatio && nrow(logRat) >1) logRat <- logRat[order(if(length(chSep) <1) logRat[,3] else abs(logRat[,2]), decreasing=TRUE),]
  logRat }
   
