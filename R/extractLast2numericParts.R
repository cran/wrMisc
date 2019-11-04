#' Extract last two numeric parts from character vector
#'
#' \code{extractLast2numericParts} extracts last 2 (integer) numeric parts between punctuations out of character vector 'x'.
#' Runs faster than \code{gregexpr} .
#' Note: won't work correctly with decimals or exponential signs !! (such characters will be considered as punctuation, ie as separator)
#' @param x main character input
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return (numeric) matrix with 2 columns (eg from initial concatenated coordinates) 
#' @seealso \code{gregexpr} from \code{\link[base]{grep}}
#' @examples
#' extractLast2numericParts(c("M01.1-4","M001/2.5","M_0001_03-16","zyx","012","a1.b2.3-7,2"))
#' @export
extractLast2numericParts <- function(x,silent=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="extractLast2numericParts")
  z <- gsub("[[:alpha:]]|[[:blank:]]","",x)                                     # simplify: remove any text or blank characters
  aa <- strsplit(z,"[[:punct:]]",fixed=FALSE)
  chLe <- sapply(aa,length) >1
  if(any(!chLe)) {aa <- aa[which(chLe)]; if(!silent) message(fxNa,sum(!chLe)," elements not fitting -> discard")}
  out <- t(sapply(aa,function(y) {le <- length(y); as.numeric(c(y[le-(1:0)]))}))
  rownames(out) <- if(any(chLe)) x[which(chLe)] else x
  out}
    
