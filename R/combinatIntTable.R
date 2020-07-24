#' Planing for making all multiplicative combinations
#'
#' Provide all combinations for each of n elements of vector 'nMax' (positive integer, eg number of max multiplicative value). 
#' For example, imagine, we have 3 cities and the (maximum) voting participants per city.
#' Results must be read vertically and allow to see all total possible compositons.
#' 
#' @param nMax (positive integer) could be max number of voting participants form different cities, eg Paris max 2 persons, Lyon max 1 person ...
#' @param include0 (logical) include 0 occurances, ie provide al combinations starting from 0 or from 1 up to nMax 
#' @param asList (logical) return result as list or as array
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return list or array (as 2- or 3 dim) with possible number of occurances for each of the 3 elements in nMax. Read results vertical : out[[1]] or out[,,1] .. (multiplicative) table for 1st element of nMax; out[,,2] .. for 2nd
#' @seealso  \code{\link[utils]{combn}} 
#' @examples
#' combinatIntTable(c(1,1,1,2), include0=TRUE, asList=FALSE, silent=TRUE)
#' ## Imagine we have 3 cities and the (maximum) voting participants per city :
#' nMa <- c(Paris=2, Lyon=1, Strasbourg=1)
#' combinatIntTable(nMa, include0=TRUE, asList=TRUE, silent=TRUE) 
#' @export
combinatIntTable <- function(nMax,include0=TRUE,asList=FALSE,callFrom=NULL,silent=TRUE){
  ##  used by .parCombinateAllAndSum(), combinateAllAndSum()
  fxNa <- .composeCallName(callFrom, newNa="combinatIntTable")
  iniNa <- names(nMax)
  ch <- as.integer(nMax)
  msg <- "'nMax' should be positive integer (numeric) of length >1"
  if(!is.integer(ch) | length(nMax) <1) stop(msg) else nMax <- ch
  ch <- is.na(nMax) | nMax <1
  if(any(ch)) {if(sum(!ch) >0) {nMax <- nMax[!ch]; iniNa <- iniNa[!ch]} else stop(msg)
    if(!silent) message(fxNa,"trimming to ",sum(!ch)," positive non-NA")}
  ## main
  k <- if(include0) 0:1 else 1:0
  if(length(nMax) <2) {out <- as.matrix(k[1]:nMax); return(if(!asList) out else list(out))}       # case of single valid nMax (improve ?)
  diM <- c(k[2] +nMax[2], prod(nMax[-2] +k[2]))
  out <- list(matrix(rep(k[1]:nMax[1], each=diM[1]), nrow=diM[1], ncol=diM[2]))
  if(length(nMax) >1) out[[2]] <- matrix((k[1]:nMax[2]), nrow=diM[1], ncol=diM[2])
  if(length(nMax) >2) for(i in 3:length(nMax)) out[[i]] <- matrix(rep(k[1]:nMax[i], each=prod(k[2]+nMax[1:(i-1)])), nrow=diM[1], ncol=diM[2])
  if(!asList) {
    out <- array(unlist(out), dim=c(dim(out[[1]]), length(nMax)))
    dimnames(out)[[3]] <- iniNa } else names(out) <- iniNa
  out }
   
