#' Correct vector to unique
#'
#' \code{correctToUnique} checks 'x' for unique entries, while maintaining the original length. If necessary a counter will added to non-unique entries. 
#' @param x input character vector
#' @param sep (character) separator used when adding counter
#' @param atEnd (logical) decide location of placing the counter (at end or at beginning of initial text)
#' @param maxIter (numeric) max number of iterations
#' @param NAenum (logical) if \code{TRUE} all \code{NA}s will be enumerated (NA_1,NA_2,...)
#' @param callFrom (character) for better tracking of use of functions
#' @return character vector
#' @seealso \code{\link[base]{unique}} will simply remove repeated elements, ie length of 'x' won't remain constant, \code{\link{filtSizeUniq}} is more complex and slower, \code{\link{treatTxtDuplicates}}
#' @examples
#' correctToUnique(c("li0","n",NA,NA,rep(c("li2","li3"),2),rep("n",4))) 
#' @export
correctToUnique <- function(x,sep="_",atEnd=TRUE,maxIter=4,NAenum=TRUE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="correctToUnique")
  chNA <- is.na(x)
  if(NAenum & any(chNA)) x[which(chNA)] <- "NA"
  dupR <- duplicated(x, fromLast=FALSE)
  dupL <- duplicated(x, fromLast=TRUE) 
  anyDu <- anyDx <- dupL | dupR
  if(any(anyDu)) { anyDu <- which(anyDu)
    xIni <- x
    x[which(!dupR & dupL)] <- if(atEnd) paste(x[which(!dupR & dupL)],"1",sep=sep) else paste("1",x[which(!dupR & dupL)],sep=sep)
    dupRx0 <- dupR[anyDu]
    iter <- 2
    finished <- FALSE
    while(iter <= maxIter & !finished){
      dupRX <- duplicated(x[anyDu], fromLast=FALSE)
      if(any(dupRX)) {sel <- which(!dupRX & dupRx0); x[anyDu[sel]] <- if(atEnd) paste(x[anyDu[sel]],iter,sep=sep) else paste(iter,x[anyDu[sel]],sep=sep)
        iter <- iter + 1
        dupRx0 <- dupRX
      } else {x[anyDu[which(dupRx0)]] <- if(atEnd) paste(x[anyDu[which(dupRx0)]],iter,sep=sep) else paste(iter,x[anyDu[which(dupRx0)]],sep=sep)
        finished <- TRUE} }
    if(!NAenum) if(any(chNA)) x[which(chNA)] <- NA
    if(!finished) {                                           
      xTab <- table(xIni[anyDu])[rank(unique(xIni[anyDu]))]    
      if(any(xTab >maxIter)) for(i in names(xTab)[which(xTab > maxIter)]) {
        z <- which(x==i); x[z] <- if(atEnd) paste(x[z],(maxIter+1):xTab[which(names(xTab)==i)],sep=sep) else paste((maxIter+1):xTab[i],x[z],sep=sep)}}}
  x }

.uniqueWName <- function(x,silent=TRUE,splitSameName=TRUE,callFrom=NULL){
  ## enhanced version of unique(): return unique of vector 'x' with names (if multiple names fit to same value of 'x', use 1st of names)
  ## assumes that names of 'x' are redundant to value of 'x'
  ## 'splitSameName' .. allows keeping different names, even if with same value in 'x' (which would disappear with unique(x))
  fxNa <- .composeCallName(callFrom,newNa=".uniqueWName")
  argNa <- deparse(substitute(x))
  inv <- FALSE
  if(length(unique(x)) < length(unique(names(x))))  {
    if(splitSameName){
      if(!silent) message(fxNa,"'",argNa,"' has more names than different values, maintaining different names")
      tmp <- names(x)
      names(tmp) <- x
      x <- tmp
      inv <- TRUE
    } else if(!silent) message(fxNa,"names of  '",argNa,"' don't fit to its values, ",
      "result not representative, rather use argument 'splitSameName'=TRUE")
  }
  out <- sapply(unique(x), function(z) x[which(x==z)[1]])
  if(is.character(x)) names(out) <- substr(names(out),nchar(out)+2,nchar(names(out)))
  if(inv){
    tmp <- names(out)
    names(tmp) <- out
    out <- tmp }
  out }
   
