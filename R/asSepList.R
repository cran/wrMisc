#' Organize data as separate list-entries
#'
#' \code{asSepList} allows reorganizing list into separate numeric vectors. For example, matrixes or data.frames will be split into separate columns 
#' (differnt to \code{\link[wrMisc]{partUnlist} which maintains the original structure}. This function also works with lists of lists.
#' This function may be helpful for reorganizing data for plots.
#'
#' @param y list to be separated/split in vectors
#' @param asNumeric (logical) to transform all list-elements in simple numeric vectors (won't work if some entries are character)
#' @param fxArg (character) optinal names to exclude if any (lazy matching) matches (to exclude other arguments be misinterpreted as data, used in wrGraph::vioplot2)
#' @param minLen (integer) (currently use of this argument not implemeneted!) min length (or number of rows), as add'l element to eliminate arguments given wo names when asSepList is called in vioplot2
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return list, partially unlisted to vectors
#' @seealso \code{\link[wrMisc]{partUnlist}}, \code{\link[base]{unlist}}
#' @examples
#' bb <- list(fa=gl(2,2),c=31:33,L2=matrix(21:24,nc=2),li=list(li1=11:14,li2=data.frame(41:44)))
#' asSepList(bb)
#' lapply(bb,.asDF2)
#' partUnlist(lapply(bb,.asDF2))
#' @export
asSepList <- function(y,asNumeric=TRUE,minLen=4,fxArg=NULL,silent=FALSE,callFrom=NULL) {
  ## convert all data-series of list (ie all list elements or columns) in separate list-elements (OK with list of lists) eg for plots
  ## 'asNumeric'.. to transform all list-elements in simple numeric vectors (won't work if some entries are character)
  ## 'minLen' .. min length (or number of rows), as add'l element to eliminate arguments given wo names when asSepList is called in vioplot2
  ## 'fxArg' .. optinal, names to exclude if any (lazy matching) matches (to exclude other arguments be misinterpreted as data, used in vioplot2)
  fxNa <- .composeCallName(callFrom,newNa="asSepList")
  if(is.matrix(y)) y <- list(y) else if(!is.list(y)) y <- as.list(y)
  z <- lapply(partUnlist(y),.asDF2);
  ## check for conflicting names to 'fxArg'
  if(length(fxArg) >0) {
    chNa <- .checkArgNa(names(z),argNa=fxArg)
    if(any(chNa)) { if(!silent) message(fxNa," reducing list from ",length(z)," to ",sum(!chNa,na.rm=TRUE))
      z <- z[which(!chNa)]} }
  f1 <- function(x) if(length(dim(x)) >1) ncol(x) >1 else FALSE
  ## main : split matrixes or data.frames in separate lists
  chCol <- sapply(z,f1)
  if(any(chCol)) if(length(z)==1) z <- as.list(z[[1]]) else {
    while(any(chCol)) {
      i <- which(chCol)[1]
      x <- z[[i]]; z <- z[-i]
      z[length(z)+(1:ncol(x))] <- as.list(x)
      newNa <- colnames(x)
      if(any(newNa %in% names(z))) newNa <- paste(newNa,"2",sep="_")
      names(z)[length(z)-(ncol(x):1)+1] <- newNa 
      chCol <- sapply(z,f1) }}
  z <- if(asNumeric) lapply(z,function(x) as.numeric(as.matrix(x))) else z
  ## transform to numeric (if possible)
  if(asNumeric) {
    chDa <- rep(NA,length(z))
    for(i in 1:length(z)) chDa[i] <- mode(z[[i]])
    chDa <- chDa=="character" & sapply(z,length) > minLen
    for(i in which(chDa)) {
     if(!silent) message(fxNa," trying to convert to numeric content of series ",names(z)[i])
     z[[i]] <- convToNum(z[[i]],spaceRemove=TRUE,convert=c(NA,"sparseChar"),remove=NULL,euroStyle=TRUE,sciIncl=TRUE,callFrom=fxNa,silent=silent)}
    ## now check again & remove non-suitable
    for(i in 1:length(z)) chDa[i] <- mode(z[[i]])
    if(any(chDa=="character")) z <- z[which(chDa !="character")] }
  z }

#' @export
.asDF2 <- function(z) (if(length(dim(z)) >1) as.data.frame(z) else {if(is.list(z)) z else as.data.frame(as.matrix(z))})  # convert anything to data.frame (but not list)
  
