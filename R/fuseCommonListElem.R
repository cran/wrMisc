#' Fuse content of list-elements with redundant (duplicated) names
#'
#' \code{fuseCommonListElem} fuses (character or numeric) elements of list re-occuring under same name, so that resultant list has unique names.
#' Note : will not work with list of matrixes
#'
#' @param lst (list) main input, list of numeric vectors
#' @param initOrd (logical) preserve initial order in output (if TRUE) or otherwise sort alphabetically
#' @param removeDuplicates (logical) allow to remove duplicate entries (if vector contains names, both the name and the value need to be identical to be removed; note: all names must have names with more than 0 characters to be considered as names)  
#' @param callFrom (character) allows easier tracking of message(s) produced 
#' @return fused list (same names as elements of input)
#' @seealso \code{\link[base]{unlist}}
#' @examples
#' val1 <- 10 +1:26
#' names(val1) <- letters
#' lst1 <- list(c=val1[3:6],a=val1[1:3],b=val1[2:3],a=val1[12],c=val1[13])
#' fuseCommonListElem(lst1)
#' @export
fuseCommonListElem <- function(lst,initOrd=TRUE,removeDuplicates=FALSE,callFrom=NULL) {
  ## fuse (character or numeric) elements of list re-occuring under same name, so that resultant list has unique names
  ## will not work with list of matrixes
  ## return fused list (same names as elements of input)
  fxNa <- .composeCallName(callFrom,newNa="fuseCommonListElem")
  chDim <- sapply(lst, function(x) length(dim(x)) < 2)
  if(any(!chDim)) stop(fxNa, " need list of numeric vectors for fusing !")
  chDup <- duplicated(names(lst),fromLast=FALSE)
  if(any(chDup)) {
    iniNa <- names(lst)
    chDup <- chDup | duplicated(names(lst), fromLast = TRUE)
    tmp <- lst[which(chDup)]
    tmp <- tmp[order(names(tmp))]
    nMa <- sapply(tmp, length)
    names(nMa) <- names(tmp)
    names(tmp) <- NULL
    tmp2 <- tapply(unlist(tmp), factor(rep(names(nMa), nMa)), function(x) x)
    if(removeDuplicates) {
      hasNa <- sapply(tmp2,function(x) all(nchar(names(x)) >0) )
      chDu <- if(!all(hasNa)) lapply(tmp2,duplicated,fromLast=FALSE) else {
        lapply(tmp2,function(x) duplicated(x) & duplicated(names(x)))}
      hasDu <- sapply(chDu,any)
      if(any(hasDu)) {
        tmp2[which(hasDu)] <- lapply(tmp2[which(hasDu)],function(x) {
          if(all(nchar(names(x)) >0)) x[which(!duplicated(x) & !duplicated(names(x)))] else x[which(!duplicated(x))] }) }} 
    ## now need to re-integrate modified elements
    chDuR <- duplicated(names(lst),fromLast=TRUE)
    lst <- lst[which(!chDup & !chDuR)]
    suplLi <- length(lst) + (1:length(tmp2))
    lst[suplLi] <- tmp2
    names(lst)[suplLi] <- names(tmp2)
    if(initOrd) lst <- lst[match(unique(iniNa),names(lst))]
  }
  lst}
   
