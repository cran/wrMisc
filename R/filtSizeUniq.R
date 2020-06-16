#' Filter for unique elements
#' 
#' This function aims to identify and remove duplicated elements in a list and maintain the list-structure in the output. 
#' \code{filtSizeUniq}  filters 'lst' (list of character-vectors or character-vector) for elements being unique (to 'ref' or if NULL to all 'lst') and of character length. 
#' In addition, the min- and max- character length may be filtered, too. Eg, in proteomics this helps removing peptide sequences which would not be measured/detected any way.
#'
#' @param lst list of character-vectors or character-vector
#' @param ref (character) optional alternative 'reference', if not \code{NULL} used in addition to 'lst' for considering elements of 'lst' as unique
#' @param minSize (integer) minimum number of characters, if \code{NULL} set to 0
#' @param maxSize (integer) maximum number of characters
#' @param filtUnique (logical) if \code{TRUE} return unique-only character-strings
#' @param byProt (logical) if \code{TRUE} organize output as list (by names of input, eg protein-names) - if 'lst' was named list
#' @param inclEmpty (logical) optional including empty list-elements when all elements have been filtered away - if 'lst' was named list 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return list of filtered input
#' @seealso \code{\link{correctToUnique}}, \code{\link[base]{unique}}, \code{\link[base]{duplicated}}
#' @examples
#' filtSizeUniq(list(A="a",B=c("b","bb","c"),D=c("dd","d","ddd","c")),filtUn=TRUE,minSi=NULL)
#' # input: c and dd are repeated
#' filtSizeUniq(list(A="a",B=c("b","bb","c"),D=c("dd","d","ddd","c")),ref=c(letters[c(1:26,1:3)],
#'   "dd","dd","bb","ddd"),filtUn=TRUE,minSi=NULL)  # a,b,c,dd repeated 
#' @export
filtSizeUniq <- function(lst,ref=NULL,minSize=6,maxSize=36,filtUnique=TRUE,byProt=TRUE,inclEmpty=TRUE,silent=FALSE,callFrom=NULL) {
  ## filter protein sequences for size/length and for unique
  fxNa <- .composeCallName(callFrom,newNa="filtSizeUniq")
  chNa <- grep("\\.$", names(utils::head(lst)))                                   # check for attached tailing '.'
  if(!is.list(lst)) {byProt <- FALSE; inclEmpty <- FALSE}
  if(length(chNa) <= min(2,length(lst))) names(lst) <- paste(names(lst),".",sep="")
  pep <- unlist(lst)
  chNa <- max(sapply(lst,length),na.rm=TRUE)
  if(chNa >1) names(pep) <- sub("\\.$","",names(pep))                 # remove tailing '.' of names if list-element has length=1
  nPep <- length(pep)
  nAA <- nchar(pep)
  if(length(minSize) <1) minSize <- 0
  if(length(maxSize) <1) {maxSize <- 40
    if(!silent) message(fxNa," can't understant 'maxSize', setting to default=40")}
  ## filter by size
  chAA <- nAA >= minSize & nAA <= maxSize
  if(any(!chAA)) {pep <- if(all(!chAA)) NULL else pep[which(chAA)]
    if(!silent) message(fxNa,nPep - length(pep)," out of ",nPep," peptides beyond range (",minSize,"-",maxSize,")")}
  ## filter unique /reundant
  if(filtUnique) {
    nPe2 <- length(pep)
    if(length(ref) >0) {pep0 <- pep; pep <- c(pep,unique(unlist(ref))) } else pep0 <- NULL
    chDup <- duplicated(pep,fromLast=FALSE)
    if(any(chDup)) {
      chDu2 <- duplicated(pep,fromLast=TRUE)
      if(length(ref) >0) {pep <- pep0; chDup <- chDup[1:nPe2]; chDu2 <- chDu2[1:nPe2]} 
      pep <- list(unique=pep[which(!chDu2 & !chDup)],allRedund=pep[which(!(!chDu2 & !chDup))], firstOfRed=pep[which(chDu2 & !chDup)])
      if(!silent) message(fxNa,length(pep$allRedund)," out of ",nPe2," peptides redundant")
    } else {if(length(ref) >0) {pep <- pep0; chDup <- chDup[1:nPe2]}}   
  }
  ##  
  if(byProt) { fac <- sub("\\.[[:digit:]]+$","",names(if(filtUnique) pep$unique else pep))
    pep <- tapply(if(filtUnique) pep$unique else pep,fac,function(x) x) 
    if(length(pep) <1) pep <- character() 
    if(inclEmpty) { iniPro <- sub("\\.$","",names(lst)) 
      curPro <- names(pep) 
      newNo <- sum(!iniPro %in% curPro)
      if(newNo >0){ pep[length(curPro)+(1:newNo)] <- lapply(1:newNo,function(x) character())
        names(pep)[length(curPro)+(1:newNo)] <- iniPro[which(!iniPro %in% curPro)]}
    }
  }
  pep }
  
#' @export
.filtSize <- function(x,minSize=5,maxSize=36) {nCha <- nchar(x); x[which(nCha >= minSize & nCha <= maxSize)]}      # filter by size (no of characters)
   
