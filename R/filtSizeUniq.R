#' Filter for unique elements
#' 
#' This function aims to identify and remove duplicated elements in list and maintaining the list-structure in the output. 
#' \code{filtSizeUniq}  filters 'x' (list of character-vectors or character-vector) for elements being unique (to 'ref' or if NULL to all 'x') and of character length. 
#' Note : This functions is fairly slow with long lists !!
#' @param x list of character-vectors or character-vector
#' @param ref (character) if not NULL used as alternative 'reference' for considering elements of 'x' as unique
#' @param minSize (integer) minimum number of characters
#' @param maxSize (integer) maximum number of characters
#' @param filtUnique (logical) if TRUE return unique-only strings, if FALSE return only repeated strings
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
filtSizeUniq <- function(x,ref=NULL,minSize=5,maxSize=36,filtUnique=TRUE,silent=TRUE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="filtSizeUniq")
  if(length(ref) <1) ref <- x
  if(length(minSize)==1 & length(maxSize)==1) x <- lapply(x,.filtSize,min=minSize,max=maxSize)
  chNch <- nchar(unlist(x,use.names=FALSE))
  if(all(chNch <2)) stop(" check input x and/or min/max filtering : nothing left ...")
  ref <- table(if(is.list(ref)) unlist(ref) else ref)
  ref <- names(ref)[which(if(filtUnique) ref <2 else ref >1)]                 # switch unique only or multi-hit
  if(length(ref) <1) stop(" nothing ",if(filtUnique) "unique" else "repeated"," !!")
  xLe <- sapply(x,length)
  names(xLe) <- names(x)
  uni <- unlist(x,use.names=FALSE)                    # names of unlist() not usable since counter gets added wo separator
  chUni <- uni %in% ref
  if(filtUnique) uni[which(if(filtUnique) !chUni else chUni)] <- NA
  if(filtUnique){ couUni <- table(chUni); message(" filtering  ",couUni[1]," unique and ",couUni[2]," non-unique")}
  out <- tapply(uni,rep(names(xLe),xLe),function(y) as.character(naOmit(y)))      
  out }

#' @export
.filtSize <- function(x,minSize=5,maxSize=36) {nCha <- nchar(x); x[which(nCha >= minSize & nCha <= maxSize)]}      # filter by size (no of characters)
   
