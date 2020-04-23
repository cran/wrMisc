#' Filter for unique elements
#' 
#' This function aims to identify and remove duplicated elements in a list and maintain the list-structure in the output. 
#' \code{filtSizeUniq}  filters 'lst' (list of character-vectors or character-vector) for elements being unique (to 'ref' or if NULL to all 'lst') and of character length. 
#' In addition, the min- and max- character length may be filtered, too. Eg, in proteomics this helps removing peptide sequences which would not be measured/detected any way.
#'
#' @param lst (list) main input, each vector, matrix or data.frame in this list will be filtered if its length or number of lines fits to \code{filt}
#' @param filt (logical) vector of \code{FALSE/TRUE} to use for filtering. If this a matrix is given, the value of \code{minLineRatio} will be applied as threshod of min content of \code{TRUE} for each line of \code{filt} 
#' @param minLineRatio (numeric) in case \code{filt} is a matrix of \code{FALSE/TRUE}, this value will be used as threshold of min content of \code{TRUE} for each line of \code{filt} 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return list of filtered input
#' @seealso \code{\link{correctToUnique}}, \code{\link[base]{unique}}, \code{\link[base]{duplicated}}, \code{\link{extrColsDeX}} 
#' @examples
#' set.seed(2020); dat1 <- round(runif(80),2)
#' list1 <- list(m1=matrix(dat1[1:40],ncol=8),m2=matrix(dat1[41:80],ncol=8),other=letters[1:8])
#' rownames(list1$m1) <- rownames(list1$m2) <- paste0("line",1:5)
#' filterList(list1, list1$m1[,1] >0.4) 
#' filterList(list1, list1$m1 >0.4) 
#' @export
filterList <- function(lst,filt,minLineRatio=0.5,silent=FALSE,callFrom=NULL) {
  ##  adjust all elements of lst to filtering
  ## minLineRatio (numeric) min ratio of columns where 
  ## assumes that all elements of lst are in correct order !
  fxNa <- wrMisc::.composeCallName(callFrom,newNa="filterList")
  if(length(filt) <1) stop(" 'filt' seems to empty")
  if(length(dim(filt)) >1) { 
    if(length(minLineRatio) <1 | !is.numeric(minLineRatio)) { minLineRatio <- 0.5
      if(!silent) message(fxNa," argument 'minLineRatio' must be numeric !  Setting to default (0.5)")}
    filt <- rowSums(filt) >= ncol(filt)*minLineRatio }
  chFi <- sub("(TRUE)|(FALSE)|T|F","",filt)
  if(any(nchar(chFi) >0)) stop(" 'filt' contains non-logical elements")
  if(is.logical(filt)) filt <- as.logical(filt)
  if(all(!filt)) stop("nothing passes filtering")  
  ## main
  if(any(!filt)) {
    nFilt <- length(filt)
    filt <- which(filt)
    lstDim <- lapply(lst,dim)
    chLst <- sapply(lstDim,length) ==2
    msg <- c(" element '","' : "," not suitable for filter")
    ## filter all matrix & data.frames
    if(any(chLst)) {
      for(i in which(chLst)) if(nrow(lst[[i]]) ==nFilt) { lst[[i]] <- if(length(filt) >1 & is.matrix(lst[[i]])) lst[[i]][filt,] else {
         matrix(lst[[i]][filt,],ncol=ncol(lst[[i]]),dimnames=list(rownames(lst[[i]])[filt],colnames(lst[[i]]))) }
      } else {
        if(!silent) message(fxNa,msg[1],names(lst)[i],msg[2],"number of lines",msg[3]) }}
    ## filter all vectors    
    chLst <- sapply(lstDim,length) ==1
    if(any(chLst)) {
      for(i in which(chLst)) if(nrow(lst[[i]]) ==nFilt) lst[[i]] <- lst[[i]][filt] else {
        if(!silent) message(fxNa,msg[1],names(lst)[i],msg[2],"length of vector",msg[3]) }}      
  } else if(!silent) message(fxNa,"all elements pass filter (nothing to remove)")
  lst }
  
