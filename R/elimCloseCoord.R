#' Eliminate close (overlapping) points (in x & y space)
#'
#' \code{elimCloseCoord} reduces number of rows in 'dat' by eliminating lines where x & y coordinates (columns of matrix '\code{dat}' defined by '\code{useCol}') are identical (overlay points) or very close.
#' The stringency for 'close' values may be fine-tuned using \code{nDig}), this function uses internally \code{\link{firstOfRepeated}}.
#'
#' @param dat matrix (or data.frame) with main numeric input
#' @param useCol (numeric) index for numeric columns of 'dat' to use/consider
#' @param elimIdentOnly (logical) if TRUE, eliminate real duplicated points only (ie identical values only)
#' @param refine (numeric) allows increasing stringency even further  (higher 'refine' .. more lines considered equal)
#' @param nDig (integer) number of significant digits used for rounding, if two 'similar' values are identical after this rounding the second will be eliminated. 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return resultant matrix/data.frame
#' @seealso \code{\link{findCloseMatch}}, \code{\link{firstOfRepeated}}
#' @examples
#' da1 <- matrix(c(rep(0:4,5),0.01,1.1,2.04,3.07,4.5),nc=2); da1[,1] <- da1[,1]*99; head(da1)
#' elimCloseCoord(da1)
#' @export
elimCloseCoord <- function(dat,useCol=1:2,elimIdentOnly=FALSE,refine=2,nDig=3,callFrom=NULL,silent=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="elimCloseCoord")
  argN <- deparse(substitute(x))
  refine <- 1/refine
  if(is.null(rownames(dat))) rownames(dat) <- 1:nrow(dat)
  firstOfRep <- firstOfRepeated(paste(dat[,useCol[1]],dat[,useCol[2]],sep="_"))$indUniq
  if(!elimIdentOnly) {
    if(nDig <3) message(fxNa," the stringency chosen (of ",nDig," significant digits) is very high, you may consider using 'nDig' values higher than 2 !")
    dat1 <- as.matrix(dat[,useCol])
    if(is.character(dat1)) dat1 <- matrix(as.numeric(dat1),ncol=2)                   
    nDig <- lapply(as.data.frame(dat1), function(x) signif(max(x,na.rm=TRUE),nDig) )      # numb of digits after decimal for max
    nDig <- sapply(nDig,function(x) nchar(x)-nchar(round(x))-1)
    nDig[nDig <0] <- 0
    firstOfRep <- firstOfRepeated(paste(round(refine*dat1[,1],nDig[1]),
      round(refine*dat1[,2],nDig[2]),sep="_"))$indUniq
    if(!silent) message(fxNa," reducing '",argN,"' from ",nrow(dat)," to ",length(firstOfRep)," lines")
    dat <- dat[firstOfRep,] }
  dat }
   
