#' Cut numeric vector to n groups (ie convert to factor) 
#'
#' \code{cutToNgrp} is a more elaborate version of \code{\link[base]{cut}} for cutting a the content of a 
#' numeric vector '\code{x}' into a given number of groups, taken from the length of '\code{lev}'.
#' Besides, this function provides the group borders/limits for convention use with legends.
#'
#' @param x numeric vector
#' @param lev (character or numeric), the length of this argument tells the number of groups to be used for cutting
#' @param NAuse (logical) include NAs as separate group
#' @param callFrom (character) for better tracking of use of functions
#' @return list with \code{$grouped} telling which element of '\code{x}' goes in which group and \code{$legTxt} with gourp-borders for convenient use with legends
#' @seealso \code{\link[base]{cut}}
#' @examples
#' set.seed(2019); dat <- runif(30) +(1:30)/2
#' cutToNgrp(dat,1:5)
#' plot(dat,col=(1:5)[as.numeric(cutToNgrp(dat,1:5)$grouped)])
#' @export
cutToNgrp <- function (x, lev, NAuse=FALSE,callFrom=NULL) {
  ##
  fxNa <- .composeCallName(callFrom,newNa="cutToNgrp")
  ra <- range(x, na.rm=TRUE)
  if(length(lev) <2) message(fxNa," 'lev' indicates to make only ",length(lev)," groups,  do you really want this ?")
  if(diff(ra) > 0) {
    ndig <- round(1.5+log(length(lev)))
    br <- signif(seq(ra[1] -diff(ra)*0.001, ra[2]+diff(ra)*0.001, length.out=length(lev) +1), digits=ndig)
    if(max(br) < ra[2]) br[length(br)] <- signif(br[length(br)] +(br[2] -br[1])/12, digits=ndig) 
    chBr <- duplicated(br,fromLast=FALSE)
    if(any(chBr)) {
      br <- signif(seq(ra[1] -diff(ra)*0.001, ra[2], length.out=length(lev) +1), digits=ndig +1)
      chBr <- duplicated(br,fromLast=FALSE)
      if(any(chBr)) {
        whRep <- !chBr
        br[which(chBr)] <- br[which(chBr)] +ra[1]/1e6 }}  
    intGrp <- cut(x, br)
  } else intGrp <- rep(1, length(x))
  legTx <- matrix(sub("\\]", "", sub("\\(","", unlist(sapply(levels(intGrp), strsplit, ",")))), ncol=2, byrow=TRUE)
  lowVa <- signif(ra[1], 1)
  if(lowVa <= ra[1]) legTx[1,1] <- lowVa
  legTx <- matrix(signif(as.numeric(legTx), digits=round(0.5+log(length(lev)))), ncol=2, dimnames=list(lev,c("from","to")))
  if(NAuse) if (any(is.na(x))) {
    levels(intGrp) <- c(levels(intGrp), "NA")
    intGrp[which(is.na(intGrp))] <- "NA"
    legTx <- rbind(legTx, c("NA", "NA"))
  }
  list(grouped=intGrp, legTxt=legTx)
}
  
