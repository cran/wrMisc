#' Filter lines of matrix for max number of NAs 
#'
#' \code{presenceFilt} produces logical matrix to be used as filter for lines of 'dat' for sufficient presence of non-\code{NA} values (ie limit number of NAs per line). 
#' Filter abundance/expression data for min number and/or ratio of non-\code{NA} values in at east 1 of multiple groups.
#' This type of procedure is common in proteomics and tanscriptomics, where a \code{NA} can many times be assocoaued with quantitation below detetction limit.
#'
#' @param dat matrix or data.frame (abundance or expression-values which may contain some \code{NA}s).
#' @param grp factor of min 2 levels describing which column of 'dat' belongs to which group (levels 1 & 2 will be used)
#' @param maxGrpMiss (numeric) at least 1 group has not more than this number of NAs (otherwise marke line as bad)
#' @param ratMaxNA (numeric) at least 1 group reaches this content of non-\code{NA} values
#' @param minVal (default NULL or numeric), any value below will be treated like \code{NA}
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message produced
#' @return logical matrix (with separate col for each pairwise combination of 'grp' levels) indicating if line of 'dat' acceptable based on \code{NA}s (and values minVal)
#' @examples
#' mat <- matrix(rep(8,150), ncol=15, dimnames=list(NULL,
#'   paste0(rep(LETTERS[4:2],each=6),1:6)[c(1:5,7:16)]))
#' mat[lower.tri(mat)] <- NA
#' mat[,15] <- NA
#' mat[c(2:3,9),14:15] <- NA
#' mat[c(1,10),13:15] <- NA
#' mat
#' wrMisc::presenceFilt(mat ,rep(LETTERS[4:2], c(5,6,4)))
#' presenceFilt(mat, rep(1:2,c(9,6)))
#' 
#' # one more example 
#' dat1 <- matrix(1:56, ncol=7)
#' dat1[c(2,3,4,5,6,10,12,18,19,20,22,23,26,27,28,30,31,34,38,39,50,54)] <- NA
#' dat1; presenceFilt(dat1,gr=gl(3,3)[-(3:4)], maxGr=0)
#' presenceFilt(dat1, gr=gl(2,4)[-1], maxGr=1, ratM=0.1)
#' presenceFilt(dat1, gr=gl(2,4)[-1], maxGr=2, rat=0.5)
#' @export
presenceFilt <- function(dat,grp,maxGrpMiss=1,ratMaxNA=0.8,minVal=NULL,silent=FALSE,callFrom=NULL){           
  fxNa <- .composeCallName(callFrom, newNa="presenceFilt")
  msg <- "expecting (2dim) numeric matrix or data.frame with >1 columns and >1 rows"
  if(length(dim(dat)) !=2) stop(msg)
  if(ncol(dat) <2) stop(msg)
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  nGrp <- table(grp)
  nGrp <- nGrp[order(unique(grp))]
  if(length(nGrp) <2) stop(" too few levels (",length(nGrp),") in 'grp' !")
  if(length(maxGrpMiss) !=length(nGrp)) {
    if(length(maxGrpMiss)==1) maxGrpMiss <- rep(maxGrpMiss, length(nGrp)) else {
      if(length(maxGrpMiss) < nGrp) {message(fxNa," length of 'maxGrpMiss' too short")} else maxGrpMiss <- maxGrpMiss[1:length(nGrp)]}}
  ## main
  if(length(minVal)==1 & is.numeric(minVal)) dat[dat <minVal] <- NA
  nNa <- matrix(unlist(by(t(dat), grp, function(x) colSums(is.na(x)))), nrow=nrow(dat))[,order(unique(grp))]  # no of NAs per group & line
  ch <- maxGrpMiss > round(nGrp*ratMaxNA)
  if(any(ch)) {maxGrpMiss[which(ch)] <- round(nGrp*ratMaxNA)[ch]
    if(!silent) message(fxNa," correcting 'maxGrpMiss' for group(s) ",pasteC(names(nGrp)[ch]), "  due to ratMaxNA=",ratMaxNA)}
  sufVa <- nNa <= matrix(rep(maxGrpMiss, each=nrow(dat)), nrow=nrow(dat))
  combin <- triCoord(length(nGrp))
  out <- apply(combin,1,function(x) sufVa[,x[1]] | sufVa[,x[2]])
  outNa <- matrix(names(nGrp)[as.numeric(combin)],ncol=2)  
  colnames(out) <- paste(outNa[,1],outNa[,2],sep="-")
  out }

#' @export
.offCenter <- function(di,nMost=1){
  ## return position of 'di' (numeric vector) which is most excentric (distant to 0), starts with NAs as most excentric
  ## used for identifying/removing (potential) outliers
  ## note : this fx doesn't consider reference distrubutions, even with "perfect data" 'nMost' points will ba tagged !
  if(any(c(length(di),length(nMost)) <1))  stop(" 'di' or 'nMost' seem to be missing")
  if(nMost <1) stop(" 'nMost' should be single numeric integer >0")
  out <- if(sum(is.na(di) >0)) naOmit(which(is.na(di))[1:nMost]) else NULL
  if(length(out) < nMost) {
    nMost <- nMost - length(out)
    out <- c(out, which(rank(-1*di,ties.method="random") <= nMost))
  }
  out }
   
