#' Filter Lines Of Matrix For Max Number Of NAs 
#'
#' This function produces a logical matrix to be used as filter for lines of 'dat' for sufficient presence of non-\code{NA} values (ie limit number of NAs per line). 
#' Filter abundance/expression data for min number and/or ratio of non-\code{NA} values in at east 1 of multiple groups.
#' This type of procedure is common in proteomics and tanscriptomics, where a \code{NA} can many times be assocoaued with quantitation below detetction limit.
#'
#' @param dat matrix or data.frame (abundance or expression-values which may contain some \code{NA}s).
#' @param grp factor of min 2 levels describing which column of 'dat' belongs to which group (levels 1 & 2 will be used)
#' @param useComparison (character or matrix) optional argument allowing to specify which pairwise comparions sould be performed, default \code{useComparison=NULL} will run all pairwise comparisons;
#'   may be character combining two group-names (from argument \code{grp}) separated by a '-' (eg 'A-B') or matrix where the rownames design the elements to be compared as pairwise;
#'   Note : the names of the groups may not contain any '-' to avoid confucing them with pairwise separators !
#' @param maxGrpMiss (numeric) at least 1 group has not more than this number of NAs (otherwise marke line as bad)
#' @param ratMaxNA (numeric) at least 1 group below this content of \code{NA} values
#' @param minVal (default NULL or numeric), any value below will be treated like \code{NA}
#' @param sep (character) in case \code{useComparison} is not given all pairwise comparisons will be done, the separator to be used when combining names of groups can be given using this argument
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a logical matrix (with separate col for each pairwise combination of 'grp' levels) indicating if line of 'dat' acceptable based on \code{NA}s (and values minVal)
#' @seealso  \code{\link{presenceGrpFilt}},   there are also other packages on CRAN and Bioconductor dedicated to filtering
#' @examples
#' mat <- matrix(rep(8,150), ncol=15, dimnames=list(NULL,
#'   paste0(rep(LETTERS[4:2],each=6),1:6)[c(1:5,7:16)]))
#' mat[lower.tri(mat)] <- NA
#' mat[,15] <- NA
#' mat[c(2:3,9),14:15] <- NA
#' mat[c(1,10),13:15] <- NA
#' mat
## default : at least 1 group with max 1 NA and one group below 80% NA
#' presenceFilt(mat, substr(colnames(mat),1,1))
#' # custom 2 groups
#' presenceFilt(mat, rep(1:2,c(9,6)))         # D1- C4, C5 - B4
#' 
#' # one more example 
#' dat1 <- matrix(1:56, ncol=7)
#' dat1[c(2:6,10,12,18,19,20,22,23,26:28,30,31,34,38,39,50,54)] <- NA
#' grp3 <- letters[c(3,3,2,2,1,1,1)]
#' colnames(dat1) <- correctToUnique(grp3, sep="") 
#' dat1
#' ## At least one group wo any NAs
#' presenceFilt(dat1, grp3, maxGr=0)
#' presenceFilt(dat1, gr=gl(2,4)[-1], maxGr=1, ratM=0.1)
#' presenceFilt(dat1, gr=gl(2,4)[-1], maxGr=2, rat=0.5)
#' @export
presenceFilt <- function(dat, grp, useComparison=NULL, maxGrpMiss=1, ratMaxNA=0.8, minVal=NULL, sep=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){           
  fxNa <- .composeCallName(callFrom, newNa="presenceFilt")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  msg <- "Expecting (2dim) numeric matrix or data.frame with >1 columns and >1 rows"
  if(length(dim(dat)) !=2) stop(msg)
  if(ncol(dat) <2) stop(msg)
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  if(!is.factor(grp)) grp <- as.factor(grp)

  ## prepare 
  nGrp <- table(grp)
  if(length(nGrp) <2) stop(" too few levels (",length(nGrp),") in 'grp' !")
  nGrp <- nGrp[order(unique(grp))]
  if(length(maxGrpMiss) !=length(nGrp)) {
    if(length(maxGrpMiss)==1) maxGrpMiss <- rep(maxGrpMiss, length(nGrp)) else {
      if(length(maxGrpMiss) < nGrp) {stop(fxNa,"Length of 'maxGrpMiss' too short")} else maxGrpMiss <- maxGrpMiss[1:length(nGrp)]}
  }
  if(debug) {message(fxNa,"pFi1"); pFi1 <- list(dat=dat,grp=grp,useComparison=useComparison,maxGrpMiss=maxGrpMiss,ratMaxNA=ratMaxNA,minVal=minVal,nGrp=nGrp)}

  ## main
  ## establish by group
  #byGrp <- matrix(TRUE, nrow=nrow(dat), ncol=length(nGrp), dimnames=list(rownames(dat), names(nGrp)))
  ## minVal : set mat to NA if below ...
  if(length(minVal)==1 && is.numeric(minVal)) dat[which(dat <minVal)] <- NA
  ## count NAs
  nNa <- matrix(unlist(by(t(dat), grp, function(x) colSums(is.na(x)))), nrow=nrow(dat))[,order(unique(grp))]  # no of NAs per group & line
  #nNa <- nNa[,order(unique(grp))]
  dimnames(nNa) <- list(rownames(dat), names(nGrp))
  #ch1 <- nNa > maxGrpMiss
  byGrp <- nNa <= maxGrpMiss  # so far those which pass on absol count
  
  #if(any(ch1)) byGrp[which(ch1)] <- FALSE
  if(debug) {message(fxNa,"pFi1b"); pFi1b <- list(dat=dat,grp=grp,useComparison=useComparison,maxGrpMiss=maxGrpMiss,ratMaxNA=ratMaxNA,nGrp=nGrp,byGrp=byGrp,nNa=nNa)}
  ## filter for ratio
  NaRat <- nNa/matrix(rep(nGrp, each=nrow(dat)), nrow=nrow(dat))
  ch1 <- NaRat > ratMaxNA & byGrp
  
  if(any(ch1)) byGrp[which(ch1)] <- FALSE
  if(debug) {message(fxNa,"pFi2"); pFi2 <- list(dat=dat,grp=grp,useComparison=useComparison,maxGrpMiss=maxGrpMiss,ratMaxNA=ratMaxNA,nGrp=nGrp,byGrp=byGrp,nNa=nNa,NaRat=NaRat)}

  ## filter for absol count
  ## combine groups
  if(length(useComparison)==0) {
    if(length(sep)==0) sep <- getPWseparator(grp=grp, silent=silent, debug=debug, callFrom=fxNa)   
    useComparison <- t(utils::combn(1:length(levels(grp)), 2, simplify=TRUE))               # relatoiv to LEVELS !!
    if(ncol(useComparison)==1) useComparison <- t(useComparison)
    dimnames(useComparison) <- list(utils::combn(levels(grp), 2, paste, collapse=sep), c("samp","ref")) 
  } else if(length(dim(useComparison)) <2) {
    if(debug) {message(fxNa,"Trying to convert 'useComparison' to matrix of indexes")}
    useComparison <- try(indexGroupsFromPW(compNames=useComparison, grp=unique(grp), includeGrp=FALSE, silent=silent, debug=debug, callFrom=fxNa))
    if(inherits(useComparison, "try-error")) {useComparison <- NULL; message(fxNa,"Failed to convert 'useComparison")}
  }   
  if(length(dim(useComparison)) !=2 && all(dim(useComparison) >= c(1,2))) {
    if(!silent) message(fxNa,"Invalid 'useComparison', must be matrix or data.fame with 2 columns;  returning NULL")
  } else {
    out <- matrix(TRUE, nrow=nrow(dat), ncol=nrow(useComparison), dimnames=list(rownames(dat), rownames(useComparison)))  
    for(i in 1:nrow(useComparison)) out[,i] <- byGrp[,useComparison[i,1]] | byGrp[,useComparison[i,2]]
    out
  } 
}
  
