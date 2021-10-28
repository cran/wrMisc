#' make MA-List object 
#'
#' \code{makeMAList} extracts sets of data-pairs (like R & G series) and makes MA objects as \code{MA-List object} (eg for ratio oriented analysis).
#' The grouping of columns as sets of replicate-measurements is done according to argumnet \code{'MAfac'}.
#' The output is fully compatible to functions of package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} (Bioconductor). 
#'
#' @details This function requires Bioconductor package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} being installed.
#'
#' @param mat main input matrix
#' @param MAfac (factor) factor orgnaizing columns of 'mat' (if \code{useF} contains the default 'R' and 'G', they should also be part of \code{MAfac})
#' @param useF (character) two specific factor-leves of \code{MAfac} that will be used/extracted
#' @param isLog (logical) tell if data is already log2 (will be considered when computing M and A values)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return limma-type "MAList" containing M and A values
#' @seealso \code{\link{test2factLimma}}, for creating RG-lists within limma: \code{MA.RG} in \code{\link[limma]{normalizeWithinArrays}}
#' @examples
#' set.seed(2017); t4 <- matrix(round(runif(40,1,9),2), ncol=4,
#'   dimnames=list(letters[c(1:5,3:4,6:4)], c("AA1","BB1","AA2","BB2")))
#' makeMAList(t4, gl(2,2,labels=c("R","G")))
#' @export
makeMAList <- function(mat, MAfac, useF=c("R","G"), isLog=TRUE, silent=FALSE, callFrom=NULL){
  ## extract sets of data-pairs (like R & G series) and MA objects as MA-List object (eg for ratio oriented analysis) according to 'MAfac'
  ## 'MAfac' .. factor
  ## require(limma)
  fxNa <- .composeCallName(callFrom, newNa="makeMAList")  
  chPa <- requireNamespace("limma", quietly=TRUE)
  if(!chPa) {return(NULL); warning(fxNa,"Package 'limma' not found ! Please install from Bioconductor")
  } else {
    if(!all(useF %in% MAfac & length(useF) ==2 & length(MAfac) >1)) stop(" Argument 'useF' should describe 2 elements of 'MAfac'")
    if(!isLog) {
      if(any(mat <0) & !silent) message(fxNa,"Negative values will create NAs at log2-transformation !") }
    out <- try(limma::MA.RG(if(isLog) list(R=2^mat[,which(MAfac==useF[1])], G=2^mat[,which(MAfac==useF[2])]) else {
      list(R=mat[,which(MAfac==useF[1])], G=mat[,which(MAfac==useF[2])])}, bc.method="subtract", offset=0), silent=TRUE)
    if("try-error" %in% class(out)) {warning("UNABLE to run limma::MA.RG() '!"); out <- NULL}  
    out } }

#' @export
.allRatios <- function(dat,ty="log2",colNaSep="_") {
  ## calculate all (log2-)ratios between (entire) indiv columns of 'dat' (matrix or data.frame)
  ## return matrix with ratios (betw columns of dat)
  out <- matrix(nrow=nrow(dat), ncol=choose(ncol(dat),2))
  pwCoor <- upperMaCoord(ncol(dat))
  out <- apply(pwCoor, 1, function(x) dat[,x[1]]/dat[,x[2]])
  colnames(out) <- apply(pwCoor,1,function(x) paste(colnames(dat)[x[1]],colnames(dat)[x[2]], sep=colNaSep))
  if(identical(ty,"log2")) out <- log2(out)
  if(identical(ty,"log10")) out <- log10(out)
  if(identical(ty,"log")) out <- log(out)
  out }

#' @export
.allRatioMatr1to2 <- function(x, y, asLog2=TRUE, sumMeth="mean", callFrom=NULL){
  ## calculate ratio(s) for each column of matrix 'x' versus all/each column(s) of matrix 'y' (reference)
  ## return matrix in dimension of 'x' (so far summarize all ratios from mult division from mult ref cols as mean or median )
  ## 'log2' .. output as log2 format (before summarizing different ratios obtained for given column of 'x')
  ## 'sumMeth' .. for method for summarizing the ratios from diff cols of 'y' (default mean, otherwise median)
  ## variant of .allRatios() but including external reference ('y')
  fxNa <- .composeCallName(callFrom,newNa=".allRatioMatr1to2")
  xDimNa <- dimnames(x)
  yDimNa <- dimnames(y)
  if(length(xDimNa) <1) x <- matrix(x, ncol=1, dimnames=list(rownames(x),"x1"))
  if(length(yDimNa) <1) y <- matrix(y, ncol=1, dimnames=list(rownames(y),"y1"))
  if(nrow(x) != nrow(y)) stop(fxNa," number of rows in 'x' and 'y' must be equal !")
  ## main
  out <- apply(x, 2, function(da,re) matrix(rep(da,ncol(re)), ncol=ncol(re))/re, re=y)    # is this really ok ?
  if(!is.list(out)) out <- list(out)
  if(asLog2) out <- lapply(out,log2)
  out <- if(identical(sumMeth,"mean")) lapply(out,rowMeans) else lapply(out,function(x) apply(x, 1, stats::median,na.rm=TRUE))     # summarize
  nLen <- sapply(out,length)
  if(length(unique(nLen)) >1 | any(nLen <1)) message(fxNa," strange format of results, length of lists: ",
    paste(nLen,collapse=" "))
  out <- out[which(nLen >0)]
  out <- if(length(out) >1) matrix(unlist(out), nrow=nrow(x), dimnames=xDimNa) else out[[1]]
  out }

#' @export
.getAmean <- function(dat,grp) {
  ## get A value (ie group mean) for each group of replicates (eg for MA-plot)
  ## NOTE : this fx is redundant; does about the same as .rowGrpMeans() !
  if(length(levels(grp)) <2) stop(" problem: 'grp' as factor should have at least 2 levels")
  if(length(grp) != ncol(dat)) stop(" problem: length of 'grp' should match numbe of columns in 'dat'")
  if(!is.factor(grp)) grp <- as.factor(grp)
  out <- matrix(nrow=nrow(dat), ncol=length(levels(grp)), dimnames=list(rownames(dat), levels(grp)))
  for(i in 1:length(levels(grp))) out[,i] <- rowMeans(dat[,which(grp==levels(grp)[i])],na.rm=TRUE)
  out }

#' @export
.getAmean2 <- function(dat,comp) as.matrix(apply(comp,1,function(co,x) rowMeans(x[,co],na.rm=TRUE),dat))   # transform dat to A-values for MA-plot (comp is matrix telling which groups to use/compare, assuming that dat are already group-means)

#' @export
.getMvalue2 <- function(dat,comp) as.matrix(apply(comp,1,function(co,x) diff(t(x[,co])),dat))      # transform dat to M-values  ; comp : matrix (2 columns) indicating which columns should be compared
  
