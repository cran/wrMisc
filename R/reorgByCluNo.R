#' Reorganize matrix according to clustering-output
#'
#' Reorganize input matrix as sorted by cluster numbers (and geometric mean) according to vector with cluster names, and index for sorting per cluster and per geometric mean.
#' In case \code{mat} is an array, the 3rd dimension will be considered as 'column' with arguments \code{useColumn} ( and \code{cluNo}, if it designs a 'column' of mat).
#' 
#' 
#' @param mat (matrix or data.frame) main input
#' @param cluNo (positive integer, length to match nrow(dat) initial cluster numbers for each line of 'mat' (obtained by separate clustering or other segmentation) or may desinn column of \code{mat} to use as cluster-numbers
#' @param useColumn (character or integer) the columns to use from mat as main data (default will use all, exept \code{cluCol} and/or \code{meanCol} if they design columns))
#' @param meanCol (character or integer) alternative summarizing data for intra-cluster sorting (instead of geometric mean)
#' @param retList (logical)  
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return list or array (as 2- or 3 dim) with possible number of occurances for each of the 3 elements in nMax. Read results vertical : out[[1]] or out[,,1] .. (multiplicative) table for 1st element of nMax; out[,,2] .. for 2nd
#' @seealso  \code{\link[utils]{combn}} 
#' @examples
#' dat1 <- matrix(round(runif(24),2), ncol=3, dimnames=list(NULL,letters[1:3]))
#' clu <- stats::kmeans(dat1, 5)$cluster
#' reorgByCluNo(dat1, clu) 
#' 
#' dat2 <- cbind(dat1, clu=clu) 
#' reorgByCluNo(dat2, "clu") 
#' @export
reorgByCluNo <- function(mat, cluNo, useColumn=NULL, meanCol=NULL, retList=FALSE, silent=FALSE, callFrom=NULL) {
  ## reorganize input matrix as sorted by cluster numbers (and geometric mean) according to vector with cluster names, and index for sorting per cluster and per geometric mean
  fxNa <- wrMisc::.composeCallName(callFrom, newNa="reorgByCluNo")
  iniCla <- class(mat)
  chDim <- dim(mat)
  dataOK <- FALSE
  if(length(chDim) >1) if(all(chDim >1)) dataOK <- TRUE
  ## extract cluNo
  if(dataOK & length(cluNo)==1) {
    cluN <- if(is.character(cluNo)) which(dimnames(mat)[[length(chDim)]]==cluNo) else cluNo
    if(length(chDim) ==3) { cluNo <- mat[,1,cluN]; mat <- mat[,,-cluN] 
    } else { cluNo <- mat[,cluN]; mat <- mat[,-cluN] } }   
  if(dataOK) if(length(cluNo) != nrow(mat)) dataOK <- FALSE 
  ## extract meanCol
  if(dataOK & length(meanCol)==1) {
    meanC <- if(is.character(meanCol)) which(dimnames(mat)[[length(chDim)]]==meanCol) else meanCol
    if(length(chDim) ==3) { meanCol <- mat[,1,meanC]; mat <- mat[,,-meanC] 
    } else { meanCol <- mat[,meanC]; mat <- mat[,-meanC] } }   
  if(dataOK & length(meanCol) >0) {if(length(meanCol) != nrow(mat)) dataOK <- FALSE }
  ## main
  if(dataOK){    
    if(length(useColumn) <1) useColumn <- 1:ncol(mat)
    mat1 <- if(length(chDim) ==3) mat[,,useColumn] else mat[,useColumn]
    if(length(dim(mat1)) <1) mat1 <- matrix(mat1,ncol=1,dimnames=list(rownames(mat1),colnames(mat)[useColumn]))
    nClu <- length(unique(wrMisc::naOmit(cluNo)))
    ## construct geometric mean for sorting (if not provided externally)
    geoMe <- if(length(meanCol)==nrow(mat)) meanCol else apply(mat1,1, function(x) prod(x,na.rm=TRUE)^(1/sum(!is.na(x))))
    if(length(dim(geoMe)) >1) geoM <- rowMeans(geoMe, na.rm=TRUE)
    ## assemble main data
    mat1 <- cbind(mat1, index=1:nrow(mat), geoMean=geoMe)    
    ## 1: split in list, determine clu median, overall score & sort clusters  
    cluL <- by(mat1, cluNo, as.matrix)
    clTab <- table(cluNo)               
    if(length(clTab) < max(cluNo) & !silent) message(fxNa," Note: Some cluster-names seem to be absent (no-consecutive numbers for names) !")
    ## need to correct when single occurance
    if(any(clTab ==1)) for(i in which(clTab ==1)) cluL[[i]] <- matrix(cluL[[i]], nrow=1, dimnames=list(rownames(mat1)[which(cluNo==i)], colnames(mat1)))
    ## sort intra
    cluL <- lapply(cluL, function(x) if(nrow(x) >1) x[order(x[,ncol(x)], decreasing=TRUE),] else x)
    ## sort inter
    cluL <- cluL[order(sapply(cluL, function(x) stats::median(x[,ncol(x)],na.rm=TRUE)),  decreasing=TRUE)]        
    names(cluL) <- 1:length(cluL)
    nByClu  <- sapply(cluL,nrow)
    if(retList) { out <- cluL
      for(i in 1:nClu) out[[i]] <- cbind(out[[i]], cluNo=1)
       if("data.frame" %in% iniCla) out <- lapply(out,wrMisc::convMatr2df, addIniNa=FALSE, silent=silent,callFrom=fxNa) 
    } else {
      out <- cbind(wrMisc::lrbind(cluL), cluNo=rep(1:nClu,nByClu)) }   # in order of input
  } else {out <- NULL; if(!silent) message(fxNa," invalid input, returning NULL")}
  out }
  
