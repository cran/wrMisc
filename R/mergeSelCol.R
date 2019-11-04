#' Merge selected columns out of 2 matrix or data.frames
#'
#' \code{mergeSelCol} merges selected columns out of 2 matrix or data.frames.
#' 'selCols' will be used to define columns to be used; optionally may be different for 'dat2' : define in 'supCols2'. 
#' Output-cols will get additions specified in newSuff (default '.x' and '.y')
#'
#' @param dat1 matrix or data.frame for fusing
#' @param dat2 matrix or data.frame for fusing
#' @param selCols will be used to define columns to be used; optionally may be different for 'dat2' : define in 'supCols2'
#' @param supCols2 if additional column-names should be extracted form dat2
#' @param byC (character) 'by' value used in \code{\link[base]{merge}}
#' @param useAll (logical) use all lines (will produce NAs when given identifyer not found un 2nd group of data)
#' @param setRownames (logical) if TRUE, will use values of col used as 'by' as rownames instead of showing as add'l col in output
#' @param newSuff (character) prefix (argument 'suffixes' in \code{merge})
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return data.frame
#' @seealso \code{\link[base]{merge}}, merge 3 data.frames using \code{\link{mergeSelCol3}}
#' @examples
#' mat1 <- matrix(c(1:7,letters[1:7],11:17),ncol=3,dimnames=list(LETTERS[1:7],c("x1","x2","x3")))
#' mat2 <- matrix(c(1:6,c("b","a","e","f","g","k"),31:36),
#'   ncol=3,dimnames=list(LETTERS[11:16],c("y1","x2","x3")))
#' mergeSelCol(mat1,mat2,selC=c("x2","x3"))
#' @export
mergeSelCol <- function(dat1,dat2,selCols,supCols2=NULL,byC=NULL,useAll=FALSE,setRownames=TRUE,newSuff=c(".x",".y"),callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="mergeSelCol")
  msg <- " should be matrix or data.frame (with >0 lines)"
  ms2 <- c(" can't find "," columns specfied in 'selCols' in ")
  if(length(dim(dat1)) !=2) stop(" argument dat1",msg)
  if(length(dim(dat2)) !=2) stop(" argument dat2",msg)
  if(is.null(byC)) {byC <- naOmit(colnames(dat1)[match(colnames(dat2),colnames(dat1))])[1]
    message(fxNa,"automatically selecting '",byC,"' as 'by'")}
  if(length(byC) >1) { byC <- byC[1]; message(fxNa,"reducing 'by' to length 1")}
  if(!(byC %in% colnames(dat1) & byC %in% colnames(dat1))) stop(ms2[1],byC," in both 'dat1' & 'dat2'")
  if(length(byC) <1 | is.na(byC)) stop(" problem with 'by' : no valid column-names")
  if(is.null(supCols2)) supCols2 <- selCols
  if(sum(selCols %in% colnames(dat1),na.rm=TRUE) < length(selCols)) stop(ms2[1],length(selCols),ms2[2],"dat1 !")
  if(sum(supCols2 %in% colnames(dat2),na.rm=TRUE) < length(supCols2)) stop(ms2[1],length(supCols2),ms2[2],"dat2 !")
  sel3 <- which(colnames(dat1) %in% unique(c(byC,selCols)))
  sel4 <- which(colnames(dat2) %in% unique(c(byC,supCols2)))
  if(any(length(sel3) <2, length(sel4) <2)) { out <- dat1; message(fxNa,"nothing to do")
  } else {
    out <- merge(as.data.frame(dat1[,sel3]),as.data.frame(dat2[,sel4]), by=byC,all=useAll, suffixes=newSuff)}
  if(setRownames) {rownames(out) <- out[,byC]; out <- out[,-1*which(colnames(out) ==byC)]}
  out }

#' mergeSelCol3
#'
#' successive merge of selected columns out of 3 matrix or data.frames.
#' 'selCols' will be used to define columns to be used; optionally may be different for 'dat2' : define in 'supCols2'. 
#' Output-cols will get additions specified in newSuff (default '.x' and '.y')
#'
#' @param dat1 matrix or data.frame for fusing
#' @param dat2 matrix or data.frame for fusing
#' @param dat3 matrix or data.frame for fusing
#' @param selCols will be used to define columns to be used; optionally may be different for 'dat2' : define in 'supCols2'
#' @param supCols2 if additional column-names should be extracted form dat2
#' @param supCols3 if additional column-names should be extracted form dat3
#' @param byC (character) 'by' value used in \code{\link[base]{merge}}
#' @param useAll (logical) use all lines (will produce NAs when given identifyer not found un 2nd group of data)
#' @param setRownames if TRUE, will use values of col used as 'by' as rownames instead of showing as add'l col in output
#' @param newSuff (character) prefix (argument 'suffixes' in \code{merge})
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return data.frame
#' @seealso \code{\link[base]{merge}}, \code{\link{mergeSelCol}}
#' @examples
#' mat1 <- matrix(c(1:7,letters[1:7],11:17),ncol=3,dimnames=list(LETTERS[1:7],c("x1","x2","x3")))
#' mat2 <- matrix(c(1:6,c("b","a","e","f","g","k"),31:36),ncol=3,
#'   dimnames=list(LETTERS[11:16],c("y1","x2","x3")))
#' mat3 <- matrix(c(1:6,c("c","a","e","b","g","k"),51:56),ncol=3,
#'   dimnames=list(LETTERS[11:16],c("z1","x2","x3")))
#' mergeSelCol3(mat1,mat2,mat3,selC=c("x2","x3"))
#' @export
mergeSelCol3 <- function(dat1,dat2,dat3,selCols,supCols2=NULL,supCols3=NULL,byC=NULL,useAll=FALSE,setRownames=TRUE,newSuff=c(".x",".y",".z"),callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="mergeSelCol3")
  msg <- c(" argument dat"," should be matrix or data.frame (with >0 lines)")
  ms2 <- c(" can't find "," columns specfied in 'selCols' in dat")
  if(length(dim(dat1)) !=2) stop(msg[1],"1",msg[2])
  if(length(dim(dat2)) !=2) stop(msg[1],"2",msg[2])
  if(length(dim(dat3)) !=2) stop(msg[1],"3",msg[2])
  allCo <- list(colnames(dat1),colnames(dat2),colnames(dat3))
  if(is.null(byC)) {
    byC <- allCo[[1]][which(allCo[[1]] %in% allCo[[2]] & allCo[[1]] %in% allCo[[3]])[1]]
    message(fxNa,"automatically selecting '",byC,"' as 'by'")}
  if(length(byC) >1) { byC <- byC[1]; message(fxNa,"reducing 'by' to length 1")}
  if(!(byC %in% colnames(dat1) & byC %in% colnames(dat1))) stop(ms2[1],byC," in both 'dat1' & 'dat2'")
  if(length(byC) <1 | is.na(byC)) stop(" problem with 'by' : no valid column-names")
  if(is.null(supCols2)) supCols2 <- selCols
  if(is.null(supCols3)) supCols3 <- selCols
  if(sum(selCols %in% allCo[[1]],na.rm=TRUE) < length(selCols)) stop(ms2[1],length(selCols),ms2[2],"1 !")
  if(sum(supCols2 %in% allCo[[2]],na.rm=TRUE) < length(supCols2)) stop(ms2[1],length(supCols2),ms2[2],"2 !")
  if(sum(supCols3 %in% allCo[[3]],na.rm=TRUE) < length(supCols3)) stop(ms2[1],length(supCols3),ms2[2],"3 !")
  sel0 <- list(which(allCo[[1]] %in% unique(c(byC,selCols))),which(allCo[[2]] %in% unique(c(byC,supCols2))),
    which(allCo[[3]] %in% unique(c(byC,supCols3))))
  if(any(sapply(sel0,length) <2) ) { out <- dat1; message(fxNa,"nothing to do")
  } else {
    out <- merge(as.data.frame(dat1[,sel0[[1]]]),as.data.frame(dat2[,sel0[[2]]]), by=byC,all=useAll, suffixes=newSuff)
    out <- merge(out,as.data.frame(dat3[,sel0[[3]]]), by=byC,all=useAll)
    co1 <- (1:ncol(out))[-1*unlist(lapply(paste(newSuff,"$",sep=""),grep,colnames(out)))][-1]
    if(length(newSuff) >2) colnames(out)[co1] <- paste(colnames(out)[co1],newSuff[3],sep="")
    }
  if(setRownames) {rownames(out) <- out[,byC]; out <- out[,-1*which(colnames(out) ==byC)]}
  out }
   
