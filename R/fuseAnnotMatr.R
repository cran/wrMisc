#' Fuse annotation matrix to initial matrix
#'
#' In a number of instances experimental measurements and additional information (annotation) are provided by separate objects (matrixes) as they may not be generated the same time.
#' The aim of this function is provide help when matching approprate lines for 2 sets of data (experimental measures in \code{iniTab} and annotation from \code{annotTab}) for fusing.  
#' \code{fuseAnnotMatr} adds suppelmental columns/annotation to an initial matrix \code{iniTab} : using column 'refIniT' as key (in \code{iniTab}) to compare with key 'refAnnotT' (from 'annotTab'). 
#' The columns to be added from \code{annotTab} must be chosen explicitely. 
#' Note: if  non-unique IDs in iniTab : runs slow (but save) due to use of loop for each unique ID.
#' @param iniTab (matrix), that may have lines with multiple (=repeated) key entries 
#' @param annotTab (matrix) containing reference annotation
#' @param refIniT (character) type of reference (eg 'Uniprot')
#' @param refAnnotT (character) column name to use for reference-annotation
#' @param addCol (character) column-namess of 'annotTab' to use/extract (if no matches found, use all)
#' @param debug (logical) for bug-tracking: more/enhanced messages 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return combined matrix (elements not found in 'annotTab' are displayed as NA)
#' @seealso \code{\link[base]{merge}}
#' @examples
#' tab0 <- matrix(rep(letters[1:25],8),ncol=10)
#' tab1 <- cbind(Uniprot=paste(tab0[,1],tab0[,2]),col1=paste(tab0[,3],
#'   tab0[,4],tab0[,5]," ",tab0[,7],tab0[,6]))
#' tab2 <- cbind(combName=paste(tab0[,1],tab0[,2]),col2=paste(tab0[,8],tab0[,9],tab0[,10]))
#' fuseAnnotMatr(tab1,tab2[c(20:11,2:5),],refIni="Uniprot",refAnnotT="combName",addCol="col2")
#' fuseAnnotMatr(tab2[c(20:11,2:5),],tab1,refAnnotT="Uniprot",refIni="combName",addCol="col1")
#' @export
fuseAnnotMatr <- function(iniTab,annotTab,refIniT="Uniprot",refAnnotT="combName",
  addCol=c("ensembl_gene_id","description","geneName","combName"),debug=TRUE,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="fuseAnnotMatr")
  argN <- c(deparse(substitute(iniTab)),deparse(substitute(annotTab)))
  concSym <- "; "                    # exeption to IDs (IDs from 'iniTab' containing these characters won't be considered)
  chMatr <- list(dim(iniTab),dim(annotTab))
  msg <- c(paste(argN,collapse=" &"),"must be matrix or data.frame","are expected to have >1 rows and >1 columns")
  if(any(sapply(chMatr,length) <1)) stop(msg[1:2])
  if(any(unlist(chMatr) <2) & !silent) message(fxNa,msg[c(1,3)])
  if(length(unique(annotTab[,refAnnotT])) < nrow(annotTab)) annotTab <- combineRedBasedOnCol(annotTab,refAnnotT,callFrom=fxNa)
  chKey <- c(refIniT %in% colnames(iniTab), refAnnotT %in% colnames(annotTab))
  if(any(!chKey)) stop(paste(c(refIniT,refAnnotT)[which(!chKey)],collapse=" & ")," not found in initial matrix(es) !!")
  useCol <- which(colnames(annotTab) %in% addCol)
  if(length(useCol) <1 | identical(refAnnotT,addCol)) {
    if(!silent) message(fxNa," NONE of 'addCol' found in ",argN[2],"!!  use/return all cols")
    useCol <- 1:ncol(annotTab)
  } else if(length(addCol) < length(useCol)) message(fxNa,length(addCol)-length(useCol)," columns of 'addCol' not found in ",argN[2])
  chKey <- sum(annotTab[,refAnnotT] %in% iniTab[,refIniT],na.rm=TRUE)
  if(chKey <1 & !silent) message(fxNa," NO matches of keys found between ",msg[1]," !!")
  uniqGe <- unique(iniTab[,refIniT])
  chConcSym <- grep(concSym,uniqGe)
  if(length(chConcSym) >0) uniqGe <- uniqGe[-1*grep(concSym,uniqGe)]                             #
  if(length(uniqGe) == nrow(iniTab)) {
    annotTa2 <- as.data.frame(annotTab[,unique(c(which(colnames(annotTab)==refAnnotT),useCol))],stringsAsFactors=FALSE)
    out <- merge(iniTab,annotTa2,all.x=TRUE,by.x=refIniT,by.y=refAnnotT)
  } else {
    if(length(uniqGe) >2 & !silent) message(fxNa," non-unique IDs found :  be patient ! (running loop across all IDs)")
    supAnn <- matrix(NA,nrow(iniTab),length(useCol),dimnames=list(iniTab[,"Uniprot"],colnames(annotTab)[useCol]))  # (empty) for cbind
    for(i in uniqGe) { useLi <- which(rownames(supAnn)==i)
      if(length(useLi) >0) { j <- which(annotTab[,refAnnotT]==i)
      if(length(j) >0) supAnn[useLi,] <- matrix(rep(annotTab[j,useCol],each=length(useLi)),nrow=length(useLi)) }}
    out <- cbind(iniTab,supAnn) }
  as.matrix(out) }
     
