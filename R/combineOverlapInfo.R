#' Find and combine points located very close in x/y space
#'
#' Search points in x,y space that are located very close and thus likely to overlap.
#' In case of points close enough, various options for joining names (and shortening longer descriptions) are available. 
#'
#' @param dat (matrix) matrix or data.frame with 2 cols (used ONLY 1st & 2nd column !), used as x & y coordinates
#' @param suplInfo (NULL or character) when points are considered overlapping the text from 'suplInfo' will be reduced to fragment before 'txtSepChar' and combined (with others from overlapping text) using 'combSym', if NULL $combInf will appear with row-numbers
#' @param disThr (numeric) distance-thrshold for considering as similar via searchDataPairs()
#' @param addNsimil (logical) include number of fused points
#' @param txtSepChar (character) for use with .retain1stPart(): where to cut (& keep 1st part) text from 'suplInfo' to return in out$CombInf; only 1st element used !
#' @param combSym (character) concatenation symbol (character, length=1) for points considered overlaying, see also 'suplInfo'
#' @param maxOverl (integer) if NULL no limit or max limit of group/clu size (avoid condensing too many neighbour points to single cloud)
#' @param debug (logical) additional messages for debugging
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return matrix with fused (condensed) information for cluster of overapping points
#' @examples
#' set.seed(2013)
#' datT2 <- matrix(round(rnorm(200)+3,1),ncol=2,dimnames=list(paste("li",1:100,sep=""),
#'   letters[23:24]))
#' # (mimick) some short and longer names for each line
#' inf2 <- cbind(sh=paste(rep(letters[1:4],each=26),rep(letters,4),1:(26*4),sep=""),
#'  lo=paste(rep(LETTERS[1:4],each=26),rep(LETTERS,4),1:(26*4),",",rep(letters[sample.int(26)],4),
#'   rep(letters[sample.int(26)],4),sep=""))[1:100,] 
#' head(datT2,n=10)
#' head(combineOverlapInfo(datT2,disThr=0.03),n=10)
#' head(combineOverlapInfo(datT2,suplI=inf2[,2],disThr=0.03),n=10)
#' @export
combineOverlapInfo <- function(dat,suplInfo=NULL,disThr=0.01,addNsimil=TRUE,txtSepChar=",",combSym="+",maxOverl=50,callFrom=NULL,debug=FALSE,silent=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="combineOverlapInfo")
  if(length(dim(dat)) !=2) stop("expecting matrix or data.frame with 2 cols as 'dat'")
  if(!is.null(suplInfo)){
    suplInfo <- as.character(suplInfo)
    if(length(suplInfo) != nrow(dat)) { message(fxNa," number of rows of 'dat' (",nrow(dat),
      ") should match length of 'suplInfo' (",length(suplInfo),") .. ignoring")
      suplInfo <- NULL }}
  datOverl <- searchDataPairs(dat[,1:2], disThr=disThr, byColumn=FALSE, realDupsOnly=FALSE, callFrom=fxNa)
  txt <- " overlap-pairs: making condensed groups via fusePairs() will take "
  if(!silent & nrow(datOverl) >1800) message(fxNa,nrow(datOverl),txt, if(nrow(datOverl) >5000) "VERY (!) " else "","MUCH TIME !")
  modDatLi0 <- sort(unique(naOmit( as.character(as.matrix(datOverl[,1:2])))))      # get all line-numbers with (potentially) close points
  modDatLi2 <- unique(convToNum(modDatLi0, remove=NULL, sciIncl=TRUE))
  modDatLi <- if(length(modDatLi2)==length(modDatLi0)) modDatLi2 else match(modDatLi0,rownames(dat))   # convert to numeric if possible, otherwise as rownames
   if(length(modDatLi) <2) {
    if(!silent) message(fxNa,"all entries seem non-overlapping, no need to combine overlapping ones")
    return(cbind(dat, clu=1:nrow(dat), combInf=if(length(suplInfo) <1) 1:nrow(dat) else suplInfo, clu=1:nrow(dat), isComb=FALSE))
  } else {             # aggregate points considered potentially overlapping to 'clusters'
    datOverl[,1] <- as.character(datOverl[,1])             
    datOverl[,2] <- as.character(datOverl[,2])
    if(debug) {message(fxNa,"combOI\n")}
    tmp <- fusePairs(datOverl, refDatNames=rownames(dat), inclRepLst=TRUE, maxFuse=maxOverl, callFrom=fxNa, debug=FALSE)
    if(is.na(txtSepChar)) txtSepChar <- "_" else if(txtSepChar %in% c("+",".")) txtSepChar <- paste("\\",txtSepChar,sep="")
    if(is.null(suplInfo)) suplInfo <- 1:nrow(dat)
    redInfo <- suplInfo
    if(debug) {message(fxNa,"xxComb3\n")}
    if(!all(is.na(suplInfo))) for(i in unique(tmp$clu)) {                # loop along cluster-names
      j <- match(names(tmp$clu)[which(tmp$clu==i)],rownames(dat))        # ie, at these locations
      if(length(j) >1 & length(txtSepChar)==1) redInfo[j] <- paste(.retain1stPart(
        suplInfo[j],sep=txtSepChar,offSet=1), collapse=sub("\\\\","",combSym)) }     
    out <- data.frame(dat, combInf=NA, clu=NA, isComb=FALSE, stringsAsFactors=FALSE)    
    if(length(suplInfo) ==nrow(dat)) { out$combInf <- redInfo }  
    out[,"clu"] <- tmp$clu[match(rownames(dat),names(tmp$clu))]
    if(any(is.na(out[,"clu"]))) out[which(is.na(out[,"clu"])),"clu"] <- max(out[,"clu"],na.rm=TRUE)+(1:sum(is.na(out[,"clu"])))    # in case of NAs, attribue new no
    cluTab <- table(out[,"clu"])
    out[which(!is.na(match(out[,"clu"],names(cluTab)[which(cluTab >1)]))),"isComb"] <- TRUE
    out }}
   
