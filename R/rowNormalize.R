#' Row Normalize
#'
#' This function was designed for normalizing data that is supposed to be particularly similar, like a collection of technical replicates.
#' Thus, for each row a normalization factor is calculated and the median or mean across all factors will be applied. 
#' A low content of \code{NA} values poses generally no problems. If the content is higher than the threshold set in \code{sparseLim}   
#' an alternative procedure for sparse data will be applied (iteratively trating subsets of \code{nCombin} columns that will be combined in a later step).
#'
#' @details
#' Arguments were kept similar with function \code{normalizeThis};
#' Normally data get normalized by proportional factors. In case of log2-data (very common in omics-data) normalizing by an additive factor is equivalent to a proportional factor.
#'
#' This function has a special mode of operation for sparse data (ie containing a high content of \code{NA} values). 
#' O-values by themselves will be primarily considered as true measurment outcomes and not as missing. 
#' By using the argument \code{minQuant} all values below a given threshold will be set as \code{NA} and this may possibly trigger the sparse mode of normaizing. 
#'
#' Note : Using a small value of \code{nCombin} will give the highest chances of finding sufficient complete data-sets with sparse data.
#' However, this will also increase the computational efforts and time required to produce an output.
#' 
#' When using default proportinal mode a potential division by 0 could occur when the initial normalization factor turns out as 0. 
#' In this case a small value (default the maximum value of \code{dat} / 10; if this value exits in the data it will be multiplied by 0.03) will be added to all data before normalizing. 
#' 
#' @param dat matrix or data.frame of data to get normalized
#' @param method (character) may be "mean","median" (plus "NULL","none"); When NULL or 'none' is chosen the input will be returned as is
#' @param refLines (NULL or numeric) allows to consider only specific lines of 'dat' when determining normalization factors (all data will be normalized)
#' @param refGrp (integer) Only the columns indicated will be used as reference, default all columns (integer or colnames)
#' @param proportMode (logical) decide if normalization should be done by multiplicative or additive factor
#' @param minQuant (numeric) optional filter to set all values below given value as \code{NA}
#' @param sparseLim (integer) decide at which min content of  \code{NA} values the function should go in sparse-mode 
#' @param nCombin (NULL or integer) used only in sparse-mode (ie if content of \code{NA}s higher than content of \code{sparseLim}): Number of groups of smller matrixes with this number of columns to be inspected initailly; 
#'   low values (small groups have higher chances of more common elements)
#' @param omitNonAlignable (logical) allow omitting all columns which can't get aligned due to sparseness
#' @param maxFact (numeric, length=2) max normalization factor
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) This function allows easier tracking of messages produced
#' @return This function returns a matrix of normalized data
#' @seealso \code{\link{exponNormalize}}, \code{\link{adjBy2ptReg}}, \code{\link[vsn]{justvsn}} 
#' @examples
#' ## sparse matrix  normalization
#' set.seed(2); AA <- matrix(rbinom(110,10,0.05), nrow=10)
#' AA[,4:5] <- AA[,4:5] *rep(4:3, each=nrow(AA))
#'
#' (AA1 <- rowNormalize(AA))
#' (AA2 <- rowNormalize(AA, minQuant=1))   # set all 0 as NAs
#' (AA3 <- rowNormalize(AA, refLines=1:6, omitNonAlignable=FALSE, minQuant=1))
#'
#'
#' @export
rowNormalize <- function(dat, method="median", refLines=NULL, refGrp=NULL, proportMode=TRUE, minQuant=NULL, sparseLim=0.4, nCombin=3, omitNonAlignable=FALSE, maxFact=10, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## integrate to normalizeThis ?
  ## method (character) normalization method applied to slection of complete lines from subsets of \code{nCombin} columns, sent to with normalizeThis()
  ##   however, if less than 25 common lines found "average" normalization will be used
  ## proportMode (logical) .. switch between normalizing by multiplicative (proportional) or additive factor
  ## minQuant (numeric) .. intensity threshold; all values below will be considered as missing (ie \code{NA})
  ## sparseLim (numeric) .. the max ratio of content of \code{NA}s in data for regular normalization, if content of \code{NA}s is higher an iterative approch using subsets of content of \code{nCombin} columns will be used
  ## omitNonAlignable (logical) .. decide if columns which can't be 'connected' to the rest (due to missing common measures) will be added without proper normalization or rather omitted
  ## minQuant (numeric) .. intensity threshold; all values below will be considered as missing (ie \code{NA})
  ## maxFact (numeric) .. max normalization factor (values higher will be set to threshold)
  ## nCombin (integer) .. (only if content of \code{NA}s higher than content of \code{sparseLim}) groups of smller matrixes with this number of columns to be inspected initailly; low values (small groups have higher chances of more common elements)
  ##
  ## This function was designed for normalizing rows of sparse matrixes. 'dat' is supposed to represent equivalent (eg replicate measures) of the same series of individuals/elements,
  ##  However, numerous values may be missing ie \code{NA}. Thus, it may occur frequently that a given element has not all measures, ie lines containing some \code{NA} values.
  ## In consequence, chances of finding lines without any \code{NA} are higher when initially fewer columns (argument \code{nCombin}) are normalized first and then combined with other sets of comumns in an iterative way.
  fxNa <- wrMisc::.composeCallName(callFrom, newNa="rowNormalize")
  # proportional mode : need to add small value to avoid divBy0 !
  if(any(length(dim(dat)) !=2, dim(dat) < 2:1)) stop("Invalid argument 'dat'; must be matrix or data.frame with min 2 lines and 1 col")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  sep <- "_"                        # separator used internally with column-numbers
  maxFact=10

  if(is.data.frame(dat)) dat <- as.matrix(dat)
  iniColNa <- colnames(dat)
  colnames(dat) <- 1:ncol(dat)

  if(length(refLines) <1) refLines <- 1:nrow(dat) else if(any(refLines > nrow(dat))) {
    if(!silent) message(fxNa,"Note: 'refLines' designs (some) lines not present in data, ignoring..")
    refLines <- refLines[which(refLines <= nrow(dat))] }
  if(length(refGrp) >0) refGrp <- refGrp[which(refGrp %in% 1:ncol(dat))]
  if(length(refGrp) <1) refGrp <- 1:ncol(dat)
  if(length(minQuant)==1 & is.numeric(minQuant)) if(!is.na(minQuant)) {chMin <- dat < minQuant
    if(any(chMin)) dat[which(chMin)] <- NA }
  if(debug) {cat("srn0 \n")}

  refVal <- if(sum(!is.na(dat[refLines, refGrp])) >10) stats::median(dat[refLines, refGrp], na.rm=TRUE) else mean(dat[refLines, refGrp], na.rm=TRUE)
  if(refVal==0) refVal <- 1
  if(debug) {cat("srn0b \n")}

  ## may be better/faster to random-shuffle column-order of combOfN ? (allow reaching last quicker)
  chNa <- sum(is.na(dat[refLines,])) / length(dat[refLines,])   # ratio of NAs
  if(debug) message(fxNa," Data contain ",round(100*chNa/length(dat),2),"% NA, using partial/iterative approach for sparse data")
  if(chNa > sparseLim) {
    if(ncol(dat) <= 20 ) {
      combOfN <- utils::combn(ncol(dat), nCombin)              # the combinations (all) ..
      colnames(combOfN) <- apply(combOfN, 2, paste0, collapse="_")
    } else {      ## shortcut for larger data-sets, consecutive sligtly overlapping segments will be considered
      overLap <- 2
      nSeg <- ceiling(ncol(dat)/(nCombin -1))
      staSto <- cbind(sta=(0:(nSeg-1))*(nCombin -overLap) +1, sto=(1:nSeg)*(nCombin -overLap) +overLap)
      chStaSto <- staSto[,2] > ncol(dat)
      if(any(chStaSto)) staSto[which(chStaSto),] <- ncol(dat) -c((nCombin-1), 0)
      combOfN <- apply(staSto, 1, function(x) x[1]:x[2])
      if( TRUE) {  ## large data-sets : complete by few additional cross-references
        suplGrpFac <- round(log2(ncol(dat)))           ##  used for extending consecutive splits by additional non-consecutive sets : split into x subgroups for shuffeling tsart-sites
        stepW <- c(floor(nCombin/2), ceiling(nCombin/2))
        tmp <- suplGrpFac*(1:(floor(ncol(dat)/suplGrpFac)))
        combOfN <- cbind(combOfN, sapply(tmp, function(x) c(1:stepW[1], x:(x +stepW[2] -1))), c(1:stepW[1], ncol(dat) -(stepW[2] -1):0) )
        if(suplGrpFac >2) for(i in 2:suplGrpFac)  combOfN <- cbind(combOfN, sapply(tmp, function(x) c(tmp[i] -1 + 1:stepW[1], x:(x +stepW[2] -1))))
        ## now filter away replicated/redundant combinations or combinations with intra-repeats (of specific cols) or combiantions suggesting col-numbers out of bound
        chDu <- duplicated(apply(combOfN, 2, function(x) paste0(sort(x), collapse=sep))) | apply(combOfN, 2, function(x) any(duplicated(x))) | apply(combOfN, 2, max) > ncol(dat)
        if(any(chDu)) combOfN <- if(sum(!chDu) >1) combOfN[,-which(chDu)] else as.matrix(combOfN[,-which(chDu)])}
    }
    comUse <- apply(combOfN, 2, function(x) which(rowSums(is.na(dat[refLines,x])) <1))   # index of valid/complete lines (no NA) per set of nCombin cols
    if(length(names(comUse)) <1) names(comUse) <- apply(combOfN, 2, paste0, collapse=sep)

    comUseLe <- if(is.list(comUse)) sapply(comUse, length) else rep(ncol(comUse), ncol(comUse))                     # will be shrunk interatively
    if(is.list(comUse)) comUse <- comUse[(order(comUseLe, decreasing=TRUE))]    # new order
    if(is.list(comUseLe)) comUseLe <- sort(comUseLe, decreasing=TRUE)
    if(debug) message(fxNa," Ready for ",length(comUseLe)," combinations (of ",nCombin," each)")
    if(debug) {message(fxNa, "srn0c")}

    if(any(comUseLe <1)) {   # some pairwise have no good solution, nCombin might be too high
      comUseLe <- comUseLe[which(comUseLe >0)]
      ## is it possible to complete with existing ?
      foundCol <- as.integer(unique(unlist(strsplit(names(comUseLe), sep))))
      if(length(foundCol) < ncol(dat)) {
        if(nCombin ==2) stop("Can't resolve !!") else {
          warning(fxNa,"Incomplete coverage of the initial columns;  try using more data / more rows")
      } }
    }
    out <- list()  # must be list for contigs   #matrix(nrow=nrow(dat), ncol=ncol(dat), dimnames=dimnames(dat))
    combSoFar <- NULL
    i <- 1
    while(length(combSoFar) < ncol(dat) & length(comUseLe) >0 & i < ncol(combOfN)) {
      #jj=1; names(jj) <- names(comUseLe[1])
      if(which.max(comUseLe) != 1 & debug) message(fxNa," i=",i,"  which.max(comUseLe) ",which.max(comUseLe)," ")
      jk <- as.integer(unlist(strsplit(names(comUseLe[1]), sep)))
      chNew <- !(jk %in% combSoFar)          # check for new elements in this cycle
      if(any(chNew)) {                # something new to add somewhere
        chCont <- sapply(out, function(x) any(jk %in% colnames(x)))    # check each matrix of list for overlap to new
        #old#tmp <- wrMisc::normalizeThis(dat[,jk], method=if(comUseLe[1] < 25) "mean" else method, refLines=which(rowSums(!is.na(dat[refLines,jk])) ==nCombin))
        tmp <-  .rowNorm(dat[,jk], method=if(comUseLe[1] < 25) "mean" else method, refLi=which(rowSums(!is.na(dat[refLines,jk])) ==nCombin), proportMode, callFrom=fxNa, silent=silent)
        if(length(colnames(tmp)) <1) colnames(tmp) <- colnames(dat)[jk]       # just in case ...
        if(any(chCont)) {       ## add to 1st of potentially multiple existing contig(s)
          if(debug) cat("  i=",i,"  add to contig(s)   chNew ",chNew,"  chCont",chCont,"\n" )
          ovLap <- which(jk %in% colnames(out[[which(chCont)[1]]]))                  # cols of new overlapping to any existing
          ovLap2 <- if(length(out) >1) which(jk %in% unlist(lapply(out, colnames))) else ovLap      # overlap to all contigs
          colR <- which(colnames(out[[which(chCont)[1]]]) %in% jk)[1]                # col in 'main' to use for alignm
          colN <- which(colnames(tmp)==colnames(out[[which(chCont)[1]]])[colR])      # col in new    to use for alignm
          ch0 <- any(tmp[,colN] ==0, na.rm=TRUE)
          supVa <- 0
          if(ch0 & proportMode) {supVa <- 0.1; while(any((-1*supVa) %in% tmp[,colN])) {supVa <- supVa*1.02} }
          aliFa <- wrMisc::naOmit(unique(if(proportMode) out[[which(chCont)[1]]][,colR]/ (tmp[,colN] + supVa) else out[[which(chCont)[1]]][,colR] - (tmp[,colN] + supVa)))
          if(length(aliFa) <1) stop(fxNa,"Problem aligning new set of data to contig no ",which(chCont))
          if(length(aliFa) >1) aliFa <- mean(aliFa)
          tmp <- aliFa*if(length(ovLap2) +1 == ncol(tmp)) matrix(tmp[,-ovLap2], dimnames=list(NULL, colnames(tmp)[-ovLap2])) else tmp[,-ovLap2]
          out[[which(chCont)[1]]] <- cbind(out[[which(chCont)[1]]], tmp)
          if(debug) message(fxNa,"i=",i,"  use n.li=",comUseLe[1],"  n until now ",length(unique(wrMisc::naOmit(combSoFar))),"  add to ",if(sum(chCont) >1) " 1st of multiple contigs" else " contig",",   jk ",paste(jk, collapse=" ") )
        } else { if(debug) message(fxNa,"i=",i,"  use n.li=",comUseLe[1],"   start new contig",",   jk ",paste(jk, collapse=" ") )
          out[[length(out) +1]] <- tmp }    ## new contig
        combSoFar <- unique(unlist(lapply(out, colnames)))            # update
      } else if(debug) message(fxNa,"  i=",i,"  nothing new in  ",names(comUseLe[1]),"  remain ",length(comUseLe))
      ## prepeare for next cycle
      comUseLe <- comUseLe[-1]
      i <- i +1
    }
    ##  fuse multiple contigs (if necessary)
    if(debug) {message(fxNa,"snF")}
    colNa <- 1:ncol(dat) %in% unlist(lapply(out, colnames))
    if(any(!colNa) & !omitNonAlignable) { out[[length(out) +1]] <- if(sum(!colNa) >1) dat[,which(!colNa)] else matrix(dat[,which(!colNa)], dimnames=list(NULL, colnames(dat)[which(!colNa)]))
      if(debug) message(fxNa," Column(s) ",wrMisc::pasteC(which(!colNa)),"  NOT normamlized (no common lines to all other data)") }

    ii <- 1
    while(is.list(out) & length(out) >1 & ii < ncol(dat)) {   # try fusing contigs
      colR <- which(colnames(out[[1]]) %in% colnames(out[[2]]))                # col in '1st'
      if(length(colR) <1) { if(!silent) message(fxNa," Can't normalize column(s) ",paste(colnames(out[[1]]), collapse=" ")," with column(s) ",paste(colnames(out[[2]]), collapse=" "), if(isFALSE(omitNonAlignable)) ", adding without normalization" else ", omit")
        if(isFALSE(omitNonAlignable)) out[[1]] <- cbind(out[[1]], out[[2]])
        out <- out[-2]
      } else {                                                                # fuse along 1st of common cols
        colR <- colR[1]
        colN <- which(colnames(out[[2]]) %in% colnames(out[[1]]))                # col in '2nd'
        aliFa <- wrMisc::naOmit(unique(out[[1]][,colR]/ tmp[,colN[1]]))
        out[[1]] <- cbind(out[[1]], aliFa*out[[2, -colN]])
      }
      ii <- ii +1
    }
    ##  final rendering
    if(debug) {message(fxNa," final rendering  snG")}
    if(is.list(out)) out <- out[[1]]
    out <- out[,order(as.integer(colnames(out)))]
    #out <- out *stats::median(out[refLines,refGrp], na.rm=TRUE) / refVal
    if(any(refGrp > ncol(out))) refGrp <- refGrp[which(refGrp <= ncol(out))]        # in case of omitted (not alignable) columns adjust refGrp
    out <-  if(proportMode) {refVal*out/(if(sum(!is.na(dat[refLines, refGrp])) >10) stats::median(out[refLines,refGrp], na.rm=TRUE) else mean(out[refLines,refGrp], na.rm=TRUE))
    } else { refVal + out -(if(sum(!is.na(dat[refLines, refGrp])) >10) stats::median(out[refLines,refGrp], na.rm=TRUE) else mean(out[refLines,refGrp], na.rm=TRUE))}
  } else {
    if(debug) { message(fxNa,"This data/selection contains only few or no NAs, no need for iterative sparse alignment .. srn9a") }
    ch1 <- colSums(!is.na(dat[refLines,]))
    ## proportional mode : .rowNorm() adds small value to avoid divBy0 !
    if(any(ch1 ==0) & length(refLines) < nrow(dat) & omitNonAlignable & !silent) message(fxNa,"Note : ",sum(ch1 ==0)," columns are all NA when using ",length(refLines)," refLines, resuting columns will be retured as NaN")
    out <- .rowNorm(dat, refLines, method, proportMode)
    chNaN <- is.nan(out)
    if(any(chNaN)) if(!omitNonAlignable) {
      out[which(chNaN)] <- dat[which(chNaN)]
      if(!silent) message(fxNa,"Note : ",sum(ch1 ==0)," columns are all NA when using ",length(refLines)," refLines, resulting columns are retured as WITHOUT ANY normalization")
    } else if(!silent) message(fxNa,"Note : ",sum(ch1 ==0)," columns are all NA when using ",length(refLines)," refLines, resulting columns are retured as NaN")
  }
  if(length(iniColNa) >0) colnames(out) <- iniColNa
  out }

#' @export  
.rowNorm <- function(dat, refLi, method, proportMode, maxFact=10, fact0val=10, callFrom=NULL, silent=FALSE) {
  ## row-normalization procedure on matrix or data.frame 'dat'
  ## return matrix of same dimensions as 'dat'
  ## note : problem with values=0 in proportional mode (div/0) => replace 0 values by maxAbsVal/fact0val (proportMode only, nor replacement needed in additive mode)
  ## maxFact .. max normalization factor
  ## should be resistant to low degree of NAs
  suplVa <- NULL
  if(proportMode & any(dat==0, na.rm=TRUE)) {suplVa <- max(abs(dat), na.rm=TRUE)/fact0val; while(any((-1*suplVa) %in% dat, na.rm=TRUE)) suplVa <- suplVa*0.02; dat <- dat + suplVa } # substitute 0 by value 10000x smaller than smallest pos value
  if(length(dim(dat)) <2) dat <- matrix(dat, nrow=1, dimnames=list(NULL, names(dat)))
  rowMe <- if("median" %in% method & length(refLi) >10) {if(length(refLi) >1) apply(dat[refLi,], 1, stats::median, na.rm=TRUE) else stats::median(dat[refLi,], na.rm=TRUE)
  } else { if(length(refLi) >1) rowMeans(dat[refLi,], na.rm=TRUE) else mean(dat[refLi,], na.rm=TRUE)}   # per line (of refLi) mean/median
  if(length(refLi)==1) {        ## single row
    fac <- if(proportMode) rep(dat[refLi,] / rowMe, each=nrow(dat)) else rep(dat[refLi,] - rowMe, each=nrow(dat))
    chFac <- fac > maxFact
    if(any(chFac, na.rm=TRUE)) {
      if(!silent) message(callFrom,"",sum(chFac)," caluclated normalization factors exceed limit, resulting normalization may be sub-optimal ")
      chFac[which(chFac)] <- maxFact }
    out <- if(proportMode) dat * rep(dat[refLi,] / rowMe, each=nrow(dat)) else dat + rep(dat[refLi,] - rowMe, each=nrow(dat))
  } else {
    ## get median/man per row
    ## summarize row-factors to (single) per column factor
    fac <- if(proportMode) dat[refLi,] / rep(rowMe, ncol(dat)) else dat[refLi,] -rep(rowMe, ncol(dat))
    fac <- if("median" %in% method & length(refLi) >10) apply(fac, 2, stats::median, na.rm=TRUE) else colMeans(fac, na.rm=TRUE)   # summarize over columns
    ## expand to per-column matrix & apply
    fac <- matrix(rep(fac, each=nrow(dat)), nrow=nrow(dat))
    out <- if(proportMode) dat/fac else dat - fac }
  if(length(suplVa)==1) out <- out - suplVa
  out  }
   
