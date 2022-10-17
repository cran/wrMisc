#' Row Normalize
#'
#' This function was designed for normalizing data that is supposed to be particularly similar, like a collection of technical replicates.
#' Thus, initially for each row an independent normalization factor is calculated and the median or mean across all factors will be finally applied to the data.
#' This function has a special mode of operation with higher content of \code{NA} values (which may pose problems with other normalization approaches). 
#' If the \code{NA}-content is higher than the threshold set in \code{sparseLim},
#' a special procedure for sparse data will be applied (iteratively trating subsets of \code{nCombin} columns that will be combined in a later step).
#'
#' @details
#' Arguments were kept similar with function \code{normalizeThis} as much as possible.
#' In most cases data get normalized by proportional factors. In case of log2-data (very common in omics-data) normalizing by an additive factor is equivalent to a proportional factor.
#'
#' This function has a special mode of operation for sparse data (ie containing a high content of \code{NA} values).
#' 0-values by themselves will be primarily considered as true measurment outcomes and not as missing.
#' However, by using the argument \code{minQuant} all values below a given threshold will be set as \code{NA} and this may possibly trigger the sparse mode of normalizing.
#'
#' Note : Using a small value of \code{nCombin} will give the highest chances of finding sufficient complete combination of columns with sparse data.
#' However, this will also increase (very much) the computational efforts and time required to produce an output.
#'
#' When using default proportional mode a potential division by 0 could occur, when the initial normalization factor turns out as 0.
#' In this case a small value (default the maximum value of \code{dat} / 10 will be added to all data before normalizing.
#' If this also creates 0-vales in the data this factor will be multiplied by 0.03.
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
#' AA[2,c(2,6,7)] <- 1; AA[3,8] <- 1;
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

  if(length(refLines) <1) refLines <- 1:nrow(dat) else if(any(refLines > nrow(dat) | refLines < 1)) {
    if(!silent) message(fxNa,"Note: 'refLines' designs (some) lines not present in data, ignoring..")
    refLines <- refLines[which(refLines <= nrow(dat) & refLines >0)] }
  if(length(refGrp) >0) refGrp <- refGrp[which(refGrp %in% 1:ncol(dat))]
  if(length(refGrp) <1) refGrp <- 1:ncol(dat)
  if(length(minQuant)==1 & is.numeric(minQuant)) if(!is.na(minQuant)) {chMin <- dat < minQuant
    if(any(chMin)) dat[which(chMin)] <- NA }
  if(debug) {message(fxNa,"  srn0")}

  refVal <- if(sum(!is.na(dat[refLines, refGrp])) >10) stats::median(dat[refLines, refGrp], na.rm=TRUE) else mean(dat[refLines, refGrp], na.rm=TRUE)
  if(refVal==0) refVal <- 1
  if(debug) {message(fxNa,"  srn0b \n")}

  ## may be better/faster to random-shuffle column-order of combOfN ? (allow reaching last quicker)
  chNa <- sum(is.na(dat[refLines,])) / length(dat[refLines,])   # ratio of NAs
  if(debug) message(fxNa," Data contain ",round(100*chNa/length(dat),2),"% NA, using partial/iterative approach for sparse data")
  if(chNa > sparseLim) {
    if(ncol(dat) <= 9 ) {
      combOfN <- utils::combn(ncol(dat), nCombin)              # the combinations (all, ie max 126) ..
      colnames(combOfN) <- apply(combOfN, 2, paste0, collapse="_")
    } else {      ## shortcut for larger data-sets, consecutive sligtly overlapping segments will be considered
      overLap <- 2
      nSeg <- floor(ncol(dat)/(nCombin  -overLap))
      staSto <- cbind(sta=(0:nSeg)*(nCombin -overLap) +1, sto=(1:(nSeg+1))*(nCombin -overLap) +overLap)
      chStaSto <- staSto[,2] > ncol(dat)
      if(any(chStaSto)) staSto[which(chStaSto),] <- rep(ncol(dat) -c((nCombin-1), 0), each=sum(chStaSto))
      combOfN <- apply(staSto, 1, function(x) x[1]:x[2])
      if( TRUE) {  ## large data-sets : complete by few additional cross-references
        suplGrpFac <- round(log2(ncol(dat)))           ##  used for extending consecutive splits by additional non-consecutive sets : split into x subgroups for shuffeling tsart-sites
        stepW <- c(floor(nCombin/2), ceiling(nCombin/2))
        tmp <- suplGrpFac*(1:(floor(ncol(dat)/suplGrpFac)))

        combOfN <- cbind(combOfN, sapply(tmp, function(x) c(1:stepW[1], x:(x +stepW[2] -1))), c(1:stepW[1], ncol(dat) -(stepW[2] -1):0) )
        if(suplGrpFac >2) for(i in 2:suplGrpFac)  combOfN <- cbind(combOfN, sapply(tmp, function(x) c(tmp[i] -1 + 1:stepW[1], x:(x +stepW[2] -1))))
        combOfN <- apply(combOfN, 2, sort)
        combOfN <- cbind(combOfN, ncol(dat) -((nCombin -1):0) )

        ## now filter away replicated/redundant combinations or combinations with intra-repeats (of specific cols) or combiantions suggesting col-numbers out of bound
        chDu <- duplicated(apply(combOfN, 2, function(x) paste0(sort(x), collapse=sep))) | apply(combOfN, 2, function(x) any(duplicated(x))) | apply(combOfN, 2, max) > ncol(dat)
        if(any(chDu)) combOfN <- if(sum(!chDu) >1) combOfN[,-which(chDu)] else as.matrix(combOfN[,-which(chDu)])
        if(debug) {message("srn1"); srn1 <- list(dat=dat,combOfN=combOfN,nSeg=nSeg,sparseLim=sparseLim,refLines=refLines,refGrp=refGrp,refVal=refVal,method=method,proportMode=proportMode,sparseLim=sparseLim,nCombin=nCombin)}
        }

    }
    ## sort dat ??

    comUse <- apply(combOfN[,], 2, function(x) which(rowSums(is.na(dat[refLines,x])) == 0) )   # index of valid/complete lines (no NA) per set of nCombin cols
    if(!is.list(comUse)) comUse <- as.list(as.data.frame(comUse))
    chLe <- sapply(comUse, length)
    if(any(chLe >0, na.rm=TRUE)) {
      ## filter to subsets of complete lines
      combOfN <- combOfN[,which(chLe >0)]
      comUse <- if(is.matrix(comUse)) as.list(as.data.frame(comUse[,which(chLe >0)])) else comUse[which(chLe >0)]
    } else { warning("No complete lines found !! Try reducing nCombin ...")}
    ## check for coverage
    chC <- !(1:ncol(dat) %in% sort(unique(as.integer(combOfN))))
    if(any(chC, na.rm=TRUE)) {
      ## search for add'l columns complementing existing choice
      suplC <- if(sum(chC)==1) as.matrix(.complCols(which(chC), dat, nCombin)) else sapply(which(chC),.complCols, dat, nCombin)
      if(length(suplC) >0) {   ## attach, remove duplicates & update comUse
        combOfN <- cbind(combOfN, suplC)
        chDu <- duplicated(apply(combOfN, 2, function(x) paste0(sort(x), collapse=sep)))
        if(any(chDu)) combOfN <- if(sum(!chDu) >1) combOfN[,-which(chDu)] else as.matrix(combOfN[,-which(chDu)])
        comUse <- apply(combOfN, 2, function(x) which(rowSums(is.na(dat[refLines,x])) == 0) ) }   # index of valid/complete lines (no NA) per set of nCombin cols}
    }

    ##
    if(debug) {message("rn2"); rn2 <- list(dat=dat,combOfN=combOfN,comUse=comUse,nSeg=nSeg,sparseLim=sparseLim,refLines=refLines,refGrp=refGrp,refVal=refVal,method=method,proportMode=proportMode,sparseLim=sparseLim,nCombin=nCombin)}
    fact <- .rowNormFact(dat=dat, combOfN=combOfN,comUse=comUse, method=method, refLi=refLines, refGrp=refGrp, proportMode=proportMode, minQuant=minQuant, maxFact=10, omitNonAlignable=omitNonAlignable, silent=silent, debug=debug, callFrom=fxNa)
    if(debug) {message("rn3")}

    ##  final rendering
    if(debug) {message(fxNa," final rendering  srnG")}
    out <- dat / rep(fact, each=nrow(dat))
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
.complCols <- function(x, dat, nCombin) {
  ## search (complementing) columns for best coverage of non-NA data for rowNormalization
  ## Context : In sparse matrix 'dat' search subsets of columns with some rows as complete (no NA).
  ## This function was designed to complete the selection of columns of sparse matrix 'dat' with sets of 'nCombin' columns at complete 'coverage'
  ## x (integer) .. column number for with other columns to combine & give (some) complete non-NA lines are seeked
  ## dat (matrix) .. init data, smay be parse matrix with numerous NA
  ## nCombin(integer) .. number of columns used to make complete subset
  ## returns matrix of column-indexes complementing (nCombin rows)
  if(any(length(dim(dat)) !=2, dim(dat) < 1:2)) stop("Invalid argument 'dat'; must be matrix or data.frame with min 1 line and 2 cols")
  if(is.null(colnames(dat))) colnames(dat) <- 1:ncol(dat)   # colnames are essential !
  msg <- "x must be integer to design column number of 'dat'"
  if(length(x) <0) stop(msg) else {
    if(is.character(x)) x <- which(colnames(x) %in% x)
    if(length(x) >1) x <- x[1]
    if(x > ncol(dat) | x < 1) stop(msg) }
  ## main
  z3 <- !is.na(dat[which(!is.na(dat[,x])), -x])           # remaining cols: !is.na of extract of lines to consider (based on incomplete col)
  z4 <- if(is.matrix(z3)) apply(z3, 1, which) else which(z3)
  if(!is.list(z4)) z4 <- as.list(as.data.frame(z4))
  chLe <- sapply(z4, length)
  chLe <- chLe > nCombin -2
  if(any(!chLe)) z4 <- z4[which(chLe)]
  if(length(z4) >0) { if(length(z4) >1) {
    c(as.integer(names(sort(table(unlist(z4)), decreasing=TRUE))) [1:(nCombin -1)], x)
    } else c(z4[[1]][1:(nCombin -1)], x)} else NULL }

#example#
# .complCols(10, dat, nCombin=3)


#' @export
.rowNormFact <- function(dat, combOfN, comUse, method="median", refLi=NULL, refGrp=NULL, proportMode=TRUE, minQuant=NULL, maxFact=10, omitNonAlignable=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## obtain normalization factor
  ## combOfN (matrix) ..  # matrix of index for all sub-groups (assumed as sorted)
  ## comUse (list) .. index of complete lines for each col of combOfN
    #combOfN <- #jh <- sapply(names(comUseLe), function(x) as.integer(unlist(strsplit(x, sep))))   # matrix of index for all sub-groups
  fxNa <- wrMisc::.composeCallName(callFrom, newNa=".rowNormFact")
  outL <- list()
  outFa <- matrix(nrow=ncol(combOfN), ncol=ncol(dat), dimnames=list(1:ncol(combOfN), 1:ncol(dat)))
  #outFa <- matrix(nrow=ncol(dat), ncol=ncol(dat))
  conti <- list()
  if(debug) message(callFrom, "..start loop") 
  for(i in 1:ncol(combOfN)) {
    if(length(comUse[[i]]) >0) {                        # has sufficent valid data .. check with refLi
      refLiX <- if(length(refLi) >0) comUse[[i]][which(comUse[[i]] %in% refLi)] else comUse[[i]]
      if(length(refLiX) <1) {comUse[[i]] <- NA; if(debug) message(fxNa,"No valid lines remain when considering 'refLi' (of columns ",wrMisc::pasteC(utils::head(combOfN[,i])),"), loop no ",i," !!")}
    }
    if(length(comUse[[i]]) >0 & sum(is.na(comUse[[i]]))==0) {                        # has sufficent valid data ..
      tm2 <- if(length(refLiX ==1)) dat[refLiX,combOfN[,i]]/mean(dat[refLiX,combOfN[,i]], na.rm=TRUE) else .rowNorm(dat[,combOfN[,i]], method=if(length(comUse[[i]]) < 25) "mean" else method, refLi=refLiX, proportMode=proportMode, retFact=TRUE, callFrom=fxNa, silent=silent)
      if(debug & length(dim(tm2)) >1) message(fxNa,"Strange, tm2 is matrix and NOT vector !!")
      outFa[i, combOfN[,i]] <- if(length(dim(tm2)) >1) tm2[1,] else tm2
      ## construct 'contigs'
      if(length(conti) <1) { conti <- list(conti1=matrix(combOfN[,i], nrow=1, dimnames=list(paste0("li",i),NULL)))   # initiate 1st
      } else {
        tm4 <- unlist(lapply(conti, function(x) sum(combOfN[,i] %in% x)))          # how many hits in which contig
        if(any(tm4 >0)) { if(sum(tm4 >0) ==1) { conti[[which(tm4 >0)]] <- rbind(conti[[which(tm4 >0)]], combOfN[,i])  # (simplest case) add to (single) existing contig
            rownames(conti[[which(tm4 >0)]])[nrow(conti[[which(tm4 >0)]])] <- paste0("li",i)
          } else { maxC <- which.max(tm4)                                      # first align to contig with most common elements
            conti[[maxC]] <- rbind(conti[[maxC]], combOfN[,i])                 # history
            rownames(conti[[maxC]])[nrow(conti[[maxC]])] <- paste0("li",i)
            if(debug) message(fxNa,"Adding contig no ",maxC," to main chunk")
            supC <- tm4[-maxC]
            conti[[maxC]] <- rbind(conti[[maxC]], conti[[supC]][nrow(conti[[supC]]):1,])        # add reversed other contig
            conti <- conti[-supC]
          }
        } else { if(debug) message(fxNa,"  i=",i,"  starting new contig..")
          conti[[length(conti) +1]] <- matrix(combOfN[,i], nrow=1, dimnames=list(paste0("li",i),NULL))
          names(conti)[length(conti)] <- paste0("conti",length(conti)) }  # add/start new contig
      }
    }
    if(debug) {message(fxNa," ..end loop",i," out of ",ncol(combOfN)); rnf2b <- list(dat=dat,combOfN=combOfN,comUse=comUse,method=method,outFa=outFa,conti=conti,i=i)}
  }
  if(debug) {message(fxNa," start 2nd part  rnf3")}
  ## now need to adjust factors row by row (if any overlap)
  if(length(conti) >1) conti <- conti[order(sapply(conti, nrow), decreasing=TRUE)]
  ## assume sorted conti, ie 1st element of conti has most data
  liNo <- as.integer(sub("^li","", rownames(conti[[1]])))  # needed for outFa
  if(ncol(combOfN) >1) for(i in 2:nrow(conti[[1]])) {
    ## loop over all lines of contig (after 2nd), ie over all combinations of subsets of columns, to adjst normalization factor
    if(i ==2) {  # 2nd line, ie only single preceeding line to adjust to
      #ovLa <- which(conti[[1]][i,] %in% conti[[1]][i -1,])
      #refPo <- which(conti[[1]][i -1,] %in% conti[[1]][i,])
      ovLa <- conti[[1]][i, which(conti[[1]][i,] %in% conti[[1]][i -1,])]
      refPo <- conti[[1]][i -1, which(conti[[1]][i -1,] %in% conti[[1]][i,])]      # needed ??
      tm2 <- (outFa[liNo[i],][ovLa]) / (outFa[liNo[i -1],][ovLa])   #(outFa[1,][conti[[1]][i -1,][refPo]]) #((outFa[liNo[1], refPo]) )
      if(length(tm2) >1) tm2 <- if("median" %in% method & length(tm2) > 5) stats::median(tm2, na.rm=TRUE) else mean(tm2, na.rm=TRUE)
      if(debug) message(fxNa," outFa[1,]", wrMisc::pasteC(round(outFa[1,],2)),"   add li 2.. tm2 ",wrMisc::pasteC(round(tm2,2)),"   correct li2 to  ", wrMisc::pasteC(round(outFa[i,] / tm2,2)))
      outFa[i,] <- outFa[i,] / tm2
    } else {
      if(debug) message(fxNa," outFa  li=",i-2,"  ",wrMisc::pasteC(round(outFa[i-2,],2)),"\n  li=",i-1,"  ",wrMisc::pasteC(round(outFa[i-1,],2)),"\n  li=",i,"  ",wrMisc::pasteC(round(outFa[i,],2)))
      ovLa <- apply(conti[[1]][1:(i-1),], 1, function(x) x %in% conti[[1]][i,])   # overlap of prev contigs to current i
      refPo <- apply(conti[[1]][1:(i-1),], 1, function(x) conti[[1]][i,] %in% x)  # overlap of current i to prev contigs
      tm2 <- list()
      for(j in 1:ncol(ovLa)) if(any(ovLa[,j], na.rm=TRUE)) {
        tm2[[j]] <- outFa[liNo[i], conti[[1]][i,][refPo[,j]]] / outFa[liNo[i], conti[[1]][i,][which(ovLa[,j])]]
        if(is.null(names(tm2[[j]]))) names(tm2[[j]]) <- conti[[1]][j,][which(ovLa[,j])]
      }
      tm2 <- tm2[sapply(tm2, length) >0]
      fact <- tapply(unlist(tm2), names(unlist(tm2)), function(x)  if(length(x) > 5 & "median" %in% method) stats::median(x) else mean(x))    # summarize over all lines matching up to i (keep factors for cols separate)
      fact <- if(length(fact) > 5 & "median" %in% method) stats::median(fact) else mean(fact)
      chFa <- rbind(fact > maxFact , fact < 1/maxFact)
      if(any(chFa, na.rm=TRUE)) {if(any(chFa[1,])) fact[which(chFa[1,])] <- maxFact; if(any(chFa[2,])) fact[which(chFa[2,])] <- 1/maxFact }
      outFa[liNo[i],] <- outFa[liNo[i],] / fact
      if(debug) { message(fxNa,"Done with loop ",i," rnf3b")}
    }
  }
  ## (future ?) fuse multiple contigs (if necessary)
  ## compress to single norm-factor per column
  finFa <- apply(outFa, 2, function(x) if(sum(!is.na(x)) > 5 & "median" %in% method) stats::median(x, na.rm=TRUE) else mean(x, na.rm=TRUE))
  if(debug) message("rnf4"); rnf4 <- list(finFa=finFa,dat=dat,combOfN=combOfN,comUse=comUse,method=method,outFa=outFa,conti=conti,ovLa=ovLa,refPo=refPo,i=i,outFa=outFa,tm2=tm2,fact=fact,maxFact=maxFact)
  if(length(conti) >1) {
    chCo <- unlist(conti[-1])
    ch2 <- !chCo %in% conti[[1]]
    if(any(ch2)) {
      if(omitNonAlignable) finFa[chCo[which(ch2)]] <- NA
      if(!silent) message(fxNa,"Note: ",sum(ch2)," columns (no ",if(sum(ch2) <7) wrMisc::pasteC(chCo[which(ch2)]) else paste0(paste0(utils::head(chCo[which(ch2)]), collapse=", ")," ..."),
        ") can't get aligned !", if(omitNonAlignable)" Setting respective norm-factors to NA") }
  }
  finFa }


#' @export
.rowNorm <- function(dat, refLi, method, proportMode, maxFact=10, fact0val=10, retFact=FALSE, callFrom=NULL, silent=FALSE) {
  ## row-normalization procedure on matrix or data.frame 'dat'
  ## return matrix of same dimensions as 'dat'
  ## note : problem with values=0 in proportional mode (div/0) => replace 0 values by maxAbsVal/fact0val (proportMode only, nor replacement needed in additive mode)
  ## maxFact .. max normalization factor
  ## should be resistant to low degree of NAs
  suplVa <- NULL
  if(proportMode & any(dat==0, na.rm=TRUE)) {suplVa <- max(abs(dat), na.rm=TRUE)/fact0val; while(any((-1*suplVa) %in% dat, na.rm=TRUE)) suplVa <- suplVa*0.02; dat <- dat + suplVa } # substitute 0 by value 10000x smaller than smallest pos value
  if(length(dim(dat)) <2) dat <- matrix(dat, nrow=1, dimnames=list(NULL, names(dat)))
  rowMe <- if("median" %in% method & length(refLi) >10) {             # per row mean or median (for each line of refLi)
    if(length(refLi) >1) apply(dat[refLi,], 1, stats::median, na.rm=TRUE) else stats::median(dat[refLi,], na.rm=TRUE)
  } else {
    if(length(refLi) >1) rowMeans(dat[refLi,], na.rm=TRUE) else mean(dat[refLi,], na.rm=TRUE)}
  corFa <- if(proportMode) dat[refLi,]/ rep(rowMe, ncol(dat)) else dat[refLi,] -rep(rowMe, ncol(dat))  # cor-factors
  if(length(dim(corFa)) >1) corFa <- if("median" %in% method & length(refLi) >10) apply(corFa, 2, stats::median, na.rm=TRUE) else colMeans(corFa, na.rm=TRUE)  # per column cor-factor
  if(retFact) corFa else { corFa <- rep(corFa, each=nrow(dat))
    if(proportMode) dat / corFa else dat - corFa }
  }
  
