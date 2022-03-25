#' Search and Select Groups of Replicates 
#'
#' This function was designed for mining annotation information organized in multiple columns to identify the grouping of multiple samples.   
#' 
#' 
#' @details 
#' Statistical tests requite specifying which samples should be considered as replicates. 
#' In some cases, like the Sdrf-format, automatic mining of such annotation to indentify an experiment's underlying structure of replicates 
#' may be challanging, since the key information may not always be found in the same column. 
#' For this reason this function allows inspecting all columns of a matrix of data.frame to identify which colmns may serve describing groups of replicates.   
#' 
#' The argument \code{exclNoRepl=TRUE} allows excluding all columns with different content for each line (like line-numbers), ie information without any replicates.
#' It is set by default to \code{TRUE} to exclude such columns, since statistical tests usually do require some replicates. 
#' 
#' When using as \code{method="combAll"}, there is risk all lines (samples) will be be considered different and no replicates remain.  
#' To avoid this situation the argument can be set to \code{method="combNonOrth"}.
#' Using this mode it will be checked if adding more columns will lead to complete loss of replicates, and -if so- concerned columns omitted.
#'
#' 
#'
#' @param x (matrix or data.frame) the annotation to inspect; each column is supposed to describe another set of annoation/metadata for the rows of \code{x} (min 1 row and 1 column),
#' @param method (character, length=1) the procedure to choose column(s) with properties of information, may be \code{highest} (max number of levels)
#'   \code{lowest} (min number of levels), \code{median} (median of all options for number of levels), 
#'   \code{combAll} (combine all columns of \code{x}) or \code{combNonOrth} (combine only non-orthogonal columns of \code{x}, to avoid avoid n lines with n levels);
#'   lazy evluation of the argument is possible
#' @param sep (character) separator used when any method combining multiple columns (eg combAll, combNonOrth)  is chosen (should not appear anywhere in \code{x})
#' @param exclNoRepl (logical) decide whether columns with all values different (ie no replicates or max divergency) should be excluded
#' @param trimNames (logical) optional trimming of names in \code{x} by removing redundant heading and tailing text
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging 
#' @return This function returns a list with $col (column index relativ to \code{x}), $lev (abstract labels of level), 
#'   $meth (note of method finally used) and $allCols with general replicate structure of all columns of  \code{x}
#' @seealso  \code{\link[base]{duplicated}}, uses \code{\link{trimRedundText}} 
#' @examples
#' ## a is all different, b is groups of 2,
#' ## c & d  are groups of 2 nut NOT 'same general' pattern as b
#' strX <- data.frame(a=letters[18:11], b=letters[rep(c(3:1,4), each=2)], 
#'  c=letters[rep(c(5,8:6), each=2)], d=letters[c(1:2,1:3,3:4,4)],        
#'  e=letters[rep(c(4,8,4,7),each=2)], f=rep("z",8) ) 
#' strX
#' replicateStructure(strX[,1:2])
#' replicateStructure(strX[,1:4], method="combAll")
#' replicateStructure(strX[,1:4], method="combAll", exclNoRepl=FALSE)
#' replicateStructure(strX[,1:4], method="combNonOrth", exclNoRepl=TRUE)
#' replicateStructure(strX, method="lowest")
#' 
#' 
#' 
#' @export
replicateStructure <- function(x, method="median", sep="__", exclNoRepl=TRUE, trimNames=FALSE, silent=FALSE, callFrom=NULL, debug=FALSE) {
  ##
  fxNa <- .composeCallName(callFrom, newNa="replicateStructure")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  out <- ou3 <- NULL              # initialize

  datOK <- all(length(x) >0 & length(dim(x)) >0)
  if(debug) message(fxNa,"ini datOK  ",datOK)
  ## check for unique colnames
  if(datOK) { iniColNa <- colnames(x)
    chColN <- duplicated(colnames(x), fromLast=FALSE)
    if(any(chColN)) { if(!silent) message(fxNa,"Need to correct redundant colnames !!!")}
  } else if(!silent) message(fxNa,"invalid entry for 'x' .. should be matrix or data.frame (min 1 row, 1 column) .. returning NULL")
  ## single available/given .. shortcut
  if(datOK) { if(ncol(x)==1) { datOK <- FALSE
    if(debug) message(fxNa,"single column available/relevant .. shortcut")
    nn <- match(x[,1], unique(x[,1]))
    names(nn) <- x[,1]
    coln <- 1
    names(coln) <- colnames(x)
    out <- list(col=coln, lev=nn, meth="single informative")    
  } else if(datOK & nrow(x)==1) { ou3 <- list(col=1, lev=1, meth="single row")
    if(!silent) message(fxNa,"Note : Only single line with levels, ie single group wo any replicates")
    datOK <- FALSE } }
  ## check argument 'method'
  if(datOK) {  
    methOpt <- c("highest","lowest","median","combAll","combNonOrth")
  	msg <- c("  'method' should designate one of the following options ",pasteC(methOpt,quoteC="'"),"; setting to default")
    if(identical(method,"min")) method <- "lowest"
    if(identical(method,"max")) method <- "highest"
    if(length(method) <1) { method <- "median"
      warning(fxNa,"Argument 'method' seems empty !",msg)}
    chNa <- is.na(method)
    if(any(chNa)) { method <- "median"
      warning(fxNa,"Argument 'method' may not contain NA !",msg)}
    if(length(method) >1) { method <- method[1]
      if(!silent) message(fxNa,"Argument 'method' should be of length=1, using first")}
    chMeth <- pmatch(method, methOpt)
    chMe2 <- any(is.na(chMeth))
    if(chMe2) { method <- "median"; chMeth <- 3
      warning(fxNa,"Unable to match argument 'method' to any of the options ",pasteC(methOpt,quoteC="'"),msg)}
    method <- methOpt[chMeth]  
  }
  ## check separator
  if(length(sep) != 1) { sep <- "__"
    warning(fxNa," Argument 'sep' must be character and of length =1; resetting to default")}
  if(nchar(sep) <1) { sep <- "__"
    warning(fxNa," Argument sep=\"\" may lead to incorrect comparisons, resetting to default")}  
  chSep <- grep(sep, x)
  if(length(chSep) >0) warning(fxNa," BEWARE, the annotation data do contain ",length(chSep)," instances of '",
    sep,"' ! The results of combined searches may be incorrect. Please consider using a different separator")  
  
  ##
  if(debug) {message(fxNa,"fd0"); fd0 <- list(x=x,ou3=ou3,method=method,sep=sep,exclNoRepl=exclNoRepl,datOK=datOK)}

  ## utility functions
  chStru <- function(y, debug) {   # check duplication structure of vector
    dup <- duplicated(y)
    out <- -1
    if(sum(dup) <1) out <- if(isTRUE(exclNoRepl)) 0 else 1:length(y)     # all unique
    if(sum(dup) == length(y) -1) out <- 1                                # all duplic
    if(any(out < 0)) out <- match(y, unique(y))
    if(debug) message(fxNa," chStru : ",wrMisc::pasteC(out))
    out }
  fMin <- function(y, ref=y) { coln <- which.min(apply(y, 2, max, na.rm=TRUE)); 
    if(debug) {message(fxNa," -> fMin   colnames y", colnames(y),"\n"); fMi <- list(y=y,coln=coln, ref=ref)}
    refn <- if(identical(y, ref)) coln else which(colnames(y)[coln]==colnames(ref))
    nn <- y[,coln]; names(nn) <- ref[,refn];
    names(refn) <- colnames(y)[coln] 
    list(col=refn, lev=nn, meth="single min col")}
  fMax <- function(y, ref=y) { coln <- which.max(apply(y, 2, max, na.rm=TRUE)); 
    if(debug) {message(fxNa," -> fMax   colnames y", colnames(y),"\n"); fMa <- list(y=y,coln=coln, ref=ref)}
    refn <- if(identical(y, ref)) coln else which(colnames(y)[coln]==colnames(ref))
    nn <- y[,coln]; names(nn) <- ref[,refn];
    names(refn) <- colnames(y)[coln] 
    list(col=refn, lev=nn, meth="single max col")}
  fMed <- function(y, ref=y) { zz <- apply(y, 2, max, na.rm=TRUE); 
    coln <- which(zz==stats::median(if((ncol(y) %% 2) ==0) c(zz[1],zz) else zz))[1]; 
    if(debug) {message(fxNa," -> fMed   colnames y", colnames(y),"\n"); fMe <- list(y=y,zz=zz,coln=coln, ref=ref)}
    refn <- if(identical(y, ref)) coln else which(colnames(y)[coln]==colnames(ref))
    names(refn) <- colnames(y)[coln]
    nn <- y[,coln]; names(nn) <- ref[,refn]
    list(col=refn, lev=nn, meth="single median col")}
  fCombAll <- function(y, ref=y) {   # uses debug
    refn <- if(identical(y, ref)) 1:ncol(y) else match(colnames(y), colnames(ref))
    names(refn) <- colnames(y)
    tm <- if(ncol(y)==2) paste(y[,1],y[,2]) else apply(y, 1, paste, collapse=" ");
    nn <- match(tm, unique(tm))
    if(debug) {message(fxNa," -> fCombAll"); fComb <- list(y=y,ref=ref,refn=refn, tm=tm,nn=nn)}
    names(nn) <- if(ncol(y)==1) ref[,refn] else apply(ref[,refn],1,paste0,collapse="_")
    list(col=refn, lev=nn, meth="comb all col") }
  
  fComNonOrth <- function(y, oup="min", ref=y) { if(debug) message(fxNa," -> fComNonOrth")
    tm <- if(ncol(y)==2) as.matrix(paste(y[,1], y[,2])) else {
      if(ncol(y)==3) cbind(paste(y[,1],y[,2]), paste(y[,1],y[,3])) else apply(y[,-1], 2, paste,y[,1])}      # pairwise groups/combin
    st2 <- as.matrix(apply(tm, 2, function(z) match(z, unique(z))))                # levels of pairwise combin
    colnames(st2) <- paste(colnames(y)[1], colnames(y)[2:ncol(y)], sep=sep)
    maxL <- apply(st2, 2, max)
    if(debug) {message(fxNa," -> fComNonOrt"); fComNonOrt <- list(y=y,oup=oup,tm=tm, st2=st2,maxL=maxL,x=x)}
    if(any(maxL ==nrow(y))) st2 <- as.matrix(st2[,which(maxL < nrow(y))])
    if(length(st2) >0) { if(TRUE) {maxP <- which.max(apply(st2, 2, max)); 
      coln <- which(colnames(y) %in% unlist(strsplit(names(maxP), split=sep)))
      refn <- if(identical(y, ref)) coln else match(colnames(y)[coln], colnames(ref))
      names(refn) <- colnames(y)[coln]
      nn <- st2[,maxP]; names(nn) <- if(length(refn)==1) ref[,refn] else {
        if(length(refn)==2) paste(ref[,refn[1]],ref[,refn[2]],sep="_") else apply(ref[,refn], 1, paste0, collapse="_")}
      out <- list(col=refn, lev=nn, meth="combNonOrth col" )}
    } else out <- fMax(y)
    out }
                 
  ## get generalized pattern of replicates  
  if(datOK) {
    str1 <- apply(x, 2, chStru, debug=debug)
    chStr <- if(is.matrix(str1)) rep(nrow(str1) <nrow(x), ncol(x)) else {sapply(str1, length) < nrow(x)}
    if(debug) message(fxNa,"fd1"); fd1 <- list(x=x,method=method,ou3=ou3,str1=str1,sep=sep,exclNoRepl=exclNoRepl,datOK=datOK)
    if(all(chStr)) {
     if(exclNoRepl){
       if(!silent) message(fxNa,"NONE of the columns has any replicates, setting exclNoRepl=FALSE")
        exclNoRepl <- FALSE
        str1 <- apply(x, 2, chStru, debug=debug)
        chStr <- sapply(str1, length) < nrow(x)
      } else warning(fxNa,"Can't find replicates for ANY of the columns !! ") 
    }
    if(any(chStr)) str1 <- str1[which(!chStr)]      
    if(is.list(str1)) str1 <- as.matrix(as.data.frame(str1))
    if(length(dim(str1)) <2) str1 <- matrix(unlist(str1), ncol=ncol(x), dimnames=dimnames(x))    
    if(debug) message(fxNa,"fd2"); fd2 <- list(x=x,method=method,ou3=ou3,str1=str1,sep=sep,exclNoRepl=exclNoRepl,datOK=datOK)
    if(!isTRUE(exclNoRepl)) {              ## check if single col at max levels may serve as shortcut
      chMax <- sapply(str1, max, na.rm=TRUE) ==nrow(x)
      if(any(chMax)) { datOK <- FALSE
        coln <- which.min(chMax)
        names(coln) <- colnames(str1)[coln]
        nn <- 1:nrow(x)
        names(nn) <- str1[,coln]
        out <- list(col=coln, lev=nn, meth="(shortcut, first) single col at max divergence", allCols=str1) } }
  }

  if(datOK) {
    ## remove non-useful, obtain matrix of indexes
    if(is.list(str1)) { chLe <- sapply(str1, length)
      if(any(chLe) <2) if(sum(duplicated(chLe[which(chLe >1)])) == sum(chLe >1) -1) { 
        ou3 <- as.matrix(as.data.frame(str1[which(chLe >1)]))  # all remaining of same length
      } else { ou3 <- NULL; message(fxNa,"Trouble ahead ?  Different length when trying to extract patterns")  #
      }
    } else { ou3 <- if(length(dim(str1)) >1) str1 else NULL }
    if(length(ou3) >0) {  # now look for replicates of these patterns (paste-collapse series internal) & remove redundant columns (keep 1st instance from left)
      tm2 <- apply(ou3, 2, paste0, collapse="") 
      chDu <- duplicated(tm2, fromLast=FALSE)
      if(any(chDu)) {tm2 <- tm2[which(!chDu)]; 
        ou3 <- matrix(ou3[,which(!chDu)],nrow=nrow(x),dimnames=list(NULL,colnames(ou3)[which(!chDu)]))}  
      if(debug) {message(fxNa,"fd4"); fd4 <- list(x=x,method=method,out=out,ou3=ou3,tm2=tm2,str1=str1,chDu=chDu,sep=sep,exclNoRepl=exclNoRepl)}
  
      out <- if(ncol(ou3) <2) {
        if(debug) message(fxNa," single col for choice ..")
        co <- which(colnames(x) ==colnames(ou3))
        names(co) <- colnames(ou3)
        nn <- as.integer(ou3)
        names(nn) <- x[,co]
        list(col=co, lev=nn, meth="single informative col")
      } else switch(method,                     ## return column index
        highest =fMax(ou3, ref=x),
        lowest =fMin(ou3, ref=x),
        median =fMed(ou3, ref=x),
        combAll=fCombAll(ou3, ref=x),
        combNonOrth=fComNonOrth(ou3, ref=x) )
    }
  ## optional trim names to ou3$lev
  if(isTRUE(trimNames)) names(out$lev) <- trimRedundText(names(out$lev), spaceElim=TRUE, silent=silent, callFrom=fxNa, debug=debug)
  }
  out }
   
