#' Extended version of merge for multiple objects (even without rownames)
#'
#' \code{mergeW2} povides flexible merging out of 'MArrayLM'-object (if found, won't consider any other input-data) or of separate vectors or matrixes.
#' The main idea was to have somthing not adding add'l lines as merge might do, but to stay within the frame of the 1st argument given, even when IDs are repeated, 
#' so the output follows the order of the 1st argument, non-redundant IDs are created (orig IDs as new column).
#' If no 'MArrayLM'-object found: try to combine all elements of input '...', input-names must match predefined variants 'chInp'.
#' IDs given in 1st argument and not found in later arguments will be displayed as NA in the output matrix of data.frame.  
#'  Note : (non-data) arguments must be given with full name (so far no lazy evaluation, may conflict with names in 'inputNamesLst'). 
#'  Note : special characters in colnames bound to give trouble. 
#'  Note : when no names given, \code{mergeW2} will presume order of elements (names) from 'inputNamesLst'. 
#' PROBLEM : error after xxMerg3 when several entries have matching (row)names but some entries match only partially  (what to do : replace with NAs ??)
#' @param ... all data (vectors, matrixes or data.frames) intendes for merge
#' @param nonRedundID (logical) if TRUE, allways add 1st column with non-redundant IDs (add anyway if non-redundant IDs found )
#' @param convertDF (logical) allows converting output in data.frame, add new heading col with non-red rownames & check which cols should be numeric
#' @param selMerg (logical) if FALSE toggle to classic merge() (will give more rows in output in case of redundant names
#' @param inputNamesLst (list) named list with character vectors (should be unique), search these names in input for extracting/merging elements use for 'lazy matching' when checking names of input, default : 7 groups ('Mvalue', 'Avalue','p.value','mouseInfo','Lfdr','link','filt') with common short versions
#' @param noMatchPursue (logical) allows using entries where 0 names match (just as if no names given)
#' @param standColNa (logical) if TRUE return standard colnames as defined in 'inputNamesLst' (ie 'chInp'), otherwise colnames as initially provided
#' @param lastOfMultCols may specify input groups where only last col will be used/extracted
#' @param duplTxtSep (character)  separator for counting/denomiating multiple occurances of same name
#' @param debug (logical) for bug-tracking: more/enhanced messages and intermediate objects written in global name-space 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return matrix or data.frame of fused data
#' @seealso \code{\link[base]{merge}}
#' @examples
#' t1 <- 1:10; names(t1) <- letters[c(1:7,3:4,8)]
#' t2 <- 20:11;  names(t2) <- letters[c(1:7,3:4,8)]
#' t3 <- 101:110; names(t3) <- letters[c(11:20)]
#' t4 <- matrix(100:81,ncol=2,dimnames=list(letters[1:10],c("co1","co2")))
#' t5 <- cbind(t1=t1,t52=t1+20,t53=t1+30)
#'   t1; t2; t3;  cbind(t1,t2)
#' mergeW2(Mval=t1,p.value=t2,debug=FALSE)
#' @export
mergeW2 <- function(...,nonRedundID=TRUE,convertDF=TRUE,selMerg=TRUE,inputNamesLst=NULL,noMatchPursue=TRUE,standColNa=FALSE,
  lastOfMultCols=c("p.value","Lfdr"),duplTxtSep="_",silent=FALSE,debug=FALSE,callFrom=NULL){
  inp <- list(...)
  out = NULL
  chAnnCol <- c("mouseOver","link","uniqUniprot","Uniprot","firstOfMultUnipr","combUnipr","custMouOv","customMouseOver") # colnames in MArrayLM$annot to extract
  chInp <- if(is.list(inputNamesLst) & length(inputNamesLst)>1) inputNamesLst else {    # names of list-elements to allowed to extract
    list(Mvalue=c("Mvalue","Mval","M.value","M.val","Mv","M"),
    Avalue=c("Avalue","Aval","Amean","A.value","A.val","Av","A"),
    Amatr=c("Amatrix","Amatri","Amatr","Amat","Ama","Am","A.matrix","A.matr","A.mat","A.m"),
    p.value=c("pValue","pVal","p.value","p.val","pVa","pV","p"),
    Lfdr=c("Lfdr","lfdr","FDR","fdr","FdrList"), means=c("allMeans","means"), 
    mouseInfo=c("mouseInfo","info","inf","mou","mouse"), link=c("link","www"),filt=c("filtFin","filter","filt","fi"),
    info2=c("shortInfo","info2","suplInfo"), annot=c("annot","annotation"))}
  if(debug) silent <- FALSE
  fxNa <- .composeCallName(callFrom,newNa="mergeW2")
  if(length(unlist(chInp)) > length(unique(unlist(chInp)))) stop(fxNa,"Problem with redundant names in (predefined/custom) 'inputNamesLst'")
  if(length(inp) <1) stop(fxNa," No valid entries !")
  ## note: potential problem with multiple classes per objet
  cheCla <- lapply(inp,class)
  cheCla2 <- sapply(cheCla,paste,collapse=" ")
  if(debug) message(fxNa," class of arguments :  ",pasteC(cheCla2))
  if("MArrayLM" %in% unlist(cheCla)) {
    tmp <- inp[[which(grep("MArrayLM",cheCla2))]]
    if(!silent) message(fxNa,"Found 'MArrayLM'-object with ",length(names(tmp))," types of data for ",length(tmp)," elements/genes")
    tm2 <- .convertNa(names(tmp),chInp)
    if(length(naOmit(tm2)) > length(unique(naOmit(tm2)))) {
      tm2ta <- table(naOmit(tm2))
      msg <- c(fxNa,"PROBLEM : multiple names of MArrayLM-obj do match to (single) type of data searched '")
      for(i in which(tm2ta >1)) {
        if(!silent) message(msg,names(tm2ta)[i],"' : found ",pasteC(names(tmp)[which(tm2==names(tm2ta)[i])],quoteC="'"),
          " -> using last (otherwise try to modify 'chInp')")
        tm3 <- which(tm2==(names(tm2ta)[i]))
        tm2[tm3[-1*length(tm3)]] <- NA }
    }
    if(sum(!is.na(tm2)) >0) {names(tmp)[!is.na(tm2)] <- naOmit(tm2)            # set names of interest in MArray-obj to standard names
      if(!silent) message(fxNa,"Resetting names in 'MArrayLM'-obj to standardized names") }
    if(length(inp) >1) {
      ## regular case (not 'MArrayLM' class)
      chLe <- length(grep("MArrayLM",cheCla2)) <1 & sapply(inp,length) >0 & !is.na(.convertNa(names(inp),chInp))
      inp2 <- if(sum(chLe) >0) inp[which(chLe)] else NULL
    } else inp2 <- NULL
    inp <- list()
    useMaEl <- naOmit(match(names(chInp),names(tmp)))                          # pick only elements of interest out of list with mult options
    for(i in 1:length(useMaEl)) inp[[i]] <- tmp[[useMaEl[i]]]
    names(inp) <- names(tmp)[useMaEl]
    if(length(inp2) >0) { for(i in names(inp2)) inp[[i]] <- inp2[[i]]           # (re)inject information given besides MArrayLM (replacing) 
      useMaEl <- naOmit(match(names(chInp),names(tmp)))                        # update
      if(!silent) message(fxNa," adding ",length(inp2)," non MArrayLM objects: ",pasteC(names(inp2),quoteC="'"))}
    ## ++ need to specify which question (MAarray may contain results for multiple comparisons !!)
    ## immediately pick relevant cols and rename p.value  & Lfdr & annot ??
    colNa <- lapply(inp,colnames)
    rmIntercept <- TRUE                                                               ## <<== supl parameter ??
    exclNa  <- "(Intercept)"
    if(any(unlist(colNa) %in% exclNa) & rmIntercept) {                          # ready to remove cols names "(Intercept)"
      useEl <- which(sapply(colNa,function(x) any(x %in% exclNa)))
      if(length(useEl) >0) for(i in useEl) {
        useCo <- which(!colnames(inp[[i]]) %in% exclNa)
        inp[[i]] <- if(length(useCo) <1) NULL else {if(length(useCo) >1) inp[[i]][,useCo] else matrix(
          inp[[i]][,useCo],nrow=nrow(inp[[i]]),dimnames=list(rownames(inp[[i]]),colnames(inp[[i]])[useCo]))}
        if(ncol(inp[[i]])==1) colnames(inp[[i]]) <- names(inp)[i] }             # if only 1 col remaining rename to name of list-element
    }
    ## (MArrayLM-only) filter suitable cols from annot :
    if(any(chInp$annot %in% names(inp)) & length(chAnnCol) >0) {
      tmp <- which.max(names(inp) %in% chInp$annot)                             # use last of names from 'inp' recognized as annot
      if(length(dim(inp[[tmp]])) >0) inp[[tmp]] <- inp[[tmp]][,which(colnames(inp[[tmp]]) %in% chAnnCol)] }    # select specific cols
    ##
    cheCla <- sapply(inp,class)                                                 # update
  } else {
    if(debug) message(fxNa,"No 'MArrayLM'-object; eximine ",length(inp)," elements (",pasteC(names(inp),quoteC="'"),")")
    if(is.list(inp) & length(inp)==1) {inp <- inp[[1]]; cheCla <- sapply(inp,class) }}
  ##
  if(debug) message(fxNa,"Examine ",length(inp)," elements (",pasteC(names(inp),quoteC="'"),")")
  if(any(sapply(inp,length) <1)) {                                             ## remove empty elements
    if(!silent) message(fxNa,"Remove empty element(s) : ",pasteC(names(inp)[which(sapply(inp,length) <1)],quoteC="'"))
    inp <- inp[which(sapply(inp,length) >0)]
    cheCla <- sapply(inp,class) }
  ## no names given : presume 'chInp' ...
  if(is.null(names(inp))) {
    names(inp) <- names(chInp)[1:length(inp)]
    txt <- "CAUTION: arguments are given without names, PRESUME same order of arguments as given in 'inputNamesLst' ! ie "
    if(!silent) message(fxNa,txt,pasteC(names(inp),quoteC="'")) }
  cheCla2 <- sapply(cheCla,paste,collapse=" ")
  chLis <- grep("list",cheCla2)
  if(length(chLis) >0) {                         # if lists (still) present in 'lst': try to bring elements searched down one level
    for(i in chLis) {               # select list elements potentially containing elements to extract
      useEl <- which( sapply(names(inp[[i]]) %in% chInp))
      ## note : bringing multiple elements dow one level (and elim initial)
      if(length(useEl) >0) {
        for(j in 1:length(useEl)) inp[[length(inp)+1]] <- inp[[i]][[j]]
        inp <- inp[-i] }}
    if(debug) message(fxNa,"revised (list) names(inp) ",pasteC(names(inp)))
  }
  ## check for 1st input element matching (Mval essential), in case of merge keep names of 1st (Mval)
  if(!any(names(inp) %in% chInp[[1]]))  stop(" essential argument group (for) '",names(chInp)[1],"' missing !")
  ##
  stdInpNa <- .convertNa(names(inp),chInp)                                      # standardized input-names
  ## remove non-name-conform list-groups
  if(any(is.na(stdInpNa))) {
    if(!silent) message(fxNa,"Remove non-recognized elements/arguments ",pasteC(names(inp)[which(is.na(stdInpNa))],quo="'"))
    inp <- inp[-1*which(is.na(stdInpNa))]
    stdInpNa <- .convertNa(names(inp),chInp)}
  ## check for arguments matching multiple times
  if(length(stdInpNa) > length(unique(stdInpNa))) {
    tmp <- table(names(which()))
    if(!silent) message(fxNa,"Arguments ",pasteC(names(inp)[which(tmp >1)],quoteC="'")," match multiple times in 'inputNamesLst', use only 1st per group")
    inp <- inp[firstOfRepeated(stdInpNa)$indUniq]                               # keep only 1st of mult
    stdInpNa <- .convertNa(names(inp),chInp) }
  ## transform all non-matrix to matrix
  chConDim <- sapply(inp,function(x) length(dim(x)) >1)    # check for matrix
  if(any(!chConDim)) for(i in which(!chConDim)) inp[[i]] <- matrix(inp[[i]],ncol=1,dimnames=list(names(inp[[i]]),names(chConDim)[i]))
  colNa <- lapply(inp,colnames)
  msg <- c("Potential trouble ahead, "," colnames !")
  if(any(nchar(unlist(colNa)) <1) & !silent) message(fxNa,msg[1],sum(nchar(unlist(colNa)) <1)," missing",msg[2])
  n2 <- c(length(unique(unlist(colNa))), length(unlist(colNa)))
  if(n2[2] > n2[1] & !silent) message(fxNa,msg[1], n2[2]-n2[1]," non-unique",msg[2])
  ## option: extract only last of multiple Cols (for p.value, ...)
  renameSingToStd <- TRUE
  lastOfMultCols <- names(inp)[which(stdInpNa %in% .convertNa(lastOfMultCols,chInp))]
  if(length(lastOfMultCols) >0) {
    if(!silent) message(fxNa,"Use only last column for elements ",pasteC(lastOfMultCols,quoteC="'"))
    for(i in which(names(inp) %in% lastOfMultCols)) { xx <- inp[[i]]
      inp[[i]] <- matrix(xx[,ncol(xx)],ncol=1,dimnames=list(rownames(xx),if(renameSingToStd) names(chInp)[i] else colnames(xx)[ncol(xx)]))}  # rename to standard name (defined in 'chInp')
    } else if(debug) message(fxNa,"Argument 'lastOfMultCols' has no matches to groups presented in 'inputNamesLst' !!")
  ## remove special characters from colnames (if reorderig at end of procedure)
  for(i in length(inp)) colnames(inp[[i]]) <- .replSpecChar(colnames(inp[[i]]),replBy="_")
  chInp <- lapply(chInp,.replSpecChar,replBy="_")
  ## check for conflicting (col)-names  -> rename by adding name of group + '.' + orig (col)name (for all cols of group)
  allColNa <- unlist(lapply(inp,colnames))
  if(length(unique(allColNa)) < length(allColNa)) {
    multNa <- names(table(allColNa))[which(table(allColNa)>1)]
    if(!silent) message(fxNa,"Problem with repeated (col)names ",pasteC(multNa,quoteC="'"),", need to rename (by adding group-name)!!")
    while(length(multNa) >0) {
      i <- multNa[1]
      tmp <- which(sapply(lapply(inp,colnames),function(x) i %in% x))
      tmp <- tmp[-1*which.min(sapply(inp[tmp],ncol))]
      for(j in tmp) colnames(inp[[j]]) <- paste(names(inp)[j],colnames(inp[[j]]),sep=".")     # main renaming of repeated colnames
      allColNa <- unlist(lapply(inp,colnames))                                                # need to update
      multNa <- names(table(allColNa))[which(table(allColNa)>1)]
    } }
  ##
  ##  ++  main joining  ++
  chNaLe <- sapply(inp,function(x) c(le=nrow(x),leNa=length(rownames(x)),nrNa=length(unique(rownames(x)))))       # each elem of 'inp' .. separ col
  chNaLe <- rbind(chNaLe,nFound=rep(NA,ncol(chNaLe)))
  for(i in 2:length(inp)) chNaLe[4,i] <- sum(rownames(inp[[i-1]]) %in% rownames(inp[[i]]))    # number of names matching to Mval
  ## look for unique (row)names
  useCol4na <- chNaLe[2,-1]==chNaLe[1,1] & chNaLe[3,-1] >1                    # same length and >1 non-red names
  if(sum(useCol4na) >1) useCol4na[-1*which.max(chNaLe[3,1+useCol4na])] <- FALSE   # for multiple potent names choose one with highest non-red
  nRedID <- if(chNaLe[1,1] >0) rownames(inp[[1]]) else {
    if(any(useCol4na)) rownames(inp[[which.max(useCol4na)+1]]) else 1:nrow(inp[[1]])}  # if names exist in Mval -> priority, else best of same le
  if(!is.null(nRedID)) nRedID <- treatTxtDuplicates(nRedID,sep=duplTxtSep,onlyCorrectToUnique=TRUE,callFrom=fxNa)          # warning if NULL
  ## check for identical rownames between elements to combine (cbind/merge)
  ## special case : search for annot (last element) with non-matching names but matching 1st col -> rename colnames of annot to allow simple merging
  compLastNa <- if(names(inp)[length(inp)] %in% chInp[[length(chInp)]]) {       # check if last of 'inp' in last group of 'chInp' present, ie annot
     if(!identical(rownames(inp)[[length(inp)]],rownames(inp[[1]])) & any(all(inp[[length(inp)]][,1]==rownames(inp[[1]])),all(
       inp[[length(inp)]][,1] ==gsub("\\+","; ",rownames(inp[[1]]))))) TRUE else FALSE} else FALSE
  if(compLastNa) rownames(inp[[length(inp)]]) <- rownames(inp[[1]])
  inpNa <- lapply(inp,rownames)
  chConNi <- cbind(idNa=sapply(inpNa[-1],function(x) all(x==inpNa[[1]])),
    noNaButEqLi=sapply(inp[-1], function(x) length(rownames(x)) <1 & nrow(x)==nrow(inp[[1]])),
    partNa=sapply(inpNa[-1],function(x) if(length(x)>0) sum(x %in% rownames(inp[[1]]),na.rm=TRUE) >0 else FALSE),
    hasNaButNoMatch=sapply(inpNa[-1],function(x) length(x) >0 & length(x)==nrow(inp[[1]]) & all(!x %in% inpNa[[1]])))
  if(any(chConNi[,2]) & !silent) message(fxNa,"No (row)names found in ",pasteC(names(inp)[which(chConNi[,2])+1],quoteC="'"),
    " but presume same as '",names(inp)[1],"'")
  if(any(chConNi[,4]) & noMatchPursue) {
    for(j in which(chConNi[,4])) rownames(inp[[j+1]]) <- rownames(inp[[1]])
    chConNi[which(chConNi[,4]),2] <- TRUE
    txt <- c("CAUTION: none of the (row)names in "," matching, since same length presume names of '")
    if(!silent) message(fxNa,txt[1],pasteC(names(inp)[chConNi[,4]],quoteC="'"),txt[2],names(inp)[1],"'")}
  if(debug) {cat("Counting of matching names etc 'chConNi' :\n"); print(chConNi); cat("\n")}
  chConNa <- chConNi[,1] | chConNi[,2] | chConNi[,3]                  # fuse for decision cbind : all names identical or (names absent & same nrow)
  ## remove elements wo names AND different length
  if(all(!chConNa)) stop(fxNa," No arguments left for merging !?!   (check input !!)")
  if(any(!chConNa)) {
    if(!silent) message(fxNa,"Cannot figure out how to join, OMIT element(s) ",pasteC(names(inp[1+which(!chConNa)]),quoteC="'"))
    inp <- inp[c(1,1+which(chConNa))]
    inpNa <- lapply(inp,rownames)
    chConNi <- cbind(idNa=sapply(inpNa[-1],function(x) identical(x,inpNa[[1]])),
      eqNumLines= sapply(inp[-1], function(x) length(rownames(x)) <1 & nrow(x)==nrow(inp[[1]])),
      eqNuLinesAndNoNames=rep(NA,length(inp[-1])),                                                    
      anyNa=sapply(inpNa[-1],function(x) if(length(x)>0) sum(x %in% rownames(inp[[1]]),na.rm=TRUE)>0 else FALSE),
      partNa=sapply(inpNa[-1],function(x) if(length(x)>0) sum(x %in% rownames(inp[[1]]),na.rm=TRUE) <nrow(inp[[1]]) else FALSE) )
    chConNi[,"eqNuLinesAndNoNames"] <- chConNi[,"eqNumLines"] & !chConNi[,"anyNa"]  
    chConNa <- chConNi[,1] | chConNi[,2] }
  out <- inp[[1]]
  ## main cbind
  chConNa <- chConNi[,1] | chConNi[,2]
  names(chConNa) <- rownames(chConNi)
  nCol <- sapply(inp,function(x) {dm <- dim(x); if(length(dm)>0) ncol(x) else 1})
  if(any(chConNa)) for(i in which(chConNa)) {                           # simple cbind when names are identical or all absent (&same length)
    tmp <- inp[[names(chConNa)[i]]]
    multCol <- FALSE
    out <- cbind(out,tmp)} 
  if(any(!chConNa)) message(fxNa,"Cannot use ",pasteC(names(chConNa)[which(!chConNa)],quoteC="'")," , different (partial?) matching than others !")
  newColNa <- if(standColNa) as.list(.convertNa(names(inp)[chConNa],chInp))  else as.list(names(inp)[chConNa])
  if(any(nCol[c(TRUE,chConNa)] >1)) for(i in which(nCol > 1 & c(TRUE,chConNa))) newColNa[[i]] <- paste(newColNa[[i]],colnames(inp[[i]]),sep=".")   # colnames for multi-col input
  colnames(out) <- unlist(newColNa) 
  ## (could not handle as cbind ->) have to combine via merge-like :
  if(any(!chConNa)) for(i in which(!chConNa)) {                                        # names do differ -> do some sort of 'merge'
    msg <- c(" .. for case of (partially) different ","(row)names ","in '","")
    useEl <- which(names(inp)==names(chConNa[i]))
    if(!silent) message(fxNa,"loop ",i,msg[1:3],names(inp)[useEl],"', prepare for merging")
    tmp <- rownames(inp[[useEl]])
    toMerNa <- rownames(inp[[useEl]])      # at this level all elements should have names
    ## nonunique rownames/IDs : if  in place, merge does all combinations, if used as NULL -> warning message when creating toMer;
    ##  if set how to re-integrate ??
    rownames(inp[[useEl]]) <- 1:nrow(inp[[useEl]])                                   # avoid warning in case of non-unique IDs !!
    toMer <- data.frame(id=toMerNa,inp[[useEl]],stringsAsFactors=FALSE)
    if(selMerg) {
      if(is.null(rownames(inp[[useEl]]))) {if(nrow(toMer)==nrow(out)) { toMer[,1] <- rownames(out)
        } else stop("problem with missing names in '",names(inp)[useEl],"', no ",useEl," and different nrow !!")}
      dim0 <- dim(out)
      out <- cbind(out,matrix(NA,nrow=nrow(out),ncol=ncol(toMer)-1,dimnames=list(NULL,colnames(toMer)[-1])))
      mat <- match(rownames(out),toMer[,1])
      n1 <- c(length(unique(naOmit(mat))),length(naOmit(mat)))
      msg <- c(" : multiple assoc for "," element(s) ","(don't know which one to choose !) ;  ","from ref not found")
      if(n1[1] <nrow(toMer) & !silent) message(fxNa," ",names(chConNa[i]),msg[1],n1[2]-n1[1],msg[2:3], sum(is.na(mat)),msg[c(2,4)])
      out[which(!is.na(mat)),(dim0[2]+1):ncol(out)] <- as.matrix(toMer[naOmit(mat),-1])
    } else {
      ## note when using merge() : does all combinations ! (ie output will have more lines than input !!), may give warning message
      out <- merge(data.frame(id=rownames(out),out),toMer,by="id",all.x=TRUE) }
  }
  ## put in order matching chInp:
  ## OK with single occurance per set of names
  tmp <- .convertNa(names(inp),chInp)
  goodOrd <- 1:ncol(out)
  if(length(tmp) <ncol(out)){ goodOrd <- naOmit(match(names(chInp),tmp))
  } else if(!silent) message(fxNa,"Can't adjust order of cols i output (since some elements have multiple columns)")
  ##
  goodOrd <- if(length(tmp) <ncol(out)) 1:ncol(out) else naOmit(match(names(chInp),tmp))   # if single column entries, set in order of chInp
  out <- out[,goodOrd]                                    # since merging non-complete elements at end changed order ...
  if(convertDF) out <- convMatr2df(out,duplTxtSep=duplTxtSep)                   # convert matrix to df
  if(is.data.frame(out)) for(i in ncol(out)) {if(is.character(out[,i])) out[,i] <- convToNum(out[,i],remove=NULL,sciIncl=TRUE)}          # keep text entries (eg annotation columns !)
  if("filt" %in% names(chInp)) {                          # convert any col type filt to logical
    useCol <- match(chInp[["filt"]],colnames(out))
    if(sum(!is.na(useCol)) >0) {
      tmp <- as.logical(out[,naOmit(useCol)[1]])
      if(sum(is.na(tmp)) <1) out[,naOmit(useCol)[1]] <- tmp else message(
        fxNa," cannot figure out how to convert '",colnames(out)[naOmit(useCol)[1]],"' to logical !") }}
  out }
  
#' @export
.convertNa <- function(query,ref,partMatch=TRUE){
  ## convert/standardize names of 'query' to standard names from 'ref' (list of possible names (char vect) where names define standardized name)
  ## take 'query' as character vector and return character vecor (same length as 'query') with 'converted/corrected' names
  ## names that are not found in 'ref' will be returned as NA
  ## 'ref' .. list of multiple possible names associated to given group, reference name for each group is name of list
  ## 'partMatch' allows partial matching (ie name of 'ref' must be in head of 'query')
  if(!partMatch) { out <- apply(sapply(ref,function(x) query %in% x),1,function(y) if(any(y)) which.max(y) else NA)                    # perfect match
    out[!is.na(out)] <- names(ref)[out[!is.na(out)]]
  } else {
    se <- sapply(ref,function(x) {unique(unlist(sapply(paste("^",x,sep=""),grep,query)))})
    out <- rep(NA,length(query))
     #cat(" pos ",unlist(se),"  insert ",rep(names(se),sapply(se,length)),"\n")
    out[unlist(se)] <- rep(names(se),sapply(se,length)) }  #unlist(se)  
  out }
                  
