#' Combine Vectors From List And Return Basic Count Statistics
#'
#' The aim of this function is to choose a fixed number (\code{nCombin}) of list-elments from \code{lst} and count the number of common values/words.
#' Furthermore, one can define levels to fine-tune the types of combinations to examine. 
#' In case multiple combinations for a given level are possible, some basic summary statistics are provided, too
#' 
#' @details
#' When a given level apprears multiple tile, all possible combinations using one of the respective entries will be be made with the other levels.  
#' For exmaple, when you ask several students from city A abut only one student each from city B and city C to cyte their preferred hobbies, one could declare the cities as levels.
#' Then, all combinations of the students from city A with the students from city B and C will be made when counting the number of common hobbies (by either 2 or 3 students).
#' In such a case, all counting results will be summarized to the average count for the various categories (seen once, twice or 3 times...), 
#' sem (standard error of the mean) and CI (confidence interval), as well as sd.
#' 
#' With very long lists and high numbers of repeats of given levels, however, the computational effort incerases very much (like it does when using \code{table}).
#' 
#' @param lst (list of character or integer vectors) main input 
#' @param lev (character) define groups of \code{lst} 
#' @param nCombin (integer) number of list-elements to combine from \code{lst} 
#' @param remDouble (logical) remove intra-duplicates (defaults to \code{TRUE})  
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns an array with 3 dimensions : i)  ii) the combinations of \code{nCombin} list-elements, 
#'   iii) the number of counts (n), sem (standard error of the mean), CI (confidence interval) and sd  	
#' @seealso  \code{\link[base]{table}},  \code{\link[wrMisc]{replicateStructure}} 
#' @examples
#' ## all list-elements are considered equal
#' tm1 <- list(a1=LETTERS[1:17], a2=LETTERS[3:19], a3=LETTERS[6:20], a4=LETTERS[8:22])  
#' combineAsN(tm1, lev=gl(1,4))[,1,]
#' 
#' ## different levels/groups in list-elements
#' tm4 <- list(a1=LETTERS[1:15], a2=LETTERS[3:16], a3=LETTERS[6:17], a4=LETTERS[8:19], 
#'   b1=LETTERS[5:19], b2=LETTERS[7:20], b3=LETTERS[11:24], b4=LETTERS[13:25], c1=LETTERS[17:26],
#'   d1=LETTERS[4:12], d2=LETTERS[5:11], d3=LETTERS[6:12], e1=LETTERS[7:10])  # 
#' te4 <- combineAsN(tm4, nCombin=4, lev=substr(names(tm4),1,1))
#' str(te4)
#' te4[,,1]
#' @export
combineAsN <- function(lst, lev=NULL, nCombin=3, remDouble=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## move to wMisc  ??
  ## combine always 'nCombin' of the vectors in list 'lst' and search how may elemets are found on average, as total ('any'), single, double, triple or min2
  ## if multiple ways of making sets of 'nCombin' vetors exist, summary statistics will be added in 3rd dimension of resulting array
  fxNa <- wrMisc::.composeCallName(callFrom, newNa="combineAsN")
  reqPa <- c("utils","wrMisc")
  chPa <- sapply(reqPa, requireNamespace, quietly=TRUE)
  if(any(!chPa)) stop("package(s) '",paste(reqPa[which(!chPa)], collapse="','"),"' not found ! Please install first from CRAN")  
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(length(lev) <1) { if(length(names(lst)) >0) { lev <- substr(names(lst),1,1)
      if(!silent) message(fxNa,"Note : argument 'lev' is missing; trying to recuperate as 1st character of names of 'lst' :  ",wrMisc::pasteC(utils::head(lev,7),quoteC="'"), if(length(lev) >7) "...")
    } else stop("Argument 'lev' (for levels of list-elements) MUST be provided when 'lst' has no names !")} 

  if(length(lst) != length(lev)) stop("Length of 'lst' and 'lev' must match !")
  colNaSep <- c("_","-","\\.","+","__","--","\\+\\+")        # separators to test/for choice when combining
  if(length(lst) <2 | nCombin <2) { datOK <- FALSE; if(!silent) message(fxNa,"Nothing to do")
  } else datOK <- TRUE

  if(datOK & nCombin ==2 & remDouble) { remDouble <- FALSE
    if(debug) message(fxNa,"Setting 'nCombin=2' AND 'remDouble=TRUE' makes no sense (nothing will match), running as 'remDouble=FALSE' ")} 

  if(datOK) {
    ## MAIN, prepare matrix of combin
    levNo <- try(suppressWarnings(as.numeric(unique(lev))), silent=TRUE)
    if(inherits(levNo, "try-error") | all(is.na(levNo))) levNo <- unique(lev)
    ## Setup combinations to try
    combOfN <- utils::combn((1:length(lst)), nCombin)  # the combinations ..
    chSep <- sapply(colNaSep, function(x) length(grep(x, levNo)) >0)
    sep <- if(any(!chSep, na.rm=TRUE)) colNaSep[which(!chSep)[1]] else "_"
    colnames(combOfN) <- apply(combOfN, 2, function(x) paste(sort(lev[x]), collapse=sep))
    if(debug) {message(fxNa,"cA3a"); cA3a <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN,nCombin=nCombin)}   
    ## FILTER
    if(remDouble) {           # remove double-same but keep triple-same (or higher n-mers)  
      chD <- sort(unique(unlist(lapply(paste(unique(lev), unique(lev), sep="_"), grep, colnames(combOfN)))))
      chMono <- which(sapply(strsplit(colnames(combOfN),"_"), function(x) length(unique(x))) ==1)
      if(length(chD) >0) {
        chRem <- chD[which(!chD %in% chMono)]
        if(length(chRem) >0) { combOfN <- combOfN[,-1*chRem]
          if(debug) {message("cA3b"); cA3b <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN,chRem=chRem,chD=chD,chMono=chMono)} 
          if(debug) message(fxNa,"Removing ",sum(chRem)," poly-repeats but not auto-repeats (out of ",ncol(combOfN),
            ") combinations since argument 'remDouble=",remDouble,"'")}
      }
    }
    if(debug) {message(fxNa,"cA3c"); cA3c <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN)}
  
    ## main part, extract elements and count frequencies
    com3c <- apply(combOfN, 2, function(x) sortByNRepeated(lst[x], silent=silent, debug=debug, callFrom=fxNa))   # make list of number of times found: 1x, 2x, 3x ...  takes a few sec
    forceN <- 3
    if(debug) {message(fxNa,"cA3d"); cA3d <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN,com3c=com3c)}
    nRep <- 1:min(forceN, max(as.integer(unique( if(is.list(com3c)) unlist(lapply(com3c, names)) else com3c ))))
    nRep <- sort(unique(c(if(length(forceN) >0) forceN, unique(if(is.list(com3c)) as.integer(unlist(lapply(com3c, names))) else as.integer(com3c)) )))   # need to integrate 1, 2 & 3 (even if not in counts)
    tmp <- matrix(0, ncol=length(com3c), nrow=length(nRep), dimnames=list(nRep, names(com3c)))
    for(i in 1:length(com3c)) tmp[match(if(is.matrix(com3c[[i]])) colnames(com3c[[i]]) else names(com3c[[i]]), nRep), i] <- if(is.matrix(com3c[[i]])) nrow(com3c[[i]]) else sapply(com3c[[i]], length)
    ch123 <- 1:3 %in% rownames(tmp)
    if(any(ch123, na.rm=TRUE)) rownames(tmp)[which(ch123)] <- c("sing","doub","trip")[which(ch123)]
    if(nrow(tmp) >3) rownames(tmp)[4:nrow(tmp)] <- paste0("x",rownames(tmp)[4:nrow(tmp)])
    
    ## adhere to previous structure : sing, doub, min2, any (& rest)
    comCou2 <- rbind(tmp[1:3,], min2=colSums(tmp[1:2,]), any=colSums(tmp), if(nrow(tmp) >3) tmp[4:nrow(tmp),])
    if(nrow(tmp) >3) rownames(comCou2)[2+ (4:nrow(tmp))] <- rownames(tmp)[(4:nrow(tmp))]
    dupNa <- duplicated(colnames(comCou2), fromLast=FALSE)
    if(debug) {message(fxNa,"cA3e  Found ",sum(dupNa)," 'redundant'  out of ",length(dupNa)); cA3e <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN,com3c=com3c,comCou2=comCou2,dupNa=dupNa,nCombin=nCombin,tmp=tmp)}
  
    ## which contain repeated combinations of groups: calculate sd & derivatives, organize as array
    if(any(dupNa, na.rm=TRUE)) {
      faRep <- wrMisc::findRepeated(colnames(comCou2), nonRepeated=TRUE)
      ## statistics for those where replicates available
      tmp <- sapply(faRep$rep, function(x) {yy <- comCou2[,x] ; sdY <- apply(yy, 1, stats::sd, na.rm=TRUE)
        c(n=rowMeans(yy, na.rm=TRUE), sem=sdY/sqrt(ncol(yy)), CI=apply(yy, 1, confInt), sd=sdY) })
      if(debug) {message(fxNa,"cA3f"); cA3f <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN,com3c=com3c,comCou2=comCou2,dupNa=dupNa,faRep=faRep,tmp=tmp,faRep=faRep,nCombin=nCombin)}
      allNa <- unique(colnames(comCou2))  
      out <- array(NA, dim=c(nrow(comCou2), length(faRep$rep) +length(faRep$nonrep), 4), dimnames=list(rownames(comCou2), allNa,c("n","sem","CI","sd")))
      isMult <- match(names(faRep$rep), allNa)
      if(debug) {message(fxNa,"cA3g"); cA3g <- list(lst=lst,lev=lev,levNo=levNo,combOfN=combOfN,com3c=com3c,comCou2=comCou2,dupNa=dupNa,faRep=faRep,tmp=tmp,allNa=allNa,out=out,isMult=isMult,faRep=faRep,nCombin=nCombin)} 
      for(i in 1:4) out[,isMult,i] <- tmp[(i-1)*(nrow(tmp)/4) +(1:(nrow(tmp)/4)),]
      if(length(faRep$nonrep) >0) out[,match(colnames(comCou2)[faRep$nonrep], allNa),1] <- comCou2[,faRep$nonrep]   
    } else { 
      out <- array(c(comCou2, rep(NA, nrow(comCou2)*ncol(comCou2)*3)), dim=c(nrow(comCou2),ncol(comCou2),4), 
        dimnames=list(rownames(comCou2), colnames(comCou2),c("n","sem","CI","sd")))
    } 
  } else {
    ## single vector given, no combinations/repeats possible - return all as 'single' 
    out <- array(NA, dim=c(5,1,4), dimnames=list(c("sing","doub","trip","min2","any"),lev,c("n","sem","CI","sd")))
    out[c(1,5),1,1] <- length(lst[[1]])
  }
  out }
     
