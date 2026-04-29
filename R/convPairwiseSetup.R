#' Check Comparison-Choice
#'
#' This function aims to help getting organized with pairwise (statistical) testing since there are multiple ways of expressing how samples 
#' should be grouped for tests. For example this may be either as combined character strings (eg 'A-B') or as separate elements. 
#' This function takes most input-formats and returns multiple converted formats.
#' 
#' @details
#' This function returns a list with $pwGrpNa (matrix with separated elements), $pwIndex (matrix with separated indexes), $grpNa (levels of \code{grp}, ie sorted),
#'  $sep (character vecor of separator found/used or suggested), $concat (pairwise concatenated names, eg 'A-B') 
#' 
#' @param useComparison (numeric or character vector or matrix) the argument allowing to specify the pairwise comparions 
#'   may be character as result of combining two group-names (from argument \code{grp}) (eg 'A-B', beware, the separator may not appear inside group-names) 
#'   or 2-column matrix of group-names (thus, without separator; names combined with separator may be shown as rownames) or 2-column matrix of indexes to sorted group-names;
#'   default setting of \code{useComparison='all'} will select all possible pairwise combinations
#' @param grp (character or factor)  vector defining groups of replicates for additional checking (may also be just the levels of grp)
#' @param experSetup (list) optional esperimental setup (list with $ind, $pwGrpIndex,$sep and $pwGrpNa  as obtained using \code{getPairwiseSetup})
#' @param sep (character) optional custom separator to use for splitting \code{useComparison}
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a list with $data, $nNA, $randParam, $NAneigLst, $seed, $filt, and $annot  
#' @seealso \code{\link{presenceFilt}}, \code{\link{getPWseparator}}, \code{\link{indexGroupsFromPW}}, \code{\link{getPairwiseSetup}}, \code{\link[base]{strsplit}}
#' @examples
#' convPairwiseSetup(c("B-C","D-A"), LETTERS[1:4])
#' convPairwiseSetup(c("B","C"), LETTERS[1:3])
#' convPairwiseSetup("all", LETTERS[1:3])
#' convPairwiseSetup(2:3, LETTERS[1:3])
#' convPairwiseSetup(2:3, LETTERS[1:3], sep="__")
#' convPairwiseSetup(matrix(c("B","D","C","A"), ncol=2), LETTERS[1:4])
#' 
#' @export
convPairwiseSetup <- function(useComparison, grp, experSetup=NULL, sep=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## useComparison (character or matrix) list/table of comparisons to be checked/mapped to grp
  ## experSetup (list with $pwGrpNa, $sep, $index, $grpNa) from getPairwiseSetup() ; old name checkUseComparison()
  ## return list with $pwGrpNa, $pwIndex, $grpNa, $sep  
  ## Note! estimation of 'grp' does NOT reflect number of replicateds neither positions
  fxNa <- .composeCallName(callFrom, newNa="convPairwiseSetup")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  pwComb <- grpNa <- concat <- pwIndex <- concatNa <- NULL
  #combineSep=NULL,
  ## use grp as redundant (length of ncol(dat))
  ## use grpNa as levels(grp)
  if(debug) {message(fxNa,"cUC1"); cUC1 <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa)}

  ## check if experSetup exists (result from getPairwiseSetup() - use to replace empty spots
  if(length(experSetup) !=0 && is.list(experSetup)) {
    if("sep" %in% names(experSetup)) sep <- experSetup$sep 
    if("grpNa" %in% names(experSetup)) grpNa <- experSetup$grpNa 
    if("grp" %in% names(experSetup) && length(grp)==0) { grp <- experSetup$grp
      if(length(grpNa)==0) grpNa <- sort(unique(grp)) else if(identical(unique(sort(as.character(grp))), as.character(grpNa))) warning(fxNa,"BEWARE 'grpNa' and 'grp' seem not to macth !!")}
    if("pwGrpNa" %in% names(experSetup)) pwGrpNa <- experSetup$pwGrpNa
    if("index" %in% names(experSetup)) index <- experSetup$index       ## ? needed ?
  }
  if(length(grp) >0) {
    grp <- as.factor(grp)
    grpNa <- levels(grp)
  }
  if(length(grpNa)==0) grpNa <- unique(sort(naOmit(grp)))
  if(length(grpNa)==0) warning(fxNa,"Potential problem with argument 'grp'  (all empty or NA ?), trying to find out (based on calssical separators, $grpNa possibly incomplete, $index possibly bad)")
  if(debug) {message(fxNa,"cUC2"); cUC2 <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa)}

  if(identical("all",useComparison)) {
    pwGrpNa <- t(utils::combn(grpNa, 2))
    useComparison <- pwIndex <- t(utils::combn(1:length(grpNa), 2))
    colnames(pwIndex) <- colnames(pwGrpNa) <- c("samp","ref")
    if(debug) message(fxNa,"Setting from useComparison='all' to ",length(grpNa)," specific names & indexes")
  }
  if(debug) {message(fxNa,"cUC2b"); cUC2b <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa)}

  ## determine sep
  if(length(sep) != 1) {
    ## extract if avail
    if(length(dim(useComparison)) <2 && !all(grepl("^[[:digit:]]+$", useComparison)) && all(nchar(useComparison) >2)) {
       ## assume as pairwise combined text : extract if avail
       sep <- if(length(sep)==0) suppressWarnings(getPWseparator(grp=grpNa, potSep="split1", silent=silent, debug=debug, callFrom=fxNa)) else {
	     suppressWarnings(getPWseparator(grp=grpNa, potSep=sep, silent=silent, debug=debug, callFrom=fxNa)) } 
    } else {
      if(length(grpNa) >0) {
        ## suggest based on grp
        sep <- if(length(sep)==0) getPWseparator(grp=grpNa, potSep="combine1", silent=silent, debug=debug, callFrom=fxNa) else {
		  getPWseparator(grp=grpNa, potSep=sep, silent=silent, debug=debug, callFrom=fxNa)}
      } else stop(fxNa,"Unable to determine or suggest 'sep' ")      
    }
  } else {
    if(grepl("(split)|(combine)[[:digit:]]{0,1}", sep))  sep <- suppressWarnings(getPWseparator(grp=grpNa, potSep=pwSeparatorList(type=sep), silent=silent, debug=debug, callFrom=fxNa))  
    #if(any(grepl("^split[[:digit:]]",sep), grepl("^combine[[:digit:]]",sep))) sep <- suppressWarnings(getPWseparator(grp=grpNa, potSep=pwSeparatorList(type=sep),silent=silent, debug=debug, callFrom=fxNa)) 
    if(length(sep)==0) {if(!silent) message(fxNa,"Encountered unknown 'sep'; setting to default")
      sep <- suppressWarnings(getPWseparator(grp=grpNa, potSep=pwSeparatorList(type="split1"), silent=silent, debug=debug, callFrom=fxNa)) 
    } #else sep <- suppressWarnings(getPWseparator(grp=grpNa, potSep=sep,silent=silent, debug=debug, callFrom=fxNa)) 
  }
  if(debug) {message(fxNa,"cUC3"); cUC3 <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa)}


  ## main checking/extracting of index & pwGrpNa
  ## check if at least 2 levels appear in useComparison;  convert all to matrix with index of levels (and useComparisonNa as matrix of names)
  if(length(useComparison)==0) {
    ## make 1st of comparisons
    if(debug) message(fxNa,"Note: argument 'useComparison' seems empty setting to 1st of comparisons   chUC3b" )
    if(length(grpNa)==0) stop(fxNa,"Ipmossible to get result since 'grp' is missing !!") else pwGrpNa <- grpNa[1:2]
    pwIndex <- 1:2
    useComparison <- paste(grpNa[1:2], collapse=sep)  # needed ??
  } else {
    ## may be char (rel to grp) OR matrix (index or char) 
    if(debug) message(fxNa," ..length useComparison : ",length(useComparison),"   cUC3b")
    chNum <- all(grepl("^[[:digit:]]+$", as.character(useComparison)), na.rm=TRUE)
    if(length(dim(useComparison))==2) {
      ## 2.0  matrix
      chNA <- is.na(useComparison)   #
      if(all(chNA) || length(useComparison)==0) stop(fxNa,"Removing NAs from (matrix-type) 'useComparison' - NOTHING REMAINS")
      if(any(chNA)) { useComparison <- useComparison[which(rowSums(chNA)==0),, drop=FALSE] 
        if(!silent) message(fxNa,"BEWARE, 'useComparison' contains NAs, removing ...")}
      if(debug) {message(fxNa,"cUC3c"); cUC3c <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, pwIndex=pwIndex, grpNa=grpNa)}
      
      if(isTRUE(chNum)) {             # presume as index
        ## 2.1 matrix of index
        if(all(as.integer(useComparison) >0) && all(as.integer(useComparison) >0)) {    # useComparison is matrix of index
          pwIndex <- useComparison
          if(length(grpNa) >0) {
            if(any(as.integer(useComparison) > length(grpNa), as.integer(useComparison) <1)) stop(fxNa,"Invalid entry for integer 'useComparison'")
          } else {
            if(!silent) message(fxNa,"NOTE : 'grp' still unknown, impossible to check range of integer 'useComparison'")
          }
          pwGrpNa <- cbind(samp=grpNa[pwIndex[,1]], ref=grpNa[pwIndex[,2]])
        } else warning(fxNa,"Cannot verify 'useComparison' as presumed integers")
      } else {
        ## 2.2 matrix of grpNa
        ## presume as matrix of character - ie names of levels, need to compare to grp (if knonn)
        if(length(grpNa) ==0) {
          warning(fxNa,"'grp' not known, very risky to try determining based on pw-names")         
          grpNa <- unique(sort(as.character(useComparison)))     ## ++ try to extract grpNa ++
        }  ## from now on assume grp as known
        if(debug) {message(fxNa,"cUC4"); cUC4 <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa)}
  
        pwGrpNa <- as.matrix(useComparison)
        pwIndex <- matrix(match(as.matrix(useComparison), grpNa), ncol=2)   # check if all exist
        chNA <- is.na(pwIndex)
        if(any(chNA)) { if(all(chNA)) stop(fxNa,"UNABLE to match 'useComparison' to 'grp' - nothing remains") else warning(fxNa,
          "BEWARE : Some comparisons do NOTNeed EXIST; need to eliminate ",sum(rowSums(chNA)==0)," comparison(s)")
          pwGrpNa <- pwGrpNa[which(rowSums(chNA)==0),,drop=FALSE]
          pwIndex <- pwIndex[which(rowSums(chNA)==0),,drop=FALSE]
        }
      }           # ... finished for matrix       
      if(debug) {message(fxNa,"cUC3d"); cUC3d <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, pwIndex=pwIndex, pwGrpNa=pwGrpNa, grpNa=grpNa)}
    } else {
      ## 3.0 (char) vector          
      ## is vector (not matrix ...)
      if(debug) {message(fxNa,"cUC4a"); cUC4a <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa,chNum=chNum)} # pwIndex=pwIndex, pwGrpNa=pwGrpNa, 
      if(any(is.na(useComparison))) useComparison <- naOmit(useComparison)
      if(length(useComparison) <1) stop(fxNa,"Removing NAs from (presumed text) 'useComparison' - NOTHING REMAINS")

      chGrp <- useComparison %in% grpNa
      if(length(grpNa)==0) grpNa <- naOmit(unique(sort(grp)))     # needed ??
      if(isTRUE(chNum) && all(as.integer(useComparison) >0) && !all(chGrp)) {   # vector of integer => index of comparison numbers
        if(length(useComparison) !=0) {                ## 3.0 vector of comparison numbers
          useComparison <- try(as.integer(useComparison), silent=TRUE)
          if(inherits(useComparison, "try-error")) {
            useComparison <- 1
            warning(fxNa,"Don't understand entry for 'useComparison', setting to default, ie 1st comparison ")
          }
          pwGrpNaAll <- t(utils::combn(grpNa, 2))
		  
          if(length(sep) != 1) { sep <- if(length(sep)==0) getPWseparator(grp=grpNa, potSep="combine1", silent=silent, debug=debug, callFrom=fxNa) else {               ## suggest based on grp
	    	    getPWseparator(grp=grpNa, potSep=sep, silent=silent, debug=debug, callFrom=fxNa)}}
          dimnames(pwGrpNaAll) <- list(paste0(pwGrpNaAll[,1], sep, pwGrpNaAll[,2]), c("samp","ref"))
          chUseComparison <- useComparison >0 & useComparison <= nrow(pwGrpNaAll) & !is.na(useComparison)
          if(any(!chUseComparison)) { if(all(!chUseComparison)) { 
              stop(fxNa,"Invalid entry for 'useComparison', should be matrix of group-names or number of all pairwise-comparisons")
            } else useComparison <- useComparison[which(chUseComparison)]}
          pwGrpNa <- pwGrpNaAll[useComparison,,drop=FALSE]
          pwIndex <- matrix(match(pwGrpNa, grpNa), ncol=2, dimnames=dimnames(pwGrpNa))
        }  #else {  ## 3.1 vector of indexes - not possible any more
        #}
      } else {           
        ## 3.2 (char) vector
        if(debug) {message(fxNa,"cUC4c  3.2 vector") }
        ## check if pair of grpNa (otherwise strsplit)
        if(all(chGrp, na.rm=TRUE)) {        # char vector of GrpNa
          ## 3.2.1 char vector of GrpNa 
          if(debug) {message(fxNa,"cUC4d  3.2.1 char vector of GrpNa") }
          pwGrpNa <- matrix(useComparison, byrow=TRUE, ncol=2)
          if(length(grpNa) !=0) pwIndex <- matrix(match(useComparison, grpNa), byrow=TRUE, ncol=2)           
        } else {     # concatenated : strsplit
          ## 3.2.2 char vector of concatenated : strsplit
          if(debug) {message(fxNa,"cUC4e  3.2.2 char vector of concatenated : strsplit"); cUC4e <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwComb=pwComb, grpNa=grpNa,chNum=chNum)} 
          nCha <- nchar(useComparison) >2
          if(any(!nCha, na.rm=TRUE)) stop(fxNa,"Unable to perform strsplit on 'useComparison' since ",sum(nCha, na.rm=TRUE)," element(s) are too short !!")
          if(length(sep)==0) {           # identify sep from  useComparison
            potSep <- pwSeparatorList("split1")
            spl1 <- lapply(protectSpecChar(potSep), function(x) strsplit(useComparison, x))          
            chLe <- sapply(spl1, sapply, length)
            use1 <- which.max(colSums(chLe==2))
            if(all(colSums(chLe !=2) !=0)) {
              msgE <- "Failed to extract 'grpNa' from 'useComparison', none of the potential separators allowed to split all 'useComparison' in 2 parts" 
              ch2 <- try(findHeadAndTail(grpNa=grpNa, pairwNa=useComparison, reportAs="list", silent=silent, debug=debug, callFrom=fxNa))
              if(inherits(ch2, "try-error") || length(ch2) !=1) message(fxNa,msgE) else {sep <- ch2$sep; pwGrpNa <- matrix(ch2$pwGrpNa) } 
            } else {
              sep <- potSep[use1]
              spl1 <- unlist(spl1[[use1]])
              pwGrpNa <- matrix(spl1, byrow=TRUE, ncol=2)              
              if(length(grpNa)==0) grpNa <- sort(unique(spl1))
            }
            pwIndex <- matrix(match(pwGrpNa, grpNa),  ncol=2) 
             
          } else {
            if(length(sep) >1) {warning(fxNa,"Truncating sep to length=1"); sep <- sep[1]}
            spl1 <- strsplit(useComparison, sep)
            ch1 <- sapply(spl1, length)
            if(any(ch1 !=2)) stop(fxNa,"User-provided 'sep' does NOT allow to split all 'useComparison' in exactely 2 parts")
            pwGrpNa <- matrix(unlist(spl1, use.names=FALSE), byrow=TRUE, ncol=2) 
            pwIndex <- matrix(match(pwGrpNa, grpNa), ncol=2 )           
          }          
        }      # finish split of char vector
        
      }        # finish char vector
    }          # finish vector
    rownames(pwGrpNa) <- rownames(pwIndex) <- concat <- paste0(pwGrpNa[,1], sep, pwGrpNa[,2])        

  }
  if(debug) {message(fxNa,"cUC6"); cUC6 <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwGrpNa=pwGrpNa, pwIndex=pwIndex, pwComb=pwComb, grpNa=grpNa)}
  if(length(pwGrpNa) >0 && length(dim(pwGrpNa)) !=2) pwGrpNa <- matrix(pwGrpNa, byrow=TRUE, ncol=2) 
  if(length(pwIndex) >0 && length(dim(pwIndex)) !=2) pwIndex <- matrix(pwIndex, byrow=TRUE, ncol=2)
  if(length(pwComb)==0 && length(pwGrpNa) !=0) pwComb <- paste0(pwGrpNa[,1], sep, pwGrpNa[,2])
  dimnames(pwGrpNa) <- dimnames(pwIndex) <- list(pwComb, c("samp","ref"))
  if(debug) {message(fxNa,"cUC7"); cUC7 <- list(useComparison=useComparison, grp=grp, experSetup=experSetup, sep=sep, pwGrpNa=pwGrpNa, pwIndex=pwIndex, pwComb=pwComb, grpNa=grpNa)}
  list(pwGrpNa=pwGrpNa, pwIndex=pwIndex, grpNa=grpNa, sep=sep, concat=pwComb)
} 
  
  
