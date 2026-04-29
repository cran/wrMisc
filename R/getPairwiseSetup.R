#' Extract Pairwise Testing Setup  
#'
#' @description
#' This function allows extacting, checking and combinig elements defining labels for pairwise comparisons.
#' 
#'  
#'  
#' @details 
#' This function was designed to support functions exploiting pairwise testing results like plotting functions for Volcano-Plots etc.
#' 
#' Initially the function will check if the object given as argument \code{pwGrpNa} is an MAarrayLM-objects object and/or was created using \code{moderTestXgrp}. 
#' If yes, the function will try to extract names of groups, pairwise combinations and separator used from $setup, if present.
#' If no $setup found this function will check through several elements in MAarrayLM-objects to gather all elements needed.
#' 
#' 2) The separator \code{sep} is given and exact matches at both sides will be searched.
#' However, if the character(s) from \code{sep} do appear inside \code{pwGrpNa} no matches will be found.
#'
#' If some \code{pwGrpNa} are not found in \code{grpNa} this will be marked as NA.  
#' 
#' Finally, if no testing result is given as input, basic parameters group-names, pairwise names and the separator to be used can be combined into a list.
#' 
#' @param pwGrpNa (character) the names of pairwise-testing (ie 'concatenated' \code{pwGrpNa} using the separator \code{sep} ) 
#' @param grpNa (character) the names of the groups of replicates (ie conditions) used to test
#' @param sep (NULL or character, length=1) if not \code{NULL} the character-vector (of length=1) given will be used to split \code{grpNa} using \code{stringsplit()} 
#' @param combPwSep (character) sequence of separators for building combined names; may be combPwSep='combine1' or 'combine2'; will be used in pwSeparatorList()  
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list with $ind with matrix of indexes to (sorted) \code{pwGrpNa} for each pairwise comparison; 
#'   $pwGrpNames with matrix of \code{pwGrpNa} for each pairwise comparison;  
#'   $sep for the separator found ; $pwGrpNa which recapitulates (sorted) \code{pwGrpNa} as factor
#' @seealso (for running multiple pair-wise test) \code{\link{moderTestXgrp}}, \code{\link{convPairwiseSetup}}, \code{\link{presenceFilt}}, \code{\link{getPWseparator}}, \code{\link{indexGroupsFromPW}}, 
#'   (used underneith:) \code{\link[base]{grep}}, \code{\link[base]{strsplit}}
#' @examples
#' grp3 <- factor(rep(LETTERS[c(3,1,4)],c(2,3,3)))
#' set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4),2), ncol=8,
#'   dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste0(grp3, c(1:2,1:3,1:3))))
#' test1 <- moderTestXgrp(t8, grp3)  
#' getPairwiseSetup(test1)
#' 
#' 
#' 
#' @export
getPairwiseSetup <- function(pwGrpNa, grpNa=NULL, sep=NULL, combPwSep="combine1", silent=FALSE, debug=FALSE, callFrom=NULL) {
  ##  Understand pairwise experim setup - to be used for exploiting testing-results (eg Volcano-Plot, ...)
  ##
  fxNa <- .composeCallName(callFrom, newNa="getPairwiseSetup")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE

  if(all(is.na(pwGrpNa))) pwGrpNa <- NULL
  if(any(is.na(grpNa))) grpNa <- naOmit(grpNa)
  out <- useComparisonNa <- pwGrpNa1 <- grpNa1 <- NULL    # initialize
  singleCompSetup <- FALSE
  
  datOK <- length(pwGrpNa) >0 
  if(datOK) {
    ## pwGrpNa may also be obj from testing
    if(any(c("list","MArrayLM") %in% class(pwGrpNa))) {

      ## A) sep :  check for present predefined sep  (from wrProteo::testRobustToNAimputation() or wrMisc::moderTestXgrp()    
      ch1 <- length(pwGrpNa$grpSep) ==1      ## need to create $grpSep for  wrProteo::testRobustToNAimputation() or wrMisc::moderTestXgrp()
      ch2 <- length(pwGrpNa$setup$sep) ==1   ## from importFx
      ## other places to check ?
      if(any(ch1, ch2)) {
        sep1 <- if(ch1) pwGrpNa$grpSep else {if(ch2) pwGrpNa$setup$sep}
        if(length(sep) >0 && !identical(sep,sep1)) {message(fxNa,"Note : 'sep' given by user seems to  DIFFER  from 'sep' found in MArrayLM-object")}
        if(length(sep)==0) sep <- sep1
      }
      if(debug) {message(fxNa,"yy0"); yy0 <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep)}
      
      ## B) pwGrpNa1 : look for 2dim table of comparisons(names)  in  $setup$useComparisonNa
      if("pwGrpNa" %in% names(pwGrpNa) && length(pwGrpNa$pwGrpNa) >0) pwGrpNa1 <- pwGrpNa$pwGrpNa else {
        if("setup" %in% names(pwGrpNa) && "pwGrpNa" %in% names(pwGrpNa$setup) && length(pwGrpNa$setup$pwGrpNa) >0) pwGrpNa1 <- pwGrpNa$setup$pwGrpNa else {
          ## otherwise, try extracting colnames from testing
          ## B1) test for special case -  single comparison : may not have pw combin in colnames
          if("t" %in% names(pwGrpNa) && ncol(pwGrpNa$t)==2 && colnames(pwGrpNa)[1] =="(Intercept)") {
            ## single comparison case (colnames of pwGrpNa$design are fixed); determine optimal sep and create pairwise name(s)
            ## note that pwGrpNa$design is not reflecting sample-names as usual !!
            singleCompSetup <- TRUE
            if("means" %in% names(pwGrpNa)) sampGrNa <- colnames(pwGrpNa$means) else {
              warning(fxNa,"Unabale obtain names of groups of repliaces, assuming as '1' and '2'")
              sampGrNa <- 1:2
            }
            if(length(sep) !=1) sep <- getPWseparator(compNames=NULL, grp=sampGrNa, potSep=sep, includeGrp=FALSE, silent=silent, debug=debug, callFrom=fxNa)
            useGrp <- sampGrNa[which(colSums(pwGrpNa$design) >0)]
            if(length(useGrp) <2) stop(fxNa,"Unable to find out which sammples were compared in this single-comparison case !")
            if(length(useGrp) >2) warning(fxNa,"Having difficulty understanding SETUP of this single-comparison data ! .. possibly associating wrong labels to data ?")
            pwGrpNa1 <- paste(useGrp, collapse=sep)
          } else {
            ## B2, regular extraction of pwGrpNa1 (multiple pw comparisons)
            if("t" %in% names(pwGrpNa) && length(colnames(pwGrpNa$t)) >0) pwGrpNa1 <- colnames(pwGrpNa$t) else {
              if("p.value" %in% names(pwGrpNa) && length(colnames(pwGrpNa$p.value)) >0) pwGrpNa1 <- colnames(pwGrpNa$p.value) else {
                ## also check $design (would need to concatenate later) ? (gives no of samples & groups) note : $contrasts NOT present in result from wrMisc::moderTest2grp() !!
                warning(fxNa,"Unable to locate 'pwGrpNa' in input")
              }
            }  
          }
        }
      } 
      if(debug) {message(fxNa,"yy1"); yy1 <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep,pwGrpNa1=pwGrpNa1)}

      ## C) grpNa : look names of groups (grpNa)
      if("grpNa" %in% names(pwGrpNa) && length(pwGrpNa$grpNa) >0) grpNa1 <- pwGrpNa$grpNa else {
        if("setup" %in% names(pwGrpNa) && "grpNa" %in% names(pwGrpNa$setup) && length(pwGrpNa$setup$grpNa) >0) grpNa1 <- pwGrpNa$setup$grpNa else {
          if("design" %in% names(pwGrpNa) && length(pwGrpNa$design) >0) grpNa1 <- colnames(pwGrpNa$design) else {    #
            if("means" %in% names(pwGrpNa) && length(pwGrpNa$means) >0) grpNa1 <- colnames(pwGrpNa$means) else {
              warning(fxNa,"Unable to locate information for 'grpNa'") }
          }  
        }
      }
      if(length(grpNa1) >0 && length(grpNa) >0) {
        if(identical( unique(sort(as.character(grpNa1))), unique(sort(as.character(grpNa))))) grpNa <- grpNa1 else {
          if(!silent) message(fxNa,"Names of groups extracted from MArrayLM-object  DIFFERENT  to user provided 'grpNa', using initial")
        }
      } else if(length(grpNa1) >0) grpNa <- grpNa1
      if(length(grpNa)==0) warning(fxNa,"Unable to locate/extract 'grpNa' in input")

      ## D) useComparisonNa : recuperate useComparisonNa
      if(length(pwGrpNa$setup$useComparisonNa) >0) useComparisonNa <- pwGrpNa$setup$useComparisonNa     
      if(debug) {message(fxNa,"yy2"); yy2 <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep,pwGrpNa1=pwGrpNa1,useComparisonNa=useComparisonNa)}
      
      ## pwGrpNa D) recuperate /restore (core) pwGrpNa  (and dismiss other info attached)
      pwGrpNa <- pwGrpNa1             # dismiss rest of MArrayLM-object 

      if(length(sep)==0) sep <- getPWseparator(grp=pwGrpNa, potSep=pwSeparatorList(type=combPwSep, silent=silent, debug=debug, callFrom=fxNa), includeGrp=FALSE, silent=silent, debug=debug, callFrom=fxNa) 
      #if(length(sep)==0) sep <- getPWseparator(grp=pwGrpNa, potSep=pwSeparatorList(type="combine1"), includeGrp=FALSE, silent=silent, debug=debug, callFrom=fxNa) 
      if(debug) {message(fxNa,"Finished extracting MArrayLM obj   yy3"); yy3 <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep,pwGrpNa1=pwGrpNa1,useComparisonNa=useComparisonNa)}
      
      ## finished recuperating info from MArrayLM-obj
    } else { datOK <- length(grpNa) >0 }
    if(length(pwGrpNa) >0) grpNa <- unique(sort(grpNa)) else warning(fxNa,"Unable to isolate 'grpNa' from MArrayLM-object -  nothing to do  ")
    #pwGrpNa <- unique(sort(pwGrpNa))   #else warning(fxNa,"Found 'pwGrpNa' but seems to be of length=0 - nothing to do  ")
    if(debug) {message(fxNa,"yy4"); yy4 <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep)}
  }  

  if(datOK) { if(length(sep)==1 && nchar(sep) >0) {  
      ## OK for strspl (sep seems valid) 
      
      if(length(pwGrpNa)==0 && length(dim(pwGrpNa)) !=2) {
        ## suppose combined grp -vector given - need to split

      }
      
      if(length(grpNa) >0) {
        pwGrpNa2 <- if(length(dim(pwGrpNa))==2) apply(pwGrpNa, 1, paste, collapse=sep) else paste(pwGrpNa, collapse=sep)
        if(debug) {message(fxNa,"yy4b"); yy4b <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep,pwGrpNa2=pwGrpNa2)}
        
        out <- indexGroupsFromPW(compNames=pwGrpNa2, grp=grpNa, includeGrp=TRUE, potSep=if(length(sep) ==1) sep else NULL, silent=silent, debug=debug, callFrom=fxNa) # uses strspl ; grp may be NULL
        if(all(is.na(out$index)) && ncol(out$index) ==2) out$index <- matrix(1:2, ncol=2, dimnames=list(paste(out$pwGrpNa, collapse=out$sep), 1:2))
      }
      if(debug) {message(fxNa,"yy5"); yy5 <- list(pwGrpNa=pwGrpNa,grpNa=grpNa,sep=sep, out=out)}           
        
    } else {
      ## check for intra-repeating grp
      chIntra <- sapply(grpNa, function(x) grepl(x, pwGrpNa[-1*which(pwGrpNa == x)]))  #
      chIntra2 <- if(length(sep)==1) {any(chIntra) && any(grepl(sep, pwGrpNa))} else any(chIntra)
      if(chIntra2 && !silent) message(fxNa," ",sum(rowSums(chIntra) >0)," elements of grpNa appear inside others (- may pose problems identifying groups from tails")    
      ## try by trimming                                                                                                         
      out <- findHeadAndTail(grpNa, pwGrpNa, silent=silent, debug=debug, callFrom=fxNa)
      if(length(out) !=1) {               ## if failed so far, go for strspl
        out <- indexGroupsFromPW(grpNa, grp=grpNa, includeGrp=TRUE, silent=silent, debug=debug, callFrom=fxNa)   # uses strspl        
      }    
    } 
  }      
  out }                
       

