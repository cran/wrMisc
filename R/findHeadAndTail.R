#' Find Group-Names In Pairwise Combined And Isolate Separator   
#'
#' @description
#' This function allows mapping which group-names are occuring either as head or as tail in pairwise combined terms (by checking from tails).
#' 
#' 
#' @details 
#' This function aims identifying the separator used wo create pairwise comparison labels by looking for given group-names at head and tail of combined labels.
#' If labeles occur inside oyther ones (eg 'AB' in 'ABC') it will always privilidge the longest one.
#' After stripping off all group-names the remaining charachers are considered as the separator used.
#' The function will also check if all pairwise labels returned the same separtator and in case of multiple separators found the most frequent may be reytained.
#' 
#'  
#'	 
#' @param grpNa (character) the names of the groups of replicates (ie conditions) used to test
#' @param pairwNa (character) the names of pairwise-testing (ie 'concatenated' \code{sampNa}
#' @param reportAs (character) default \code{'reportAs=list'} to return list, otherwise a character vector with separator(s) found will be returned 
#' @param sortGrp (logical) by default groups will be sorted (as levels of a factor appear sorted)
#' @param rmAmbig (logical) remove all ambiguous 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list containing $sep a character vector of the separator(s) identified 
#' @seealso (for running multiple pair-wise test) \code{\link{moderTestXgrp}}, used underneith \code{\link[base]{grep}}
#' @examples
#' findHeadAndTail(LETTERS[4:2], c("D-X","D-C"))
#' 
#' grp1 <- c("C","ACC","CC","B.B","B.BA","B","CA")
#' pwNa1 <- "B.B-CC" 
#' findHeadAndTail(grp1, pwNa1)
#' 
#' pwNa2 <- c("B.B-CC", "CA-ACC")
#' findHeadAndTail(grp1, pwNa2)
#' @export
findHeadAndTail <- function(grpNa, pairwNa, reportAs="list", sortGrp=TRUE, rmAmbig=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {   # trimming strategy !
  ## find which grpNa are occuring either as head or as tail in pairwNa  (by checking from tails)
  ## will find longest of grpNa as head or tail of pairwNa; 
  ## possible artifact : head & tail identified may be overlapping (will give warning)
  ## returns vector with index of grpNa (and grpNa as name); or NULL if not both head and tail identified
  ## NAs will be removed (changes length of grpNa )
  fxNa <- .composeCallName(callFrom, newNa="findHeadAndTail")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE

  if(any(is.na(pairwNa))) pairwNa <- wrMisc::naOmit(pairwNa)
  if(all(is.na(grpNa))) grpNa <- NULL
  datOK <- length(pairwNa) >0 && length(grpNa) >0
  if(datOK) { 
    if(isTRUE(sortGrp)) grpNa <- sort(grpNa)
    chDup <- duplicated(grpNa)
    if(any(chDup)) grpNa <- grpNa[which(!chDup)]
    grpNaIni <- grpNa    
    grpNaIni2 <- grpNa <- grpNa[order(nchar(grpNa), decreasing=TRUE)]
    grpNaIni3 <- grpNa <- wrMisc::protectSpecChar(grpNa)
    sep <- NULL    
    ## need to sort decreasing by length (no of characters) since some grpNa may be shorter version of others    
    ## main grep from ends
    indL <- paste0("^", grpNa)
    chL <- sapply(indL, grepl, pairwNa)
    indR <- paste0(grpNa, "$")
    chR <- sapply(indR, grepl, pairwNa)
    names(chL) <- names(chR) <- grpNaIni2
    if(debug) {message("fHT1"); fHT1 <- list(grpNa=grpNa,pairwNa=pairwNa,chL=chL,chR=chR,grpNaIni=grpNaIni,grpNaIni2=grpNaIni2,grpNaIni3=grpNaIni3)}
    if(any(chL) && any(chR)) {
      if(length(pairwNa)==1) {
        out <- c(which(chL)[1], which(chR)[1])         
        if(any(c("list","index","all") %in% reportAs)) {    
          outNa <- names(out)
          outI <- match(names(out), grpNaIni)
          names(outI) <- outNa
          if(sum(nchar(outNa)) > nchar(pairwNa)) { warning(fxNa,"Overlapping instance reported", if(!isTRUE(rmAmbig)) " : Reported results may be likely incorrect")
            if(isTRUE(rmAmbig)) {out[2] <- outI[2] <- sep <- NA}  } 
          if(debug) message("fHT1b"); fHT1b <- list(out=out, grpNa=grpNa,pairwNa=pairwNa,chL=chL,chR=chR,grpNaIni=grpNaIni,grpNaIni2=grpNaIni2,grpNaIni3=grpNaIni3)
          if("list" %in% reportAs && !all(is.na(out))) { if(debug) message(fxNa,"Start substr ",nchar(names(out[1])) +1,"  end ", nchar(pairwNa) - nchar(names(out[2])))
             sep <- substr(pairwNa, nchar(names(out[1])) +1, nchar(pairwNa) - nchar(names(out[2])))          
          }
          if(debug) {message("fHT1c"); fHT1c <- list(out=out,outI=outI,sep=sep,grpNa=grpNa,pairwNa=pairwNa,chL=chL,chR=chR,grpNaIni=grpNaIni,grpNaIni2=grpNaIni2,grpNaIni3=grpNaIni3)}
        } else {
          out <- names(out)
          if(sum(nchar(out)) > nchar(pairwNa)) warning(fxNa,"Overlapping instance reported")
        }                                          
      } else {
        ## pairwNa is longer                             
        out <- cbind(head=apply(chL, 1, function(x) which(x)[1]), tail=apply(chR, 1, function(x) which(x)[1]))  # index to grpNaIni2 and NOT to grpNaIni
        if(debug) {message("fHT2"); fHT2 <- list(out=out,grpNa=grpNa,pairwNa=pairwNa,chL=chL,chR=chR,grpNaIni=grpNaIni,grpNaIni2=grpNaIni2,grpNaIni3=grpNaIni3)}
        rownames(out) <- pairwNa
        ## main localization for longer pairwNa (as char)
        out <- matrix(grpNaIni2[match(grpNa[out], grpNaIni3)], ncol=2, dimnames=dimnames(out)) ## correct
        ## supl checks
        chLe <- rowSums(nchar(out))
        chLe1 <- chLe - nchar(pairwNa) 
        chLe2 <- chLe >= nchar(pairwNa)
        if(debug) { message("fHT3"); fHT3 <- list(out=out,chLe=chLe,chLe1=chLe1,chLe2=chLe2)}
        if(any(chLe2, na.rm=TRUE)) { warning(fxNa,sum(chLe2)," overlapping instances reported")
          if(isTRUE(rmAmbig)) { out[which(chLe2),] <- NA 
            pairwNa[which(chLe2)] <- NA
            chLe[which(chLe2)] <- NA
        } }
        ## isolate sep
        if(any(c("li","list","all") %in% reportAs)) {
          chSep <- nchar(pairwNa) -chLe 
          if(length(unique(wrMisc::naOmit(chSep))) >1) warning(fxNa,"It looks like finding non-unique separator")         
          sep <- unique(substr(pairwNa, nchar(out[,1]) +1, nchar(out[,1]) +0 +unique(chSep)))
          if(length(sep) >1) {          
            sep <- wrMisc::naOmit(unique(sub("^ +","", sub(" +$","", sep))) ) # trim heading or tailing spaces
          }
        } 
        ## located index
        if(any(c("ind","index","li","list","all") %in% reportAs)) {
          outI <- matrix(match(out, grpNaIni), ncol=2, dimnames=dimnames(out))
          if(debug) {message("fHT4"); fHT4 <- list(out=out,outI=outI,sep=sep,grpNa=grpNa,pairwNa=pairwNa,chL=chL,chR=chR,grpNaIni=grpNaIni,grpNaIni2=grpNaIni2,grpNaIni3=grpNaIni3)}
        } 
      }
      if("list" %in% reportAs) list(pwGrpNa=out, sep=sep, pwIndex=outI) else {if("index" %in% reportAs) outI else out } 
    } else NULL
  }
}
  
