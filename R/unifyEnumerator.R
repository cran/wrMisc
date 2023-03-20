#' Unify Enumerators
#' 
#' The aim of this function is to provide help in automatically harmonizing enumerators at the end of sample-names.
#' When data have same grouped setup/design, many times this is reflected in their names, eg 'A_sample1', 'A_sample2' and 'B_sample1'.
#' However, human operators may use multiple similar (but not identical) ways of expressing the same meanin, eg writng 'A_Samp_1'.
#' This function allows testing a panel of different extensions of enumerators and (if recognized) to replace them by a user-defined standard text/enumerator.
#' Please note that the more recent function \code{\link{rmEnumeratorName}} offers better/more flexible options.
#'
#' @details
#' This function has been developed for matching series of the same samples passing in parallel through different evaluation software (see R package wrProteo).
#' The way human operators may name things may easily leave room for surprises and this function allows testing only a limited number of common ways of writing.
#' Thus, in any case, the user is advised to inspect the results by eye and - if needed- to adjust the parameters.
#'
#' Basically enumerator separators can be constructed by combing a base-separator \code{baseSep} (like '-', '_' etc) and an enumerator-abbreviation \code{suplEnu}.
#' Then, all possible combinations will be tested if they occur in the text \code{x}.
#' Furthermore, the text searched has to be followd by on or multiple digts at the end of text-entry (decimal comma-separators etc are not allowed).
#' Thus, if there is other 'free text' following to the right after the enumerator-text this function will not find any enumerators to replace.
#'
#' The argument \code{stringentMatch} allows defining if this text has to be found in all text-entries of \code{x} or just one of them.
#' Whe using \code{stringentMatch=FALSE} there is risk that other text not meant to design enumerators may be picked up and modified.
#'
#' Please note, that with large data-sets (ie many columns) testing/checking a larger panel of enumerator-abreviations may result in slower performance.
#' In cases of larger data-sets it may be more effective to first study the data and then run simple subsitions using sub targeted for this very case.
#'
#' @param x (character) main input
#' @param refSep (character) separator for output
#' @param baseSep (character) basic seprators to test (you have to protect special characters)
#' @param suplEnu (character) additional text
#' @param stringentMatch (logical) decide if enumerator text has to be found in all instances or only once
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a character vector of same length as input \code{x}, with it's content as adjusted enumerators
#' @seealso \code{\link{rmEnumeratorName}} for better/more flexible options; \code{\link[base]{grep}} or \code{sub()}, etc if exact and consistent patterns are known
#' @examples
#' unifyEnumerator(c("ab-1","ab-2","c-3"))
#' unifyEnumerator(c("ab-R1","ab-R2","c-R3"))
#' unifyEnumerator(c("ab-1","c3-2","dR3"), strin=FALSE);
#'
#' @export
unifyEnumerator <- function(x, refSep="_", baseSep=c("\\-","\\ ","\\."), suplEnu=c("Repl","Rep","R","Number","No","Sample","Samp"), stringentMatch=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## unify Enumerators (towards end, must be followed by terminal digit) to refSep
  ## redSep (character, length=1)
  #example#
  fxNa <- .composeCallName(callFrom, newNa="unifyEnumerator")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(debug) {message(fxNa,"uE1")}

  ## need to filter as much as possible t keep combinatorics to check low  (init check without considering combination of baseSep, suplEnu and digits)
  chBa <- sapply(baseSep, function(y) length(grep(y, x)))               # check if they occur at all, no matter if with digit or not..
  chBa <- if(stringentMatch) chBa==length(x) else chBa >0
  chRe <- length(grep(refSep, x))                                                                                    # check if ref occur at all, no matter if with digit or not..
  chRe <- if(stringentMatch) chRe==length(x) else chRe >0
  if(debug) message(fxNa,"uE1b ",pasteC(chBa), "   chRe ",pasteC(chRe))
  if(any(chBa, chRe)) { baseSep <- if(any(chBa)) baseSep[which(chBa)] else NULL
    if(debug) {message(fxNa,"uE2") ; uE2 <- list(x=x,refSep=refSep,baseSep=baseSep,suplEnu=suplEnu,chBa=chBa,chRe=chRe)}
    ## test enumerators
    if(length(suplEnu) >0) {
      suplEnu <- union(suplEnu, tolower(suplEnu))
      chSup <- sapply(suplEnu, function(y) length(grep(y, x)))                                  # check if they occur at all, no matter if with digit or not..
      chSup <- if(stringentMatch) chSup==length(x) else chSup >0
      suplEnu <- if(any(chSup)) suplEnu[which(chSup)] else NULL }
    if(debug) {message(fxNa,"uE3") ; uE3 <- list(x=x,refSep=refSep,baseSep=baseSep,suplEnu=suplEnu,chBa=chBa,chRe=chRe)}
    if(length(suplEnu) >0) {
      ## construct 'complex' enumerator pattern (all combin of separator & enumerator)
      sep <- paste0(c( paste0(rep(baseSep, each=length(suplEnu)), suplEnu), paste0( suplEnu, rep(baseSep, each=length(suplEnu))),
        paste0(rep(baseSep, each=length(suplEnu)*length(baseSep)), rep(suplEnu,each=length(baseSep)), baseSep), suplEnu, baseSep ), "[[:digit:]]+$")
      chS <- sapply(sep, function(y) length(grep(y, x)))   # test which acually oocur
      chS <- if(stringentMatch) chS==length(x) else chS >0
      if(all(chS)) warning(fxNa,".. no pattern found, this should not happen here")
      if(debug) {message(fxNa,"uE3c") }
      sep <- sep[which(chS)]
    } else {  ## only basic enumerators occur, test which ones occur with digits
      baseSep <- paste0(baseSep,"[[:digit:]]+$")
      chBa <- sapply(baseSep, function(y) if(stringentMatch) length(grep(y, x)) == length(x) else length(grep(y, x)) >0)
      sep <- if(any(chBa)) baseSep[chBa] else NULL }
  } else sep <- NULL
  if(length(sep) >0) {
    if(debug) {message(fxNa,"uE4"); uE4 <- list(x=x,sep=sep,refSep=refSep,baseSep=baseSep,suplEnu=suplEnu,chBa=chBa,chRe=chRe)}
    for(i in sep) x <- sub(i, substr(i, 1, nchar(i) -13), x)
    x
  } else  {if(debug) message(fxNa,"uE4b"); x}    # no need to change, baseSep doesn't occur
}
   
