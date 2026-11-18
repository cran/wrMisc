#' Convert Character Vector Into Very Short Upper-Case 'Code-Names'
#' 
#' This function aims to convert all entries from a character vector into the shortest possible upper-case code-names.
#'  
#' @details
#' For vecors with equal or less than 26 entries this will result in single-letter 'code-names', for longer 2 letters will have to get used (tmax 676 combinations).
#' When all entries start with different letters, the starting letter(s) of each entry will be used, otherwise (conflicting cases with same startoing letters) 
#' other letters from the end of the alphabet will be used.
#' Duplicated text-entries will get the same abbreviation 'code-name'.
#' 
#' Note, all resultant text will be upper case, thus no distinction of initial lower- or upper-case letters can be made.
#' 
#' The function returns a character vector with shortened text (and initial text as names) 
#' 
#' @param txt (character or matrix of text) main input, text to be truncated
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return The function returns a character vector with shortened text (and initial text as names) 
#' @seealso \code{\link{truncateVect}} , \code{\link[base]{substr}} 
#' @examples
#' txt1 <- c("BB", "CC", "GHJ", "C3", "C3", "C3H", "cdf")
#' txt2 <- c("BB", "CC", "GHJ", paste0("C",1:26))
#' asShortNa(txt1[1:3])  # like substr()
#' asShortNa(txt1)
#' asShortNa(txt2)
#' 
#' @export
asShortNa <- function(txt, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## convert txt into very short upper 'code-names', if possible with same starting letter as initial (however, as caps), in case of multiple starting with same letter replace by other character taken from end of alphabet
  ## if less than 27 (different) elements reduce all to single letter, otherwise as 2-letter (caps) combinations (ie max 676)
  ## note  : NO upper/lower distinction
  ## returns vector with shortened text (and initial text as names) try as single/short names ( single letter/char  or double if not possible as single)
  ## move to wrMisc ? / integrate to wrMisc::truncateVect() ?
  fxNa <- .composeCallName(callFrom, newNa="asShortNa")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) { silent <- FALSE } else { debug <- FALSE }
    
  ## make unique
  chN <- duplicated(txt, fromLast=FALSE)
  if(any(chN)) { txtIni <- txt
    txt <-  txt[which(!chN)]       # make txt unique !
  } else txtIni <- NULL
  ## check if multiple with same start (ie short version)
  leNa <- 1 + (length(txt) >26)
  txtN <- toupper(substr(txt, 1, leNa))   # force to upper case
  chN <- duplicated(txtN, fromLast=FALSE)
  if(debug) {message(fxNa," aSN1")}
  if(any(chN)) {
    ## multiple with same start; need to create new short 'versions'
    hasDu <- chN | duplicated(txtN, fromLast=TRUE)
    iniDu <- txtN[which(hasDu)]
    useLe <- if(leNa == 1) LETTERS else paste0(rep(LETTERS, each=26), rep(LETTERS, 26))
    newLET <- !(useLe %in% txtN )        # which letters have (not) been used so far
    ## check if sufficient (single) letters avail
    if(sum(newLET) >= sum(hasDu) ) {
      txtN[which(hasDu)] <- rev(rev(useLe[which(newLET)]) [1:sum(hasDu)])
    } else {
      stop(fxNa,"UNABLE to replace names as short names")   
    }     
  }
  names(txtN) <- txt           
  if(is.null(txtIni)) txtN else {
    ## in case of replicated values in initial txt, expand to initial order&length
    txtN <- txtN[match(txtIni, txt)]
    names(txtN) <- txtIni
    txtN
  }                      
}

