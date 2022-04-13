#' Extract Longest Common Text Out Of Character Vector
#'
#' @description 
#' This function allows recovering the single longest common text-fragments (from center, head or tail) out of character vector \code{txt}.
#' Only the first of all of the longest solutions will be returned.
#'
#' @details
#' Please note, that finding common parts between chains of characters is not a completely trivial task. 
#' This topic still has ongoing research for the application of sequence-alignments, where chains of characters to be compared get very long.
#' This function uses a k-mer inspirated approach.
#' The initial aim with this function was allowing to treat smaller chains of characters (and finding shorter strteches of common text), like eg with column-names.
#' 
#' Important : This function identifies only the first best hit, ie other shared/common character-chains of the same length will not be found !
#' 
#' Using the argument \code{hiResol=FALSE} it is possible to accelerate the search aprox 3x (with larger character-vectors), however, frequently the very best solution may not be found. 
#' This means, that in this case the result should rather be considered a 'seed', allowing check if further extension may improve the result, 
#' ie for identifying a (slightly) longer chain of common characters.
#' 
#' With longer vectors and longer character chains this may get demanding on computational reesources, the argument \code{hiResol=FALSE} allows reducing this at the price of missing the best solution.
#' With this argument single common/matching characters will not be searched if all text-elements are longer than 500 characters, an empty character vector will be returned. 
#' 
#' When argument \code{side} is either \code{left}, \code{right} or \code{terminal} only terminal common text may be found (a potentially even longer internal text will be lost).
#' Of course, choosing this option makes searches much faster.  
#' 
#' This function does not return the position of the shared/common characters within the text, you may use \code{gregexpr} or \code{regexec} to locate them.
#' 
#' 
#' @param txt character vector to be treated
#' @param minNchar (integer) minumin number of characters that must remain
#' @param side (character) may be be either 'center', 'any', 'terminal', 'left' or 'right'; only with \code{side='center'} or \code{'any'} internal text-segments may be found
#' 
#' @param hiResol (logical) find best solution, but at much higher comptational cost (eg 3x slower, however \code{hiResol=FALSE} rather finds anchor which may need to get extended)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a character vector of length=1, ie only one (normally the longest) common sequence of characters is identified.
#'  If nothing is found common/shared an empty character-vector is returned
#' @seealso Use \code{gregexpr} or \code{regexec} in \code{\link[base]{grep}} for locating the identified common characters in the initial query. 
#' @seealso Inverse : Trim redundant text (from either side) to keep only varaible part using \code{\link{trimRedundText}}; 
#'  you may also look for related functions in package \href{https://CRAN.R-project.org/package=stringr}{stringr}
#' @examples
#' txt1 <- c("abcd_abc_kjh", "bcd_abc123", "cd_abc_po")
#' keepCommonText(txt1, side="center")       # trim from right
#' 
#' txt2 <- c("ddd_ab","ddd_bcd","ddd_cde")
#' trimRedundText(txt2, side="left")          #  
#' keepCommonText(txt2, side="center")        # 
#' @export
keepCommonText <- function(txt, minNchar=1, side="center", hiResol=TRUE, silent=TRUE, callFrom=NULL, debug=FALSE) {
  ##
  ##
  fxNa <- .composeCallName(callFrom, newNa="keepCommonText")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isFALSE(hiResol)) hiResol <- TRUE
  side <- try(as.character(side), silent=TRUE)
  if(inherits(side, "try-error")) stop("Invalid entry for 'side', must be character, 'center','any', 'terminal', 'left' or 'right'")
  out <- lastOK <- NULL                      # initialize
  txOK <- TRUE
  ## check of input
  if(length(txt) <1 | all(is.na(txt))) { txOK <- FALSE; out <- NA
    if(!silent) message(fxNa,"Empty or all NA input, no common text possible")
  }
  if(txOK) {
    chNa <- is.na(txt)
    if(any(chNa)) txt <- txt[which(!chNa)]
    if(length(txt)==1) { out <- txt                          # length=1 .. full text
      if(debug) message(fxNa,"Single (valid) vector-element")}
    if(length(txt) >0 & length(out) <1) { chSame <- all(sapply(txt[-1], function(x) x==txt[1]))     # all the same no need for extensive testing
      if(all(chSame)) { out <- out[1]
        if(!silent) message(fxNa,"All text identical") } }
    nChar <- nchar(txt)
    if(length(out) <1 & any(nChar <1)) { out <- ""
      if(!silent) message(fxNa,"Some text elements are empty, no common text possible")}   
    if(debug) {message(fxNa,"kCT1"); kCT1 <- list(txt=txt,side=side,minNchar=minNchar,out=out,chSame=chSame,nChar=nChar,hiResol=hiResol)}  

    ## search at terminal positions
    if(length(txt) >0 & any(c("left","terminal") %in% side)) {
      txTr <- .trimRight(txt, minNchar=minNchar, silent=silent, callFrom=fxNa)
      if(nchar(txt[1]) > nchar(txTr[1])) { out <- txt <- sub(txTr[1],"",txt[1])
        nChar <- nchar(txt)}             # update
      if(debug) {message(fxNa,"kCT1a")}
    }
    if(length(txt) >0 & any(c("right","terminal") %in% side)) {
      txTr <- .trimLeft(txt, minNchar=minNchar, silent=silent, callFrom=fxNa)
      if(nchar(txt[1]) > nchar(txTr[1])) out <- sub(txTr[1],"",txt[1])    #
      nChar <- nchar(txt)                # update
    }
  }
  
  if(debug) {message(fxNa,"kCT2"); kCT2 <- list(txt=txt,side=side,minNchar=minNchar,out=out,chSame=chSame,nChar=nChar,hiResol=hiResol)}  
  
  ## search anywhere (incl center)
  if(length(txt) >0 & any(c("center","any") %in% side)) {
    ch1 <- min(nChar, na.rm=TRUE)
    if(ch1 -1 > minNchar) {                       # sufficient characters in all instances
      if(!hiResol & !silent) message(fxNa,"Please note that using the argument hiResol=FALSE, the optimal solution may not be found, you may check if the result can be further extended") 
      ch2 <- txt[which.min(nChar)]
      kMer <- 2
      kMerIni <- kMer    ## first round
      stSpl <- seq(1, ch1-kMer+1, by=if(hiResol) 1 else kMer)
      words <- unique(substr(rep(ch2, length(stSpl)), stSpl, stSpl+kMer-1))
      ch3 <- sapply(lapply(words, grep, txt[-which.min(nChar)]), length) == length(txt) -1
      lastOK <- if(any(ch3)) list(kMer=kMer,words=words, ch3=ch3) else NULL
      ## now searh for longer if any matches found above
      while(any(ch3) & kMer < ch1 -2) { kMer <- kMer +2
        stSpl <- seq(1, ch1-kMer+1, by=if(hiResol) 1 else kMer)
        words <- unique(substr(rep(ch2, length(stSpl)), stSpl, stSpl+kMer-1))
        if(debug) message(fxNa,"Increase kMer from ",kMer -2," to ",kMer,";  testing ",length(words)," character-chains")
        ch3 <- sapply(lapply(words, grep, txt[-which.min(nChar)]), length) == length(txt) -1
        if(any(ch3)) lastOK <- list(kMer=kMer,words=words, ch3=ch3)
      }
      if(debug) {message(fxNa,"kCT3"); kCT3 <- list(txt=txt,side=side,minNchar=minNchar,out=out,chSame=chSame,nChar=nChar,hiResol=hiResol,lastOK=lastOK, words=words,stSpl=stSpl,kMer=kMer,ch2=ch2,ch3=ch3)}  
      ## have reached too long words try step down, 1 less
      if(all(kMer > 1, !ch3, kMer > lastOK$kMer +1)) { 
        if(kMer==2) {    # nothing found at kMer=2, try split in single characters & look for common character
            if(minNchar==1 & any(all(nChar <500), hiResol)) {            # run only if hiResol=TRUE or all text entries are less than 500 chars
              txtSn <- lapply(strsplit(txt, ""), unique)
              ch3 <- table(unlist(txtSn))
              if(any(ch3==length(txt))) out <- names(ch3)[which(ch3==length(txt))[1]] }
        } else {
          kMer <- kMer -1
          stSpl <- seq(1, ch1 -kMer +1, by=if(hiResol) 1 else kMer)
          words <- unique(substr(rep(ch2, length(stSpl)), stSpl, stSpl+kMer-1))
          if(debug) message(fxNa,"Decrease kMer from ",kMer +1," to ",kMer,";  testing ",length(words)," character-chains")
          ch3 <- sapply(lapply(words, grep, txt[-which.min(nChar)]), length) == length(txt) -1
          if(any(ch3)) lastOK <- list(kMer=kMer,words=words, ch3=ch3)
        }
      }
      if(debug) {message(fxNa,"kCT4")}  
      if(length(lastOK) >0) out <- lastOK$words[which(lastOK$ch3)[1]]
    }
  }
  if(length(out) >0) out else ""}
  
