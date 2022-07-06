#' Make a list of common occurances sorted by number of repeats  
#'
#' The aim of this function is to count the number of occurances of words when comaring separate vectors (\code{x}, \code{y} and \code{z}) or from a list (given as \code{x})
#' and to give an output sorted by their frequency.
#' The output lists the various values/words by their frequency, the names of the resulting list-elements indicate number of times the values/words were found repeated.	
#' 
#' @details
#' 
#' In order to compare the frquency of values/words between separate vectors or vectors within a list, it is necessary that these have been made unique before calling this function or using \code{filterIntraRep=TRUE}. 
#' 
#' In case the input is given as list (in \code{x}), there is no restriction to the number of vectors to be compared. 
#' With very long lists, however, the computational effort incerases (like it does when using \code{table})
#' 
#' @param x (list, character or integer) main input, if list, arguments \code{y} and \code{z} will not be used 
#' @param y (character or integer) supplemental vector to comare with \code{x} 
#' @param z (character or integer) supplemental vector to comare with \code{x} 
#' @param filterIntraRep (logical) allow making vectors \code{x}, \code{y} and \code{z} unique before comparing (defaults to \code{TRUE})  
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list sorted by number of occurances. The names of the list indicate the number of repeats. 	
#' @seealso  \code{\link[base]{table}},  \code{\link[wrMisc]{replicateStructure}} 
#' @examples
#' sortByNRepeated(x=LETTERS[1:11], y=LETTERS[3:13], z=LETTERS[6:12])
#' sortByNRepeated(x=LETTERS[1:11], y=LETTERS[c(3:13,5:4)], z=LETTERS[6:12])
#' 
#' @export
sortByNRepeated <- function(x, y=NULL, z=NULL, filterIntraRep=TRUE, silent=TRUE, debug=FALSE, callFrom=NULL) {
  ## make list of common occurances sorted by number of repeats based on list of character-entries/words (eg peptide sequ) or up to 3 separate character vectors
  ## the name of output indicates number onf times all elements of the vector are repeated
  ## for 4 sets of data provide 'x' in form of list conatining all data
  fxNa <- .composeCallName(callFrom, newNa="sortByNRepeated")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(x) >1 & is.list(x)) { 
    if(length(y) >0) x[length(x) +1] <- unlist(y)
    if(length(z) >0) x[length(x) +1] <- unlist(z)
  } else {x <- list(x=x, y=y, z=z); rm(y,z)}
  chLe <- sapply(x, length) >0
  if(debug) { message(fxNa,"sBYN1   list-elements >0 ",pasteC(chLe)); sBYN1 <- list(x=x,chLe=chLe)}

  ## remove empty list-elements
  if(!all(!chLe) & any(!chLe)) { x <- x[which(chLe)]
    if(debug) message(fxNa,"Removing ",sum(!chLe)," empty entries (out of ",length(x),")")
    chLe <- sapply(x, length) >0 }
  if(debug) {message(fxNa,"sBYN2   length of list-elements >0 ",pasteC(sapply(x,length))); sBYN2 <- list(x=x,chLe=chLe)}
  if(length(x) <2) { if(!silent) message(fxNa,"Only ",length(x)," set(s) of data provided", if(length(x) <1 | filterIntraRep)" nothing to do !") 
    if(length(x) ==1) {             ## simple case : single vector to treat
      chDu <- duplicated(x[[1]]) 
      if(any(chDu) & filterIntraRep) x[[1]] <- unique(naOmit(x[[1]]))
      out2 <- table(table(x[[1]]))
      out <- rep(0, max(as.integer(names(out2))))
      out[match(as.integer(names(out2)), 1:max(as.integer(names(out2))) )] <- out2
      if(length(out) >1) {
        out <- c(out[1:3], min2=sum(out[2:3],na.rm=TRUE), any=sum(out), if(length(out) >3) out[4:length(out)])
      } else out <- c(out,0,0,0,out)
      out <- array(c(out, rep(NA, length(out)*3)), dim=c(length(out),1,4), dimnames=list(
        c("sing","doub","trip","min2","any", if(length(out) >5) paste0("x",4:(length(out) +2))),
        names(x[[1]]), c("n","sem","CI","sd")) )                                      
    } else out <- NULL
  } else { 
    ## check/remove for internal repeats
    if(filterIntraRep) { 
      chDu <- lapply(x, duplicated)
      chDu2 <- sapply(chDu, any)
      if(any(chDu2)) { if(debug) message(fxNa,sum(chDu2)," list elements contain (intra-) duplicated elements, need to remove for correct inter-comparison")
        for(i in which(chDu2)) x[[i]] <- x[[i]][which(!chDu[[i]])] }
    }
    if(debug) {message(fxNa,"sBYN3   length of list-elements ",pasteC(sapply(x,length))); sBYN3 <- list(x=x,chLe=chLe)}
  
    ## sort according to level of duplication
    x <- table(unlist(x, use.names=FALSE))
    ta2 <- table(x)
    out <- sapply(names(ta2), function(y) names(x)[which(x==as.integer(y))]) } 
  out }      
   
