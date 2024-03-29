#' Transform (factor) levels into index
#'
#' This function helps transforming a numeric or character vector into indexes of levels (of its original values).
#' By default indexes are assigned by order of occurance, ie, the first value of \code{x} will be get the index of 1.
#' Using the argument \code{byOccurance=FALSE} the resultant indexes will follow the sorted values.
#'
#' @param dat (numeric or character vector or factor) main input 
#' @param byOccurance (logical) toogle if lowest index should be based on alphabetical order or on order of input
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return matrix with mean values
#' @seealso \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' x1 <- letters[rep(c(5,2:3),1:3)]
#' levIndex(x1)
#' levIndex(x1, byOccurance=FALSE)
#' ## with factor 
#' fa1 <- factor(letters[rep(c(5,2:3),1:3)], levels=letters[1:6])
#' levIndex(fa1)
#' levIndex(fa1, byOccurance=FALSE)
#' @export
levIndex <- function(dat, byOccurance=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## transform levels into index
  ## based on https://stackoverflow.com/questions/50898623/how-to-replace-multiple-values-at-once
  fxNa <- .composeCallName(callFrom, newNa="levIndex")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(length(dat) <1) { out <- NULL; if(!silent) message(fxNa, "'dat' seems to be empty")
  } else {
    out <- as.integer(as.factor(dat))
    names(out) <- dat
    if(isTRUE(byOccurance)) {
      levU <- naOmit(unique(out))           # levels in orig order (non-alpahbetical)    
      corsp <- data.frame(old=levU, new=1:length(levU))
      out[out %in% levU] <- (1:length(levU))[match(out, levU, nomatch = 0)] 
      names(out) <- dat } }
  out }  
  
