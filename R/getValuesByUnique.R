#' Print matrix-content as plot
#'
#' When data have repeated elements (defined by names inside the vector), it may be advantageous to run some operations 
#' only on a unique set of the initial data, or somtimes all repeated occurances need to be replaced by a common (summarizing) value.
#' This function allows to re-introduce new values from on second vector with unique names, to return a final vector of initial input-length and order of names (elements) like initial, too.
#' Normally the user would provide 'datUniq' (without repeated names) containing new values which will be expanded to structure of 'dat', 
#' if 'datUniq' is not provided a vector with unique names will be made using the first occurance of repeated value(s).
#' For more complex cases the indexing relative to 'datUniq' can be returned (setting \code{asIndex=TRUE}).
#' Note: If not all names of 'dat' are found in 'datUniq' the missing spots will be returned as \code{NA}.
#' 
#' @param dat (numeric or character) main long input, must have names
#' @param datUniq (numeric or character) will be used to impose values on \code{dat}, must have names that should match names (at least partially) from \code{dat}
#' @param asIndex (logical) if \code{TRUE} index values will be returned instead of replacing values
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return vector of length \code{dat} with imposed values, or index values if \code{asIndex=TRUE}
#' @seealso \code{\link[base]{unique}}, \code{\link{findRepeated}}, \code{\link{correctToUnique}}, \code{\link{treatTxtDuplicates}}
#' @examples
#' dat <- 11:19
#' names(dat) <- letters[c(6:3,2:4,8,3)]
#' ## let's make a 'datUniq' with the mean of repeated values :
#' datUniq <- round(tapply(dat,names(dat),mean),1)
#' ## now propagate the mean values to the full vector
#' getValuesByUnique(dat,datUniq)
#' cbind(ini=dat,firstOfRep=getValuesByUnique(dat,datUniq),
#'   indexUniq=getValuesByUnique(dat,datUniq,asIn=TRUE))
#' @export
getValuesByUnique <- function(dat,datUniq=NULL,asIndex=FALSE,silent=FALSE,callFrom=NULL) {
  ## main
  fxNa <- .composeCallName(callFrom,newNa="getValuesByUnique")
  if(length(dat) <1 | is.null(names(dat))) stop("'dat' must be vector of length >0 with names; no names nothing to do")
  if(length(datUniq) >0) if(is.null(names(datUniq))) {
    if(!silent) message(fxNa," 'datUniq' has no names; unable to use ! (using default instead)")
    datUniq <- NULL
  }
  if(is.null(datUniq)) datUniq <- dat[!duplicated(names(dat))]
  if(identical(dat,datUniq)) {out <- if(asIndex) 1:length(dat) else dat
  } else {
    out <- rep(NA,length(dat))
    names(out) <- names(dat)
    ## note: this step performs much faster using match than via as.integer(as.factor) !
    ind <- match(names(dat),names(datUniq))
    if(!asIndex) out[which(!is.na(ind))] <- datUniq[naOmit(ind)] else out <- ind
    chNa <- is.na(out)
    if(any(chNa)) {
      if(!silent) message(fxNa," ",sum(chNa)," name(s) of 'dat' not found in 'datUniq' !")
    }
  }
  out}
     
