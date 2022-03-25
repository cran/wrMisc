#' Difference in ppm between numeric values
#'
#'  This is a \code{diff()}-like function to return difference in ppm between subsequent values. 
#'  Result is oriented, ie neg ppm value means decrease (from higher to lower value). Note that if the absolute difference remains the same the difference in ppm will not remain same.
#'  Any difference to NA is returned as NA, thus a single NA will result in two NAs in output (unless NA is 1st or last).
#'
#' @param dat (numeric) vector for calculating difference to preceeding/following value in ppm
#' @param toPrev (logical) determine oriention
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a list with close matches of 'x' to given 'y', the numeric value dependes on 'sortMatch' (if FALSE then always value of 'y' otherwise of longest of x&y)
#' @seealso \code{\link{checkSimValueInSer}} and (from this package) \code{.compareByDiff},  \code{\link[base]{diff}} 
#' @examples
#' aa <- c(1000.01, 1000.02, 1000.05, 1000.08, 1000.09, 1000.08)
#' .compareByPPM(list(aa,aa), 30, TRUE)                    # tabular 'long' version
#' diffPPM(aa)
#' @export
diffPPM <- function(dat, toPrev=FALSE, silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="diffPPM")
  if(!is.numeric(dat)) dat <- try(as.numeric(if(is.factor(dat)) as.character(dat) else dat), silent=TRUE)
  if(inherits(dat, "try-error")) { if(!silent) message(fxNa,"Can't convert 'dat' to numeric !"); return(NULL)
  } else {
    rat <- if(toPrev) dat[-1]/dat[-length(dat)] else dat[-length(dat)]/dat[-1]
    (2*(dat[-1] > dat[-length(dat)])-1)*abs(rat -1)/1e-6}}
     
