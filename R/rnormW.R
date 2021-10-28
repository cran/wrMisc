#' Normal random number generation with close fit to expected mean and sd
#'
#' This function allows creating a vector of random values similar to \code{rnorm}, but resulting value get recorrected to fit to expected mean and sd.
#' When the number of random values to generate is low, the mean and sd of the resultant values may deviate from the expected mean and sd when using the standard \code{rnorm} function.
#' In such cases the function \code{rnormW} helps getting much closer to the expected mean and sd.
#' 
#' @details
#' For making result reproducible, a seed for generating random numbers can be set via the argument \code{seed}.
#' However, with \code{n=2} the resulting values are 'fixed' since no random component is possible at n <3.
#' 
#' @param n (integer, length=1)  number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param mean (numeric, length=1) expected mean
#' @param sd (numeric, length=1) expected sd
#' @param seed (integer, length=1) seed for generating random numbers
#' @param digits (integer, length=1 or \code{NULL}) number of significant digits for output, set to \code{NULL} to get all digits   
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message produced
#' @return numeric vector of random values
#' @seealso \code{\link[stats]{Normal}} 
#' @examples
#' x1 <- (11:16)[-5]
#' mean(x1); sd(x1)
#' ## the standard way
#' ra1 <- rnorm(n=length(x1), mean=mean(x1), sd=sd(x1))
#' ## typically the random values deviate (slightly) from expected mean and sd
#' mean(ra1) -mean(x1) 
#' sd(ra1) -sd(x1)
#' ## random numbers with close fit to expected mean and sd :
#' ra2 <- rnormW(length(x1), mean(x1), sd(x1))
#' mean(ra2) -mean(x1) 
#' sd(ra2) -sd(x1)          # much closer to expected value
#' @export
rnormW <- function(n, mean=0, sd=1, seed=NULL, digits=8, silent=FALSE, callFrom=NULL) {
  ## reconstitute orginal values based on summary
  fxNa <- .composeCallName(callFrom, newNa="rnormW")
  if(!isTRUE(silent)) silent <- FALSE
  if(length(n) <1 | !is.numeric(n)) { out <- NULL; if(!silent) message(fxNa,"invalid 'n', returning NULL")
  } else { 
    if(length(n) >1) n <- length(n)
    msg <- c("argument '","' must be positive numeric and of length=1")
    if(!is.numeric(mean)) mean <- try(as.numeric(mean))
    if(length(mean) != 1 | "try-error" %in% class(mean)) stop(msg[1],"mean",msg[2])
    if(!is.numeric(sd)) sd <- try(as.numeric(sd))
    ## main
    if(n==1) {out <- mean
      if(isFALSE(silent) & is.finite(sd)) message(fxNa,"Ignoring 'sd' since n=1") 
    } else if(length(sd) != 1 | "try-error" %in% class(sd)) stop(msg[1],"sd",msg[2])
    if(n==2) out <- mean + c(-1,1)*sd/sqrt(2) 
    if(n >2) {
      if(length(seed) ==1) try(set.seed(seed), silent=TRUE)
      out <- if(length(digits)==1 & is.finite(digits)) signif(stats::rnorm(n=n, mean=mean, sd=sd), digits) else stats::rnorm(n=n, mean=mean, sd=sd)
      out <- out - mean(out)            # set meant to 0
      out <- mean + out*sd/sd(out)      # adjust sd and reset mean
  }}
  out }
   
