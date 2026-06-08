#' Rescale Data To Given Minimum- And Maxiumum- Values
#'
#' This function allows convenient rescaling of data in the sence that the range covered will be adjusted to user-specified values.  
#' Instead of fixed min and max values for the output it is also possible to fix 2 given quantiles for user-specified values (and rescale by linear transformation).
#' 
#' @param x (numeric vector, matrix or data.frame) object to rescacle
#' @param min (numeric) minimum value in output, if of length=2 and max not given/valid its 2nd value will be used as max 
#' @param max (numeric) maximum value in output, defaults to 1 if not specified or use 2nd value of min (if min of length=2)  
#' @param q (numeric, length=2) vector giving quantile values to be used for setting frame of normalization (defaults to lowest and highest values expected)
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#'  
#' @return This function returns a vector (or matrix) of rescaled data (in dimensions of input)
#' @seealso  \code{\link[base]{scale}}, \code{\link{normalizeThis}},  \code{\link{blockNormalize}}
#' @examples
#' ## simple vector
#' scale(11:19)    # standardizes (base R), is not 'rescaling'
#' scaleXY(11:19)  # range form 0 to 1
#' scaleXY(11:19, min=1, max=10)
#' scaleXY(11:19, c(1, 10))          # min & max as single argument
#' # rescale matrix with NA
#' ## rescale to 20th and 80th quantile (ie other than min or max) 
#' scaleXY(11:19, min=1, max=10, q=c(0.2, 0.8))
#' 
#' ## rescale matrix with NA
#' mat1 <- matrix(11:20, ncol=2, dimnames=list(letters[1:5], c("A","B")))
#' mat1[2,1] <- NA
#' scaleXY(mat1)
#' scaleXY(mat1, min=1, max=10, q=c(0.2, 0.8))
#' 
#' ## rescale for each column individually
#' dat2 <- apply(mat1, 2, scaleXY, 1, 100)
#' range(dat2)
#' summary(dat2)
#'  
#' @export
scaleXY <- function(x, min=0, max=NULL, q=c(0, 1), silent=FALSE, debug=FALSE, callFrom=NULL) {
  stopifnot(is.numeric(x), length(x) !=0, !all(is.na(x)), length(min) %in% 1:2, all(is.finite(min)),
    is.finite(q), length(q)==2) 
  fxNa <- .composeCallName(callFrom, newNa="scaleXY")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE  
  if(length(min)==2 && (length(max)==0 || all(is.na(max)))) { max <- min[2]; min <- min[1]; if(debug) message(fxNa,"Assume min & max are given in 'min'")}
  ra <- if(length(min)==2 && (length(max)==0)) try(sort(min)) else try(sort(c(min[1], if(length(max)==1 && is.finite(max)) max else 1)), silent=TRUE)
  if(inherits(ra, "try-error")) {stop(fxNa,"Invalid entry for 'max' (or 'min' if you intended giving 2 numeric values for 'min')")}         
  if(any(duplicated(ra))) stop(fxNa,"Invalid entry for 'min' and 'max', values may not be identical !")

  ## main : Compute quantiles for input, linear transformation
  qVal <- stats::quantile(x, probs=q, na.rm=TRUE)
  slope <- (ra[2] - ra[1]) / (qVal[2] - qVal[1])
  intercept <- ra[1] - slope * qVal[1]
  xScaled <- slope * x + intercept
  if(length(dim(x))==0) xScaled else matrix(xScaled, nrow=nrow(x), dimnames=dimnames(x))
}  
  

#' Scale between 0 and 1 (main)
#'
#' This function rescales between 0 and 1
#' 
#' @param x numeric vector to be re-scaled
#' @return This function returns a numeric vector of same length with re-scaled values
#' @seealso \code{\link{scaleXY}} , \code{\link[base]{scale}} 
#' @examples
#' .scale01(11:15)
#' @export
.scale01 <- function(x){low <- min(x, na.rm=TRUE); (x - low)/(max(x, na.rm=TRUE) - low)}     ## adjust values to range form 0 to 1

#' Scale between min and max value (main)
#'
#' This function rescales between user-defined min and max values
#' 
#' @param x numeric vector to be re-scaled
#' @param minim (numeric) minimum value for resultant vactor
#' @param maxim (numeric) minimum value for resultant vactor
#' @return This function returns a matrix of CV values
#' @seealso \code{\link{scaleXY}} , \code{\link[base]{scale}} 
#' @examples
#' .scaleXY(11:15, min=1, max=100)
#' @export
.scaleXY <- function(x, minim=2, maxim=3) {       ## adjust values to range form min to max
  y <- .scale01(x)
  y*(maxim-minim) +minim}

