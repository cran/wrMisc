#' Scale data to given minimum and maxiumum
#'
#' This is a convenient way to scale data to given minimum and maxiumum without full standarization, ie without deviding by the sd. 
#' 
#' @param x (numeric) vector to rescacle
#' @param min (numeric) minimum value in output 
#' @param max (numeric) maximum value in output  
#' @return vector of rescaled data (in dimensions as input)
#' @seealso  \code{\link[base]{scale}} 
#' @examples
#' dat <- matrix(2*round(runif(100),2), ncol=4)
#' range(dat)
#' dat1 <- scaleXY(dat, 1,100)
#' range(dat1)
#' summary(dat1)
#' 
#' ## scale for each column individually
#' dat2 <- apply(dat, 2, scaleXY, 1, 100)
#' range(dat2)
#' summary(dat2)
#' @export
scaleXY <- function(x, min=0, max=1) {       ## adjust values to range form min to max
  y <- .scale01(x)
  y*(max-min) + min}

#' Scale between 0 and 1 (main)
#'
#' This function rescales between 0 and 1
#' 
#' @param x numeric vector to be re-scalded
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
#' @param x numeric vector to be re-scalded
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

