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
#' ## scale for each column individually
#' dat2 <- apply(dat, 2, scaleXY, 1, 100)
#' range(dat2)
#' summary(dat2)
#' @export
scaleXY <- function(x, min=0, max=1) {       ## adjust values to range form min to max
  y <- .scale01(x)
  y*(max-min) + min}

#' @export
.scale01 <- function(x){low <- min(x, na.rm=TRUE); (x - low)/(max(x, na.rm=TRUE) - low)}     ## adjust values to range form 0 to 1

#' @export
.scaleXY <- function(x, minim=2, maxim=3) {       ## adjust values to range form min to max
  y <- .scale01(x)
  y*(maxim-minim) +minim}

