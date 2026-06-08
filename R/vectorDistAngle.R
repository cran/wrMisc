#' @title Vector Distance and Angle
#'
#' @description
#' Compute the Euclidean distance and angle (in radians) for 2-dimensional vectors defined by
#' start and end points in the first and second columns of argumnents \code{x} and \code{y}.
#'
#' @details
#' The input  \code{x} and \code{y} is considered as describing start- and enc-points of vectors in two-dim space. 
#' Each row of x describes the starting and end-points of separate (and independent) vectors along the x- and y-axis.
#' 
#' Then, the function calculates for each line the **distance** as Euclidean distance 
#' and the **angle** as atan2(y_diff, x_diff)` in radians (range: `[-pi, pi]`), measured
#'   counterclockwise from the positive x-axis.
#' 
#' This function was designed for movement trajectories or any paired
#' coordinate data where direction and magnitude matter.  
#' If \code{x} and \code{y} contain more than 2 columns, these additional columns will be ignored.
#'
#' @param x (numeric matrix or data.frame of 2 column, min 2 rows) describes start & end x-coordinates
#' @param y (numeric matrix or data.frame of 2 column, min 2 rows) describes start & end x-coordinates
#' @param addCoord (logical) add original start and end-coordinates to output
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced).
#'
#' @return This function returns a data.frame with the columns:
#'   \item{`dist`}{Numeric vector of Euclidean distances}
#'   \item{`angle`}{Numeric vector of angles in radians}
#'   Optionally the original start and end-coordinates to output may get added if \code{addCoord=TRUE}
#' @seealso \code{\link[stats]{dist}},  \code{\link[base]{Trig}}
#' @examples
#' x <- matrix(c(0, 3, 0.5, 4, 1, 1), ncol=2, byrow=TRUE)  # x: (0,0)→(3,0), (0,0)→(4,0), (0,0)→(0,0)
#' y <- matrix(c(0, 0, 1, 3, 0.5, 5), ncol=2, byrow=TRUE)  # y: (0,0)→(0,0), (0,0)→(3,0), (0,0)→(5,0)
#' (vectDist <- vectorDistAngle(x, y))
#'
#' plot(cbind(x=as.numeric(x),y=as.numeric(y)), main="Example for vectorDistAngle()",las=1)
#' arrows(x[,1],y[,1],x[,2],y[,2])
#' text(x[,2], 0.1 +y[,2], paste0("dist=",round(vectDist[,1],2),
#'   ", angle=",round(vectDist[,2],2)), adj=1)
#'
#' @export
vectorDistAngle <- function(x, y, addCoord=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## express change from 1st to 2nd col of x & y as distance and angle of vector (alternative to correlation) for each pair of lines from x & y
  fxNa <- .composeCallName(callFrom, newNa="vectorDistAngle")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  datOK <- length(x) >0 && length(y) >0 && all(dim(x) > c(2,1)) && all(dim(y) > c(2,1))
  if(datOK) {
    rowNa <- if(!is.null(rownames(x))) rownames(x) else if(!is.null(rownames(y))) rownames(y)
    ## angle & dist
    dis <-  sqrt((x[,2] - x[,1])^2 + (y[,2] - y[,1])^2)
    ang <- atan2(x=(x[,2] - x[,1]), y= (y[,2] - y[,1]))
    out <- data.frame(dist=dis, angle=ang)
    if(!is.null(out)) rownames(out) <- rowNa
    if(isTRUE(addCoord)) out <- cbind(out, x1=x[,1], x2=x[,2], y1=y[,1], y2=y[,2])
    out
  } else if(!silent) message(fxNa,"Invalid input, expecting matrix or data.frame with 2 columns and at least 2 lines for x and y, returning NULL")
} 
  
  
