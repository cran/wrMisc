#' Make single vector gray-gradient
#'
#' This function helps making gray-gradients.
#' Note : The resulting color gradient does not seem linear to the human eye, you may try \code{\link[grDevices]{gray.colors}} instead
#' @param startGray (numeric) gray shade at start
#' @param endGrey (numeric)  gray shade at end 
#' @param nSteps (integer) number of levels
#' @param transp (numeric) transparency alpha
#' @return character vector (of same length as x) with color encoding
#' @seealso \code{\link[grDevices]{gray.colors}}
#' @examples
#' layout(1:2)
#' col1 <- transpGraySca(0.8,0.3,7,0.9) 
#' pie(rep(1,length(col1)), col=col1, main="from transpGraySca")
#' col2 <- gray.colors(7,0.9,0.3,alph=0.9)
#' pie(rep(1,length(col2)), col=col2, main="from gray.colors")
#' @export
transpGraySca <- function(startGray=0.2, endGrey=0.8, nSteps=5, transp=0.3){
  ## make single vector gray-gradient
  ## 'transp' should be numeric (betw 0 and 1) or NULL for no transparency (high val .. no transparency)
  ## default startGray (low) -> endGrey (high) goes from dense to light
  ## NOTE : color gradient NOT LINEAR to EYE !! use rather grey.colors
  grSeq <- seq(startGray, endGrey, len=nSteps)
  useCol <- grDevices::rgb(r=grSeq, g=grSeq, b=grSeq, alpha=if(is.null(transp)) 1 else transp)
  useCol }
