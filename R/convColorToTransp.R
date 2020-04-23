#' Assign new transparency to given colors 
#'
#' This function alows (re-)defining a new transparency. A color encoding vector will be transformed to the same color(s) but with new transparency (alpha).   
#' @param color (character) color input
#' @param alph (numeric) transparency value (1 for no transparency, 0 for complete opaqueness), values <1 will be treated as percent-values
#' @return character vector (of same length as input) with color encoding for new transparency
#' @seealso \code{\link[grDevices]{rgb}}, \code{\link[graphics]{par}}
#' @examples
#' col0 <- c("#998FCC","#5AC3BA","#CBD34E","#FF7D73")
#' col1 <- convColorToTransp(col0,alph=0.7)
#' layout(1:2)
#' pie(rep(1,length(col0)),col=col0)
#' pie(rep(1,length(col1)),col=col1,main="new transparency")
#' @export
convColorToTransp <- function(color,alph=1){
  ## set given colors to specific transparency
  ## convert standard color vector to same color but with specific transparency 'alph'
  if(any(alph <1)) alph <- round(alph*100)
  col1 <- grDevices::col2rgb(color,alpha=TRUE)
  col1[4,] <- rep(alph,ncol(col1))[1:ncol(col1)]
  apply(col1,2,function(x) grDevices::rgb(x[1],x[2],x[3],alpha=x[4],maxColorValue=255)) }

#' @export
.convColorToTransp <- function(color,alph=1){
  .Deprecated("convColorToTransp")
  if(any(alph <1)) alph <- round(alph*100)
  col1 <- grDevices::col2rgb(color,alpha=TRUE)
  col1[4,] <- rep(alph,ncol(col1))[1:ncol(col1)]
  apply(col1,2,function(x) grDevices::rgb(x[1],x[2],x[3],alpha=x[4],maxColorValue=255)) }  
  
