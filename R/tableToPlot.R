#' Print matrix-content as plot
#'
#' \code{tableToPlot} prints all columns of matrix in plotting region for easier inclusion to reports (default values are set to work for output as A4-sized pdf).
#' This function was made for integrating listings of text to graphical output to devices like \code{png}, \code{jpeg} or \code{pdf}. 
#' This function was initially designed for listings with small/medium 1st col (eg couner or index), 2nd & 3rd col small and long 3rd col (like file paths).
#' Obviously, the final number of lines one can pack and still read correctly into the graphical output depends on the size of the device 
#' (on a pdf of size A4 one can pack up to apr. 11O lines). 
#' Of ourse, \code{\link[utils]{Sweave}}, combined with LaTeX, provides a powerful alternative for wrapping text to pdf-output (and further combining text and graphics).
#' Note: The final result on pdf devices may vary depending on screen-size (ie with of current device), the parameters 'colPos' and 'titOffS' may need some refinements.
#' Note: In view of typical page/figure layouts like A4, the plotting region will be split to avoid too wide spacing between rows with less than 30 rows. 
#' @param matr (matrix) main (character) matrix to display
#' @param colPos (numeric) postion of columns on x-scale (from 0 to 1)
#' @param useCex (numeric) cex expension factor forsiez of text (may be different for each column)
#' @param useAdj (numeric) left/cneter/right alignment for text (may be different for each column)
#' @param useCol color specification for text (may be different for each column)
#' @param titOffS (numeric) offset for title line (ralive to 'colPos')
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return NULL (no R-object returned), print 'plot' in current device only
#' @seealso \code{\link[utils]{Sweave}} for more flexible framework
#' @importFrom graphics par
#' @examples
#' ## as example let's make a listing of file-names and associated parameters in current directory
#' mat <- dir()
#' mat <- cbind(no=1:length(mat),fileName=mat,mode=file.mode(mat),
#'   si=round(file.size(mat)/1024),path=getwd())
#' ## Now, we wrap all text into a figure (which could be saved as jpg, pdf etc)
#' tableToPlot(mat[,-1],colPos=c(0.01,0.4,0.46,0.6),titOffS=c(0.05,-0.03,-0.01,0.06))
#' tableToPlot(mat,colPos=c(0,0.16,0.36,0.42,0.75),useAdj=0.5,titOffS=c(-0.01,0,-0.01,0,-0.1)) 
#' @export
tableToPlot <- function(matr,colPos=c(0.05,0.35,0.41,0.56),useCex=0.7,useAdj=c(0,1,1,0),titOffS=0,useCol=1,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="tableToPlot")
  opar <- list(mar=graphics::par("mar"))
  on.exit(graphics::par(opar))
  msg <- " requires matrix (or data.frame) with >0 rows and 2-7 columns"
  msg2 <- " number of columns of 'matr' doesn't match with number of elements in 'colPos'"
  if(length(dim(matr)) <2) stop(msg)
  if(is.data.frame(matr)) matr <- as.matrix(matr)
  if(nrow(matr) <1) stop("nothing to display", msg)
  if(ncol(matr) >7 | ncol(matr) <2) stop(msg)
  if(length(colPos) != ncol(matr)) stop(msg2) 
  if(length(titOffS) < ncol(matr)) titOffS <- c(titOffS,rep(0,ncol(matr)-length(titOffS)))
  if(length(useAdj) < ncol(matr)) useAdj <- c(useAdj,rep(0.5,ncol(matr)-length(useAdj)))
  if(length(useCex) < ncol(matr)) useCex <- rep(useCex,ceiling(ncol(matr)/length(useCex)))
  if(length(useCol) < ncol(matr)) useCol <- rep(useCol,ceiling(ncol(matr)/length(useCol)))
  matr <- matr[nrow(matr):1,]            # reverse order since plot starts at bottom
  graphics::par(mar=c(1,1,1,1))
  graphics::plot.new()
  if(nrow(matr) <30) graphics::layout(matrix(1:2), heights=c(nrow(matr)/2,1))
  j <- 1:nrow(matr) +0.1                                               # y-position in plot
  graphics::plot.window(xlim=c(0,1),ylim=c(1,nrow(matr)+1.7))
  titPos <- colPos + titOffS
  ## add col-heads
  graphics::text(titPos,nrow(matr)+1,colnames(matr),cex=sort(c(0.5,max(useCex,na.rm=TRUE)+0.1,1.2))[2])  
  for(i in 1:ncol(matr)) graphics::text(colPos[i],j,matr[,i],cex=useCex[i],col=useCol[i],adj=useAdj[i])
}
   
