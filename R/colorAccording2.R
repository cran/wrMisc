#' Transform numeric values to color-gradient
#'
#' This function helps making color-gradients for plotting a numerical variable.  
#' @param x (character) color input
#' @param gradTy (character) type of gradeint may be 'rainbow', 'heat.colors', 'terrain.colors', 'cm.colors', 'grey.colors'
#' @param nStartOmit (integer)  omit n steps from beginnin of gradient range
#' @param nEndOmit (integer)  omit n steps from end of gradient range
#' @param revCol (logical) reverse order
#' @param alpha (numeric) optional transparency value (1 for no transparency, 0 for complete opaqueness)
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return character vector (of same length as x) with color encoding
#' @seealso \code{\link[base]{cut}}
#' @examples
#' set.seed(2015); dat1 <- round(runif(15),2)
#' plot(1:15,dat1,pch=16,cex=2,col=colorAccording2(dat1))
#' plot(1:15,dat1,pch=16,cex=2,col=colorAccording2(dat1,nStartO=0,nEndO=4,revCol=TRUE))
#' plot(1:9,pch=3)
#' points(1:9,1:9,col=.transpGraySca(st=0,en=0.8,nSt=9,trans=0.3),cex=42,pch=16)
#' @export
colorAccording2 <- function(x,gradTy="rainbow",nStartOmit=NULL,nEndOmit=NULL,revCol=FALSE,alpha=1,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="colorAccording2")
  x <- convToNum(x,remove=NULL,sciIncl=TRUE,callFrom=fxNa)
  if(revCol) x <- -1*x
  ind <- round(1+(.scale01(x)*(sum(!is.na(x))-1)) )
  stFro <- if(is.null(nStartOmit)) 1 else nStartOmit+1        # + test/convert as integer
  upTo <- sum(!is.na(x)) +stFro -1
  nCol <- sum(!is.na(x)) +stFro-1 +if(is.null(nEndOmit)) 0 else nEndOmit   # + test/convert as integer
  switch(gradTy,
    rainbow=grDevices::rainbow(nCol,alpha)[stFro:upTo][ind],
    heat.colors=grDevices::heat.colors(nCol,alpha)[stFro:upTo][ind],
    terrain.colors=grDevices::terrain.colors(nCol,alpha)[stFro:upTo][ind],
    cm.colors=grDevices::cm.colors(nCol,alpha)[stFro:upTo][ind],
    gray.colors=grDevices::grey.colors(nCol,start=0.2,end=0.9,alpha=alpha)[stFro:upTo][ind],
      logGray=.transpGraySca(0,1,nSteps=sum(!is.na(x)),transp=alpha)[stFro:upTo][ind]
    ) }

#' @export
.transpGraySca <- function(startGray=0.2,endGrey=0.8,nSteps=5,transp=0.3){
  ## make single vector gray-gradient
  ## 'transp' should be numeric (betw 0 and 1) or NULL for no transparency (high val .. no transparency)
  ## default startGray (low) -> endGrey (high) goes from dense to light
  ## NOTE : color gradient NOT LINEAR to EYE !! use rather grey.colors
  grSeq <- seq(startGray,endGrey,len=nSteps)
  useCol <- grDevices::rgb(r=grSeq,g=grSeq,b=grSeq, alpha=if(is.null(transp)) 1 else transp)
  useCol }

#' @export
.convColorToTransp <- function(color,alph=1){
  ## set given colors to specific transparency
  ## convert standard color vector to same color but with specific transparency 'alph'
  if(any(alph <1)) alph <- round(alph*100)
  col1 <- grDevices::col2rgb(color,alpha=TRUE)
  col1[4,] <- rep(alph,ncol(col1))[1:ncol(col1)]
  apply(col1,2,function(x) grDevices::rgb(x[1],x[2],x[3],alpha=x[4],maxColorValue=255)) }
  #example# col0 <- rgb(0,1,1,alph=c(0.1,0.5,1))
  #.specColTransp(col0,al=0.8)

#' @export
.chooseGrpCol <- function(nGrp,paired=FALSE,alph=0.2) {
  ## automatic choice of colors: if single-> grey, if few -> RColorBrewer, if many : gradient green -> grey/red
  ## if max groups =8 (or max 2x6 paired groups) choose contrasting colors from RColorBrewer
  ## require(RColorBrewer)
  finCol <- grDevices::rgb(0.2,0.2,0.2,alpha=alph)                                  # dark grey
  if(nGrp >1 & nGrp < (9+4*paired)) {
    finCol <- if(paired) RColorBrewer::brewer.pal(12,"Paired")[c(5:6,1:4,7:12)] else RColorBrewer::brewer.pal(8,"Accent")[c(6,5,1:3,4,7:8)]   # red, blue,green,purple,orange,yellow,brown,grey
    if(nGrp < length(finCol)) finCol <- finCol[1:nGrp]
    if(alph <1 & alph >0) finCol <- .convColorToTransp(finCol,alph=alph)}
  if(nGrp > (8+4*paired)) {      # gradient bright green -> dark olive -> bright red
    finCol <- grDevices::rgb(seq(0,0.9,length.out=nGrp),seq(0.9,0,length.out=nGrp),0.2,alpha=alph)
  }
  finCol }
    