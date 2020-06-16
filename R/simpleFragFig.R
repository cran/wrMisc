#' Simple figure showing line from start- to end-sites of edges (or fragments) defined by their start- and end-sites
#'  
#' \code{simpleFragFig} draws figure showing start- and end-sites of edges (or fragments) 
#'
#' @param frag (matrix) 2 columns defining begin- and end-sites (as interger values)
#' @param fullSize (integer) optional max size used for figure (x-axis)
#' @param sortByHead (logical) sort by begin-sites (if \code{TRUE}) or sort by end-sites
#' @param useTit (character) custom title
#' @param useCol (character) specify colors, if numeric vector will be onsidered as score values
#' @param displNa (character) display names of edges (figure may get crowded)
#' @param useCex (numeric) expansion factor, see also \code{\link[graphics]{par}}
#' @return matrix with mean values
#' @seealso \code{\link{buildTree}}, \code{\link{countSameStartEnd}}, \code{\link{contribToContigPerFrag}}, 
#' @examples
#' frag2 <- cbind(beg=c(2,3,7,13,13,15,7,9,7, 3,7,5,7,3),end=c(6,12,8,18,20,20,19,12,12, 4,12,7,12,4)) 
#' rownames(frag2) <- c("A","E","B","C","D","F","H","G","I", "J","K","L","M","N")
#' simpleFragFig(frag2,fullSize=21,sortByHead=TRUE) 
#' buildTree(frag2)
#' @export
simpleFragFig <- function(frag,fullSize=NULL,sortByHead=TRUE,useTit=NULL,useCol=NULL,displNa=TRUE,useCex=0.7){
  useColumn <- c(1,2)
  opar <- list(yaxt=graphics::par("yaxt"))
  on.exit(graphics::par(opar))
  fra <- frag[,useColumn]
  if("score" %in% colnames(fra)) useCol <- fra[,"score"]
  tmp <- table(as.numeric(fra))
  lin <- sort(unique(names(tmp)[which(tmp > stats::median(tmp,na.rm=TRUE))]))  
  ra <- range(as.numeric(fra),na.rm=TRUE)
  if(is.null(fullSize)) fullSize <- ra[2]
  if(is.numeric(useCol) & length(useCol)==nrow(fra)) {
    scoreV <- useCol
    col1 <- grDevices::rgb(red=c(179,131,135,203,253,244,255),green=c(170,204,221,211,174,109,0),blue=c(215,248,164,78,97,67,0),maxColorValue=255)        # pale purple, pale blue, 2x pale green, 2x orange to red
    useCol <- col1[as.numeric(cut(useCol,length(col1)))][]
    }  else scoreV <- NULL
  formDig <- function(x) sprintf(paste("%0",nchar(as.character(round(ra[2]))),"d",sep=""),x)
  reSort <- if(sortByHead) sort.list(paste(formDig(fra[,1]),formDig(fra[,2]))) else sort.list(paste(formDig(fra[,2]),formDig(fra[,1])),decreasing=TRUE)
  fra <- fra[reSort,]
  if(length(useCol) >1) useCol <- useCol[reSort]
  if(is.null(useCol)) useCol <- grDevices::grey(0.8)
  labOff <- round(ra[2]/40,1)  
  graphics::par(yaxt="n")                   
  graphics::plot(c(1,fullSize),c(1,nrow(fra)),type="n",main=useTit,xlab="fragment location",ylab="",las=1)
  if(length(lin) >0) graphics::abline(v=lin,lty=2,col=grDevices::grey(0.7))
  graphics::segments(x0=fra[,1],y0=1:nrow(fra),x1=fra[,2],y1=1:nrow(fra),lty=1,lwd=4,col=useCol)
  if(!is.null(rownames(fra)) & displNa) graphics::text(fra[,1]-labOff,1:nrow(fra),rownames(fra),cex=useCex)
  if(length(useCol) >1) {
    grpMean <- paste(">", signif(seq(min(scoreV),max(scoreV),length.out=2*length(col1)+1)[2*(1:length(col1))-1],3))
    graphics::legend("bottomright",legend=grpMean,text.col="black",fill=col1,cex=0.8,xjust=0.5,yjust=0.5)
    }
}
 
