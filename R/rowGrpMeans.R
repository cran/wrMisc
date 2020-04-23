#' rowMeans with destinction of groups (of columns, eg groups of replicates)
#'
#' \code{rowGrpMeans} calculates column-means for matrix with multiple groups of data, ie similar to rowMeans but one mean for each group of data.
#' Groups are specified as columns of 'x' in 'grp' (so length of grp should match number of columns of 'x', NAs are allowed).
#'
#' @param x matrix or data.frame
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates)
#' @return matrix with mean values
#' @seealso \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' head(rowGrpMeans(dat1,gr=gl(4,3,labels=LETTERS[1:4])[2:11]))
#' @export
rowGrpMeans <- function(x,grp){
  if(!is.matrix(x) & !is.data.frame(x)) stop(" 'x' should be data.frame or matrix")
  if(length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(length(grp) != ncol(x)) stop(" 'grp' should be of length of number of cols in 'x'")
  if(length(grp) <1 | sum(is.na(grp)) == length(grp)) stop(" 'grp' appears to be empty or all NAs")
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(!is.matrix(x)) x <- matrix(as.matrix(x),nrow=nrow(x),dimnames=dimnames(x))
  ## main
  .rowGrpMeans(x,grp) }
 
#' @export
.rowGrpMeans <- function(x,grp,na.replVa=NULL){
  ##  determine means of rows conditional as (multiple) groups
  ## NAs (eg from counting data) can be replaced by specified value 'na.replVa', eg 0)
  ## 'grp' expected as factor !!
  if(!is.null(na.replVa)) x[is.na(x)] <- na.replVa
  grNa <- unique(naOmit(as.character(grp)))
  out <- matrix(nrow=nrow(x),ncol=length(grNa),dimnames=list(rownames(x),grNa))
  for(i in 1:length(grNa)) {
    useC <- which(as.character(grp)==grNa[i])
    out[,i] <- if(length(useC) >1) base::rowMeans(x[,useC],na.rm=TRUE) else x[,useC] }
  out }
   
