#' rowSums with destinction of groups (of columns, eg groups of replicates)
#'
#' This function calculates column-sums for matrix with multiple groups of data, ie similar to \code{rowSums} but one summed value for each line and group of data.
#' Groups are specified as columns of 'x' in 'grp' (so length of grp should match number of columns of 'x', NAs are allowed).
#'
#' @param x matrix or data.frame
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates)
#' @param na.rm (logical) a logical value indicating whether \code{NA}-values should be stripped before the computation proceeds.
#' @return This function a matrix with sum values
#' @seealso \code{\link{rowGrpMeans}}, \code{\link{rowGrpSds}}, \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' head(rowGrpMeans(dat1, gr=gl(4, 3, labels=LETTERS[1:4])[2:11]))
#' @export
rowGrpSums <- function(x, grp, na.rm=TRUE){
  if(!is.matrix(x) & !is.data.frame(x)) stop(" 'x' should be data.frame or matrix")
  if(length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(length(grp) != ncol(x)) stop(" 'grp' should be of length of number of cols in 'x'")
  if(length(grp) <1 | sum(is.na(grp)) == length(grp)) stop(" 'grp' appears to be empty or all NAs")
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(!is.matrix(x)) x <- matrix(as.matrix(x), nrow=nrow(x), dimnames=if(length(dim(x)) >1) dimnames(x) else list(names(x),NULL))
  if(length(na.rm) !=1 | !is.logical(na.rm)) na.rm <- TRUE
  ## main
  out <- .rowGrpSums(x, grp, na.rm=na.rm) 
  chNan <- is.nan(out)
  if(any(chNan)) out[which(chNan)] <- NA
  out }
 
#' @export
.rowGrpSums <- function(x,grp, na.replVa=NULL, na.rm=TRUE){
  ## determine sums of rows conditional as (multiple) groups
  ## NAs (eg from counting data) can be replaced by specified value 'na.replVa', eg 0)
  ## 'grp' expected as factor !!
  if(!is.null(na.replVa)) x[is.na(x)] <- na.replVa
  grNa <- unique(naOmit(as.character(grp)))
  out <- matrix(nrow=nrow(x), ncol=length(grNa), dimnames=list(rownames(x),grNa))
  for(i in 1:length(grNa)) {
    useC <- which(as.character(grp)==grNa[i])
    out[,i] <- if(length(useC) >1) base::rowSums(x[,useC], na.rm=na.rm) else x[,useC] }
  out }
   
