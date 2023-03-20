#' rowMeans with destinction of groups (of columns, eg groups of replicates)
#'
#' \code{rowGrpMeans} calculates column-means for matrix with multiple groups of data, ie similar to rowMeans but one mean for each group of data.
#' Groups are specified as columns of 'x' in 'grp' (so length of grp should match number of columns of 'x', NAs are allowed).
#'
#' @param x matrix or data.frame
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates)
#' @param na.rm (logical) a logical value indicating whether \code{NA}-values should be stripped before the computation proceeds.
#' @return matrix with mean values
#' @seealso \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' head(rowGrpMeans(dat1, gr=gl(4, 3, labels=LETTERS[1:4])[2:11]))
#' @export
rowGrpMeans <- function(x, grp, na.rm=TRUE){
  if(!is.matrix(x) && !is.data.frame(x)) stop(" 'x' should be data.frame or matrix")
  if(length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(length(grp) != ncol(x)) stop(" 'grp' should be of length of number of cols in 'x'")
  if(length(grp) <1 || all(is.na(grp))) stop(" 'grp' appears to be empty or all NAs")
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(!is.matrix(x)) x <- matrix(as.matrix(x), nrow=nrow(x), dimnames=if(length(dim(x)) >1) dimnames(x) else list(names(x),NULL))
  if(length(na.rm) !=1 || !is.logical(na.rm)) na.rm <- TRUE
  ## main
  out <- .rowGrpMeans(x, grp, na.rm=na.rm) 
  chNan <- is.nan(out)
  if(any(chNan)) out[which(chNan)] <- NA
  out }
 
#' row group mean (main)
#'
#' This function calculates CVs for matrix with multiple groups of data, ie one CV for each group of data. 
#' 
#' @param x numeric matrix where relplicates are organized into separate columns
#' @param grp (factor) defining which columns should be grouped (considered as replicates)
#' @param na.replVa (numeric) value to replace \code{NA} values
#' @param na.rm (logical) remove all \code{NA} values 
#' @return This function returns a matrix of mean values per row and group of replicates
#' @seealso \code{\link{rowGrpCV}}, \code{\link{rowCVs}}, \code{\link{arrayCV}}, \code{\link{replPlateCV}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' grp1 <- gl(4,3,labels=LETTERS[1:4])[2:11]
#' head(.rowGrpMeans(dat1, grp1))
#' @export
.rowGrpMeans <- function(x, grp, na.replVa=NULL, na.rm=TRUE){
  ## determine means of rows conditional as (multiple) groups
  ## NAs (eg from counting data) can be replaced by specified value 'na.replVa', eg 0)
  ## 'grp' expected as factor !!
  if(!is.null(na.replVa)) x[is.na(x)] <- na.replVa
  grNa <- unique(naOmit(as.character(grp)))
  out <- matrix(nrow=nrow(x), ncol=length(grNa), dimnames=list(rownames(x),grNa))
  for(i in 1:length(grNa)) {
    useC <- which(as.character(grp)==grNa[i])
    out[,i] <- if(length(useC) >1) base::rowMeans(x[,useC], na.rm=na.rm) else x[,useC] }
  out }
   
