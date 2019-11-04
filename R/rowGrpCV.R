#' row group CV
#'
#' \code{rowGrpCV} calculates CVs for matrix with multiple groups of data, ie one CV for each group of data. 
#' Groups are specified as columns of 'x' in 'grp' (so length of grp should match number of columns of 'x', NAs are allowed)
#' @param x numeric matrix where relplicates are organized into separate columns
#' @param grp (factor) defining which columns should be grouped (considered as replicates)
#' @param means (numeric) alternative values instead of means by .rowGrpMeans()
#' @param listOutp (logical) if TRUE, provide output as list with $CV, $mean and $n
#' @return matrix of CV values
#' @seealso \code{\link{rowCVs}}, \code{\link{arrayCV}},  \code{\link{replPlateCV}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' head(rowGrpCV(dat1,gr=gl(4,3,labels=LETTERS[1:4])[2:11]))
#' @export
rowGrpCV <- function(x,grp,means=NULL,listOutp=FALSE){
  if(!is.matrix(x) & !is.data.frame(x)) stop(" 'x' should be data.frame or matrix")
  if(length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(length(grp) != ncol(x)) stop(" 'grp' should be of length of number of cols in 'x'")
  if(length(grp) <1 | sum(is.na(grp)) == length(grp)) stop(" 'grp' appears to be empty or all NAs")
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(is.null(means)) means <- .rowGrpMeans(x,grp)
  ## main
  out <- .rowGrpCV(x,grp,means)
  if(listOutp) out <- list(CV=out,mean=means,n=summary(grp))
  out }

#' @export
.rowGrpCV <- function(x,grp,means){
  .rowGrpSds(x,grp)/means
  }
    
