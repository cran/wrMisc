#' Per line and per group sd-values
#'
#' \code{rowGrpSds} calculate Sd (standard-deviation) for matrix with multiple groups of data, ie one sd for each group of data. 
#' Groups are specified as columns of 'x' in 'grp' (so length of grp should match number of columns of 'x', NAs are allowed).
#' @param x matrix where relplicates are organized into seprate columns
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates)
#' @return This function returns a matrix of sd values
#' @seealso  \code{\link{rowGrpMeans}}, \code{\link{rowCVs}}, \code{\link{rowSEMs}},\code{\link[stats]{sd}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' head(rowGrpSds(dat1, gr=gl(4,3,labels=LETTERS[1:4])[2:11]))
#' @export
rowGrpSds <- function(x,grp){
  ## return Sd for within-row subgroup of data 'x' (matrix or data.frame)
  if(is.null(ncol(x))) stop("data should be matrix or data.frame with multiple columns !") else {
    if(ncol(x) < 2) stop("data should be matrix or data.frame with at least 2 columns !")}
  if(length(grp) !=ncol(x)) stop("length of (factor) 'grp' and number of cols in 'x' don't match !")
  .rowGrpSds(x,grp) }

#' @export
.rowGrpSds <- function(x,grp){
  ##
  ## return (entire col of) NA if only single col for spec group
  grpM <- unique(naOmit(as.character(grp)))
  out <- matrix(nrow=nrow(x), ncol=length(grpM), dimnames=list(rownames(x),grpM))
  for(i in 1:length(grpM)) {
    useC <- which(grp==unique(naOmit(grp))[i])
    out[,i] <- if(length(useC) >1) rowSds(x[,useC]) else NA }
  out }
   
