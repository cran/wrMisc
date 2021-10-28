#' Count number of NAs per row and group of columns
#' 
#' This functions allows easy counting the number of NAs per row in data organized in multiple sub-groups as columns.
#'  
#'	 
#' @param mat (matrix of data.frame) data to count the number of \code{NA}s
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates)
#' @return matrix with number of \code{NA}s per group
#' @seealso \code{\link{rowGrpMeans}}, \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' mat2 <- c(22.2, 22.5, 22.2, 22.2, 21.5, 22.0, 22.1, 21.7, 21.5, 22, 22.2, 22.7,
#'    NA, NA, NA, NA, NA, NA, NA, 21.2,   NA, NA, NA, NA,
#'    NA, 22.6, 23.2, 23.2,  22.4, 22.8, 22.8, NA,  23.3, 23.2, NA, 23.7,
#'    NA, 23.0, 23.1, 23.0,  23.2, 23.2, NA, 23.3,  NA, NA, 23.3, 23.8)
#' mat2 <- matrix(mat2, ncol=12, byrow=TRUE)
#' gr4 <- gl(3, 4, labels=LETTERS[1:3])
#' # overal number of NAs per row
#' rowSums(is.na(mat2)) 
#' # number of NAs per row and group
#' rowGrpNA(mat2, gr4)
#' @export
rowGrpNA <- function(mat, grp) {
  ## get number of NAs per line & group of replicates
  if(any(length(dim(mat)) !=2, dim(mat) < 1)) stop("Invalid argument 'mat'; must be matrix (or data.frame) with min 1 line and 1 column")
  if(length(grp) != ncol(mat)) stop("Length of 'grp' and number of columns of 'mat' do not match !")
  if(is.data.frame(mat)) mat <- as.matrix(mat)
  gr1 <- naOmit(unique(grp))
  nNA <- matrix(nrow=nrow(mat), ncol=length(gr1), dimnames=list(NULL, gr1))
  for(i in 1:length(gr1)) {
    nNA[,i] <- rowSums(is.na(matrix(mat[,which(grp==gr1[i])], nrow=nrow(mat)) )) }
  nNA }
  
