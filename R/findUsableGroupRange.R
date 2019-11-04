#' Select groups within given range
#'
#' This function aims to help finding streches/segments of data with a given maximum number of NA-instances.
#  To do so, it searches independently each line of the input-matrix 'dat' for sretches with a given maximum of NA-instaces (\code{'maxNA'}. 
#' This function is used to inspect/filter each lines of 'dat' for a subset with sufficient presence/absence of NA values (ie limit number of NAs per level of 'grp').
#' Note :  optimal perfomance with n.lines >> n.groups
#' @param dat (matrix or data.frame) main input
#' @param grp (factor) information which column of 'dat' is replicate of whom
#' @param maxNA (interger) max number of tolerated NAs
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix with boundaries of 1st and last usable column (NA if there were no suitable groups found)
#' @examples
#' dat1 <- matrix(1:56,nc=7)
#' dat1[c(2,3,4,5,6,10,12,18,19,20,22,23,26,27,28,30,31,34,38,39,50,54)] <- NA
#' rownames(dat1) <- letters[1:nrow(dat1)]
#' findUsableGroupRange(dat1,gl(3,3)[-(3:4)])
#' @export
findUsableGroupRange <- function(dat,grp,maxNA=1,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="findUsableGroupRange")
  msg <- "expecting (2dim) numeric matrix or data.frame with >1 columns and >1 rows"
  if(length(dim(dat)) !=2) stop(msg)
  if(ncol(dat) <2) stop(msg)
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  if(length(grp) != ncol(dat)) stop("Number of columns in 'dat' not matching levels of 'grp'")
  nGrp <- table(grp)
  nGrp <- nGrp[order(unique(grp))]
  if(length(nGrp) <2) stop(" too few levels in 'grp' !")
  if(any(nGrp <= maxNA)) stop(" some levels of 'grp' with too few instances !")
  ## main
  out <- ou2 <- rep(0,nrow(dat))
  for(i in length(levels(grp)):1) {
    tmp <- dat[,which(grp==levels(grp)[i])]
    out[which(rowSums(is.na(tmp)) < maxNA)] <- i
    }
  for(i in 1:length(levels(grp))) {
    tmp <- dat[,which(grp==levels(grp)[i])]
    ou2[which(rowSums(is.na(tmp)) < maxNA)] <- i
    }
  out <- cbind(from=out,to=ou2)
  rownames(out) <- rownames(dat)
  out[which(out <1)] <- NA
  out }

