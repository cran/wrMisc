#' Filter for each group of columns for sufficient data as non-NA
#'
#' The aim of this function is to filter for each group of columns for sufficient data as non-NA.
#' 
#'
#' @details
#' This function allows to identify lines with an \code{NA}-content above the threshold \code{presThr} per group as defined by the levels of factor \code{grp}.  
#' With different types of projects/questions different threshold  \code{presThr} levels may be useful.
#' For example, if one would like to keep the degree of threshold  \code{presThr}s per group rather low, one could use a value of 0.75 (ie >= 75% values non-NA. 
#' 
#' 
#'
#' @param dat matrix or data.frame (abundance or expression-values which may contain some \code{NA}s).
#' @param grp factor of min 2 levels describing which column of 'dat' belongs to which group (levels 1 & 2 will be used)
#' @param presThr (numeric) min ratio of non- \code{NA} values (per group) for returning a given line & group as  \code{TRUE} 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return logical matrix (with on column for each level of \code{grp})
#' @seealso  \code{\link{presenceFilt}}, there are also other packages totaly dedicated to filtering on CRAN and Bioconductor
#' @examples
#'  mat <- matrix(NA, nrow=11, ncol=6)
#'  mat[lower.tri(mat)] <- 1
#'  mat <- cbind(mat, mat[,1:4])
#'  colnames(mat) <- c(paste0("re",1:6), paste0("x",1:4))
#'  mat[6:8,7:10] <- mat[1:3,7:10]  # ref
#'  mat[9:11,1:6] <- mat[2:4,1:6]
#'
#' ##  accept 1 NA out of 4, 2 NA out of 6   (ie certainly present)
#'  (filt0a <- presenceGrpFilt(mat, rep(1:2, c(6,4)), pres=0.66))
#' ##  accept 2 NA out of 4, 2 NA out of 6   (ie min 50% present)
#'  (filt0b <- presenceGrpFilt(mat, rep(1:2, c(6,4)), pres=0.5))
#' ##  accept 3 NA out of 4, 4 NA out of 6   (ie possibly present)
#'  (filt0c <- presenceGrpFilt(mat, rep(1:2, c(6,4)), pres=0.19))
#'
#' @export
presenceGrpFilt <- function(dat, grp, presThr=0.75, silent=FALSE, callFrom=NULL) {
  ## dat(matrix or data.frame)
  ## filter for each group of columns for sufficient data as non-NA
  fxNa <- .composeCallName(callFrom, newNa="presenceGrpFilt")
  if(length(grp) != ncol(dat)) stop("Number of columns of 'dat' (",ncol(dat),") does NOT match length of 'grp' (",length(grp),") !")
  if(presThr >1) { presThr <- 1; if(!silent) message(fxNa, "Argument 'presThr' may not be higher tan 1 (ie 100%), correcting")}
  ## construct unique group-names to use
  chDu <-  duplicated(grp, fromLast=FALSE)
  grpU <- if(any(chDu)) as.character(grp)[which(!chDu)] else as.character(grp)
  ## main filter
  out <- matrix(nrow=nrow(dat), ncol=length(grpU), dimnames=list(rownames(dat), grpU))
  for(i in 1:length(grpU)) { useCol <- which(grp ==grpU[i])
    out[,i] <- if(length(useCol) >0) {if(length(useCol) ==1) !is.na(dat[,useCol]) else rowSums(!is.na(dat[,useCol]))/length(useCol) >= presThr} else FALSE }
  out }
