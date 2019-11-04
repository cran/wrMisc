#' Characterize individual contribution of single edges in tree-structures
#'
#' This function helps investigating tree-like structures with the aim of indicating how much individual tree components contribute 
#' to compose long stretches.
#' \code{contribToContigPerFrag} characterizes individual (isolated) contribution of single edges in tree-structures. 
#' Typically used to process/exploit summarized trees (as matrix) made by \code{\link{buildTree}} which makes use of the package \href{https://CRAN.R-project.org/package=data.tree}{data.tree}.
#' For example if A,B and C can be joined aa well and B +D, this function will check if A+B+C is longer and if A contributes to the longest tree. 
#' @param joinMat (matrix) matrix with concatenated edges as rownames (separated by slashes), column \code{sumLen} for total length and column \code{n} for number of edges
#' @param fullLength (integer) custom total length (useful if the concatenated edges do not cover 100 percent of the original precursor whose fragments are studied)  
#' @param nDig (integer) rounding: number of digits for 3rd column \code{len.rat} in output
#' @return matrix of 3 columns: with length of longest tree-branches where given edge participates (column \code{sumLen}), the (total) number of edges therein (col \code{n.frag}) and a relative value (\code{len.rat})
#' @seealso to build tree \code{\link{buildTree}}
#' @examples
#' path1 <- matrix(c(17,19,18,17, 4,4,2,3),ncol=2,
#'   dimnames=list(c("A/B/C/D","A/B/G/D","A/H","A/H/I"),c("sumLen","n")))
#' contribToContigPerFrag(path1)
#' @export
contribToContigPerFrag <- function(joinMat,fullLength=NULL,nDig=3){
  argN <- deparse(substitute(joinMat))
  msg <- c("Unknown input-format in '",argN,"'")
  if(is.list(joinMat)) {if("paths" %in% names(joinMat)) joinMat <- joinMat$paths else stop(msg)}
  if(length(dim(joinMat)) <2) stop(msg) else if(!"sumLen" %in% colnames(joinMat)) stop("Need column 'sumLen' in ",argN)
  if(is.null(fullLength)) fullLength <- max(joinMat[,"sumLen"])  
  joinMat <- joinMat[sort.list(joinMat[,"sumLen"],decreasing=FALSE),]
  jLi <- lapply(rownames(joinMat),strsplit,"/")
  frags <- sort(unique(unlist(jLi)))
  out <- t(sapply(frags,function(x) {z <- grep(x,rownames(joinMat))
    w <- max(joinMat[z,"sumLen"])
    c(w, min(joinMat[z[which(joinMat[z,"sumLen"]==w)],"n"]) )
  } ))
  colnames(out) <- c("sumLe","n.frag")
  out <- cbind(out,len.rat=round(out[,"sumLe"]/fullLength,nDig))
  out }
      
