#' Match All Lines of Matrix To Reference
#'
#' This function allows adjusting the order of lines of a matrix \code{mat} to a reference character-vector \code{ref},
#' even when initial direct matching of character-strings using \code{match} is not possible/successful.
#' In this case, various variants of using \code{grep} will be used to see if unambiguous matching is possible of characteristic parts of the text.
#' All columns of \code{mat} will be tested an the column giving the bes resuts will be used.
#'
#' @details
#' This function tests all columns of \code{mat} to see which one gives the best matching results to the reference \code{ref}.
#' In case no direct matching is possible, \code{grep} will be used to find the best partial matching.
#' The orderof the rows of input \code{mat} will be adjusted according to the matching results.
#'
#' If \code{addRef=TRUE}, the reference will be included as additional column to the results, too.
#'
#' @param mat (matrix or data.frame) main input, all columns of of \code{mat} will be tested for (partial) matching of \code{ref}
#' @param ref (character) reference for trying to match each of the columns of  \code{mat}
#' @param addRef (logical), if \code{TRUE} the content of \code{ref} will be added to  \code{mat} as additional column
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns the input matrix in an adjusted order (plus an optional additional column showing the reference)
#'
#' @seealso \code{\link[base]{match}},  \code{\link[base]{grep}}, \code{\link{replicateStructure}}
#' @examples
#' mat1 <- matrix(paste0("__",letters[rep(c(1,1,2,2,3),3) +rep(0:2,each=5)], rep(1:5)), ncol=3)
#' matchMatrixLinesToRef(mat1, paste0(letters[c(3,4,5,3,4)],c(1,3,5,2,4)))
#'
#' mat2 <- matrix(paste0("__",letters[rep(c(1,1,2,2,3),3) +rep(0:2,each=5)],
#'   c(rep(1:5,2),1,1,3:5 )), ncol=3)
#' matchMatrixLinesToRef(mat2, paste0(letters[c(3,4,5,3,4)],c(1,3,5,1,4)))
#'
#' mat3 <- matrix(paste0(letters[rep(c(1,1,2,2,3),3) +rep(0:2,each=5)],
#'   c(rep(1:5,2),1,1,3,3,5) ), ncol=3)
#' matchMatrixLinesToRef(mat3, paste0("__",letters[c(3,4,5,3,4)],c(1,3,5,1,3)))
#'
#'
#'
#' @export
matchMatrixLinesToRef <- function(mat, ref, addRef=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## find best column for matching lines of mat (matrix) to ref (char vector) via two way grep
  ## return list $grep (matched matrix), $col best column
  ## originally wrProteo::.compToRef
  fxNa <- .composeCallName(callFrom, newNa="matchMatrixLinesToRef")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  out <- NULL              # initialize
  leM <- apply(nchar(as.matrix(mat)), 2, stats::median, na.rm=TRUE)
  leR <- stats::median(nchar(ref), na.rm=TRUE)     # median length
  ## grep ref in each col of mat
  gM <- apply(as.matrix(mat), 2, function(x) sapply(ref, grep, x))
  lM <- sapply(gM, sapply, length)          # matrix of counts
  ## 'reverse' grep in each col of mat in ref
  gR <- apply(as.matrix(mat), 2, sapply, grep, ref)
  lR <- if(is.list(gR)) {if(is.list(gR[[1]])) sapply(gR, sapply, length) else sapply(gR, length) } else sapply(gR, length)
  if(is.list(lR)) warning(fxNa, " Trouble ahead, can't make matrix/vector of counts from grep in each col of mat in ref")
  if(debug) {message(fxNa,"..mML1\n"); mML1 <- list(mat=mat,ref=ref,leM=leM,leR=leR,gM=gM,lM=lM,gR=gR,lR=lR)}
  ##
  if(any(lM >0, lR >0)) {           # some (perfect or partial) solutions
    chM1 <- apply(lM, 2, function(x) all(x==1))
    chR1 <- apply(lR, 2, function(x) all(x==1))
    if(sum(chM1, chR1) >0) {         # ideal solution exists
       if(any(chM1)) { out <- list(by="mat", colNo=which(chM1), le=if(length(dim(lM)) >1) lM[,which(chM1)] else lM[which(chM1)], grep=gM[[which(chM1)]])} else {
         out <- list(by="ref", colNo=which(chR1), le=if(length(dim(lR)) >1) lR[,which(chR1)] else lR[which(chR1)], grep=gR[[which(chR1)]]) }
    } else {                         # less ideal (multiple hits and/or empty)
      chM1 <- colSums(lM >0)
      chR1 <- colSums(lR >0)
      if(any(chM1==nrow(mat), chR1==nrow(mat))) {   # multiple hits, no empty, need further refinement
         if(any(chM1==nrow(mat))) { out <- list(by="mat", colNo=which(chM1==nrow(mat))[1],
           le=if(length(dim(lM)) >1) lM[,which(chM1==nrow(mat))[1]] else lM[which(chM1==nrow(mat))[1]], grep=gM[[which(chM1==nrow(mat))[1]]])
         } else if(any(chR1==nrow(mat))) { out <- list(by="mat", colNo=which(chR1==nrow(mat))[1],
           le=if(length(dim(lR)) >1) lR[,which(chR1==nrow(mat))[1]] else lR[which(chR1==nrow(mat))[1]], grep=gR[[which(chR1==nrow(mat))[1]]])}
        ## resolve multiple hits in $grep by picking 1st not yet used
        newGr <- sapply(out$grep, function(x) x[1])
        multX <- unique(names(newGr[which(newGr >1)]))                 # 5jul22
        for(i in 1:length(multX)) newGr[which(names(newGr)==multX[i])] <- out$grep[[multX[i]]]
        out$grep <- newGr
      } else warning(fxNa,"Impossible to find complete solution, returning NULL")
    }
    if(out$by=="ref") { message(fxNa,"Best matching by direct matching")
       out <- mat[out$grep,]
      if(!isFALSE(addRef)) out <- if("ref" %in% colnames(out)) cbind(out, ref) else cbind(out, ref=ref)
    } else {  message(fxNa,"Best matching by reverse matching")
      ref2 <- ref[out$grep]
      out <- mat
      if(!isFALSE(addRef)) out <- if("ref" %in% colnames(out)) cbind(out, ref2) else cbind(out, ref=ref2)
      out <- mat[match(ref,ref2),]
    }
  } else warning(fxNa,"Impossible to find any matches, returning NULL")
  out }

