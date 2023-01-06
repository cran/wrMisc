#' Order Lines of Matrix According to Reference (Character) Vector
#'
#' @description
#' This function orders lines of matrix \code{mat} according to a (character) reference vector \code{ref}.
#' To do so, all columns of \code{mat} will be considered to use the first column from left with the best (partial) matching results.
#' This function first looks for unambiguous perfect matches, and if not found successive rounds of more elaborte partial matching will be engaged: 
#' In case of no perfect matches found, grep of \code{ref} on all columns of \code{mat} and/or grep of all columns of \code{mat} on \code{ref} (ie 'reverse grep') will be applied (finally a 'two way grep' approach).
#' Until a perfect match is found each element of \code{ref} will be tested on \code{mat} and inversely (for each column) each element of \code{mat} will be tested on \code{ref}.
#' The approach with the best number of (unique) matches will be chosen. In case of one-to-many matches, it will be tried to use most complete lines (see also last example).
#'
#' @param mat (matrix, data.frame) main input of which rows should get re-ordered according to a (character) reference vector \code{ref}
#' @param ref (character) reference imposing new order
#' @param addRef (logical) add \code{ref} to output as new column
#' @param listReturn (logical) allows retrieving more information in form of list
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns, depending on \code{listReturn}, either the input-matrix in new order or a list with $mat (the input matrix in new order), $grep (matched matrix) and $col indicating the colum of \code{mat} finally used 
#' @seealso for basic ordering see \code{\link[base]{match}}; \code{\link{checkGrpOrder}} for testing each line for expected order, \code{\link{checkStrictOrder}} to check for strict (ascending or descending) order
#' @examples
#' mat1 <- matrix(paste0("__",letters[rep(c(1,1,2,2,3),3) +rep(0:2,each=5)], rep(1:5)), ncol=3)
#' orderMatrToRef(mat1, paste0(letters[c(3,4,5,3,4)],c(1,3,5,2,4)))
#' 
#' mat2 <- matrix(paste0("__",letters[rep(c(1,1,2,2,3),3) +rep(0:2,each=5)], 
#'   c(rep(1:5,2),1,1,3:5 )), ncol=3)
#' orderMatrToRef(mat2, paste0(letters[c(3,4,5,3,4)],c(1,3,5,1,4)))
#' 
#' mat3 <- matrix(paste0(letters[rep(c(1,1,2,2,3),3) +rep(0:2,each=5)], 
#'   c(rep(1:5,2),1,1,3,3,5 )), ncol=3)
#' orderMatrToRef(mat3, paste0("__",letters[c(3,4,5,3,4)],c(1,3,5,1,3)))
#'
#' @export
orderMatrToRef <- function(mat, ref, addRef=TRUE, listReturn=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## order lines of matrix \code{mat} according to (character) vector \code{ref}
  ## in case of no perfect matches, grep of \code{ref} on all columns of \code{mat} and/or grep of all columns of \code{mat} on \code{ref} (ie 'reverse grep') will be applied
  ## find best column for matching mat (matrix) to ref (char vector) via two way grep
  ## return list $grep (matched matrix), $col best colum
  ## idea : also use trimRedundText() ?
  fxNa <- .composeCallName(callFrom, newNa="orderMatrToRef")   # was '.compToRef'
  out <- NULL              # initialize
  ## check for simple solution
  ch1 <- colSums(matrix(mat %in% ref, ncol=ncol(mat)))==nrow(mat)
  if(any(ch1)) {
    out <- list(by="match", colNo=which(ch1), le=rep(1,nrow(mat)), ord=match(ref, mat[,which(ch1)]))
    out$mat <- mat[out$ord,]
  } else {
    ## need to use grep ..
    leM <- apply(nchar(as.matrix(mat)), 2, stats::median, na.rm=TRUE)          # median number of characters per col (mat)
    leR <- stats::median(nchar(ref), na.rm=TRUE)                               # median number of characters (ref)
    ## grep ref in each col of mat
    gM <- apply(as.matrix(mat), 2, function(x) sapply(ref, grep, x))           # grep each col of mat in ref
    lM <- sapply(gM, sapply, length)                                           # matrix of counts
    ## 'reverse' grep in each col of mat in ref
    gR <- apply(as.matrix(mat), 2, sapply, grep, ref)
    lR <- if(is.list(gR)) { if(is.list(gR[[1]])) sapply(gR, sapply, length) else sapply(gR, length) } else sapply(gR, length)
    #if(is.list(lR)) lT <- sapply(gR, sapply, length) else if(length(dim(lR)) <2) lR <- matrix(lR, nrow=1,dimnames=list(NULL, names(lR)))
    if(is.list(lR)) warning(fxNa, " Trouble ahead, can't make matrix/vector of counts from grep in each col of mat in ref")
    if(debug) {message(fxNa,"..cTR1"); cTR1 <- list(mat=mat,ref=ref,leM=leM,leR=leR,gM=gM,lM=lM,gR=gR,lR=lR)}
    ##                         leM=leM,leR=leR,gM=gM,lM=lM,lR=lR,gR=gR,
    if(any(lM >0, lR >0)) {           # some (perfect or partial) solutions
      chM1 <- apply(lM, 2, function(x) all(x==1))
      chR1 <- apply(lR, 2, function(x) all(x==1))
      if(sum(chM1, chR1) >0) {         # ideal solution exists (direct matching)
         if(any(chM1)) { out <- list(by="mat", colNo=which(chM1), le=if(length(dim(lM)) >1) lM[,which(chM1)] else lM[which(chM1)], ord=gM[[which(chM1)]])} else {
           out <- list(by="ref", colNo=which(chR1), le=if(length(dim(lR)) >1) lR[,which(chR1)] else lR[which(chR1)], ord=gR[[which(chR1)]]) }
      } else {                        # less ideal (multiple hits and/or empty)
        chM1 <- colSums(lM >0)
        chR1 <- colSums(lR >0)
        if(any(chM1==nrow(mat), chR1==nrow(mat))) {   # multiple hits, no empty, need further refinement
           if(any(chM1==nrow(mat))) { out <- list(by="ref", colNo=which(chM1==nrow(mat))[1],        # solution in M
             le=if(length(dim(lM)) >1) lM[,which(chM1==nrow(mat))[1]] else lM[which(chM1==nrow(mat))[1]], ord=gM[[which(chM1==nrow(mat))[1]]])
           } else if(any(chR1==nrow(mat))) { out <- list(by="mat", colNo=which(chR1==nrow(mat))[1],
             le=if(length(dim(lR)) >1) lR[,which(chR1==nrow(mat))[1]] else lR[which(chR1==nrow(mat))[1]], ord=gR[[which(chR1==nrow(mat))[1]]])}
          ## resolve multiple hits in $ord by picking 1st not yet used
          newGr <- sapply(out$ord, function(x) x[1])                    # simply 1st of multiple
          ## try using other instances of repeated : other cols of mat may contain other information
          multX <- unique(names(newGr[which(sapply(out$ord, length) >1)]))   # names of all multi-hit
          for(i in 1:length(multX)) newGr[which(names(newGr)==multX[i])] <- out$ord[[multX[i]]]
          out$ord <- newGr
          if(debug) message(fxNa," newGr ",wrMisc::pasteC(newGr))
        } else {warning(fxNa,"Impossible to find complete solution, returning NULL")}
      }
      if(debug) { message(fxNa,"..cTR2"); cTR2 <- list(mat=mat,ref=ref,leM=leM,leR=leR,gM=gM,lM=lM,gR=gR,lR=lR,out=out)}
    } else warning(fxNa,"Impossible to find any matches, returning NULL")
    if(length(out) >0) {
      if(out$by=="mat")  out$ord <- order(out$ord)
      out$mat <- mat[out$ord,]
      if(!isFALSE(addRef)) out$mat <- if("ref" %in% colnames(out$mat)) cbind(out$mat, ref) else cbind(out$mat, ref=ref)
      if(isFALSE(listReturn) & length(out) >0) out <- out$mat }
  }
  out }
  
