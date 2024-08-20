#' Match All Lines of Matrix To Reference Note
#'
#' This function allows adjusting the order of lines of a matrix \code{mat} to a reference character-vector \code{ref},
#' even when initial direct matching of character-strings using \code{match} is not possible/successful.
#' In this case, various variants of using \code{grep} will be used to see if unambiguous matching is possible of characteristic parts of the text.
#' All columns of \code{mat} will be tested an the column giving the bes resuts will be used.
#'
#' @details
#' This function tests all columns of \code{mat} to find perfect matching results to the reference \code{ref}.
#' In case of multiple results the
#' In case no direct matching is possible, \code{grep} will be used to find the best partial matching.
#' The orderof the rows of input \code{mat} will be adjusted according to the matching results.
#'
#' If \code{addRef=TRUE}, the reference will be included as additional column to the results, too.
#'
#' @param mat (matrix or data.frame) main input, all columns of \code{mat} will be tested for (partial) matching of \code{ref}
#' @param ref (character, length must match ) reference for trying to match each of the columns of  \code{mat}
#' @param exclCol (character or integer) column-name or -index of column to ignore/exclude when looking for matches
#' @param addRef (logical), if \code{TRUE} the content of \code{ref} will be added to  \code{mat} as additional column
#' @param inclInfo (logical) allows returning list with new matrix and additional information
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns the input matrix in an adjusted order (plus an optional additional column showing the reference)
#'  or if \code{inclInfo=TRUE} a list with $mat (adjusted matrix), $byColumn, $newOrder and $method;
#'  the reference can bee added as additional last column if \code{addRef=TRUE}
#' @seealso \code{\link[base]{match}},  \code{\link[base]{grep}}, \code{\link{trimRedundText}}, \code{\link{replicateStructure}}
#' @examples
#' ## Note : columns b and e allow non-ambigous match, not all elements of e are present in a
#' mat0 <- cbind(a=c("mvvk","axxd","bxxd","vv"),b=c("iwwy","iyyu","kvvh","gxx"), c=rep(9,4),
#'   d=c("hgf","hgf","vxc","nvnn"), e=c("_vv_","_ww_","_xx_","_yy_"))
#' matchMatrixLinesToRef(mat0[,1:4], ref=mat0[,5])
#' matchMatrixLinesToRef(mat0[,1:4], ref=mat0[1:3,5], inclInfo=TRUE)
#'
#' matchMatrixLinesToRef(mat0[,-2], ref=mat0[,2], inclInfo=TRUE)   # needs 'reverse grep'
#' @export
matchMatrixLinesToRef <- function(mat, ref, exclCol=NULL, addRef=TRUE, inclInfo=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## find best column for matching lines of mat (matrix) to ref (char vector) via two way grep
  ## return list $grep (matched matrix), $col best column
  fxNa <- .composeCallName(callFrom, newNa="matchMatrixLinesToRef")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  byCol <- out <- matElim <- msg <- msgM <- newOr <- chIdenCol <- NULL              # initialize
  datOK <- length(mat) >0
  namesXY <- sub("\\[.*","",c(deparse(substitute(mat)), deparse(substitute(ref))))
  .applyOrder <- function(mat,  ref, newOr, goodCol=NULL, matElim=NULL, chIdenCol=NULL, addRef=TRUE) {
    ## set rows of matrix 'mat' into new order 'newOr'   #; if 'matAlt' give, use instead of 'mat'
    ## 'ref' .. (character); add'l column to add to output
    ## 'newOr' ..(integer) new order
    ## 'chIdenCol' ..(logical) remove all TRUE; has names of very orig mat ; 
    ## 'matElim' ..(matrix) add'l matrix to add to output (order will be adjusted like mat)
    ## 'goodCol' - not used any more #from counting NAs (eg from match), has names of colnames of mat
    if(length(mat) <1 || length(newOr) <1 || length(ref) <1) { out <- mat       # empty or invalid entry - nothing to do
    } else {
      if(is.logical(newOr)) { if(length(newOr)==nrow(mat)) newOr <- which(newOr) else newOr <- NULL } }
    if(length(mat) >0 && length(newOr) >0) {  
      if(length(ref) != length(newOr)) addRef <- FALSE
      out <- if(length(newOr) ==1 || ncol(mat)==1) matrix(mat[newOr,], nrow=length(newOr), dimnames=list(rownames(mat)[newOr], colnames(mat))) else mat[newOr,]
      if(length(matElim) >0) {
        cat("aOO1 \n"); aOO1 <- list(out=out,mat=mat,ref=ref,newOr=newOr,goodCol=goodCol,matElim=matElim,chIdenCol=chIdenCol,addRef=addRef)
        outDiNa <- list(rownames(mat), colnames(out)[-1*which(colnames(out) %in% colnames(matElim))])
        rmCol <- which(colnames(out) %in% colnames(matElim))
        out <- if(length(rmCol) < ncol(out) -1) out[,-1*rmCol] else matrix( out[,-1*rmCol], nrow=nrow(mat), dimnames=list(outDiNa[[1]], outDiNa[[1]][-1*rmCol]))
        if(length(chIdenCol) >0) {
          iniColNa <- colnames(out)
          if(is.logical(chIdenCol) && length(names(chIdenCol)) >0) out <- out[,-1*which(iniColNa %in% names(chIdenCol)[which(chIdenCol)])]
          if(length(out) >0 && length(dim(out)) <1) out <- matrix(out, nrow=nrow(mat), dimnames=list(names(out), iniColNa[-1*which(iniColNa %in% names(chIdenCol)[which(chIdenCol)])]))  
        }
      } 
      if(isTRUE(addRef)) out <- cbind(out, ref=ref)}
    out }

  if(length(mat)==1) { out <- mat; if(!silent) message(fxNa,"length of '",namesXY[1],"'(mat) ==1, nothing to do - return as input"); datOK <- FALSE}
  if(datOK) { if(length(dim(mat)) <2) mat <- matrix(mat, nrow=length(mat), dimnames=list(names(mat), namesXY[1]))
    if(length(ref) <1) { datOK <- FALSE
      stop(fxNa,"Argument '",namesXY[2],"' has incorrect length (found ",length(ref),") !")}}
  if(datOK) if(nrow(mat)==1) {out <- mat; if(!silent) message(fxNa,"'",namesXY[1],"' has single row, nothing to do - return as input"); datOK <- FALSE}

  if(datOK) {
    ## remove all columns of mat with identical values
    fidentVal <- function(x) length(unique(x)) ==1
    chIdenCol <- apply(mat, 2, fidentVal)        # designate columns not useful (all identical values); has also init colnames (as names)

    if(all(chIdenCol, na.rm=TRUE)) { datOK <- FALSE
      if(!silent) message(fxNa,"All lines of '",namesXY[1],"' seem identical, nothing to do for best matching !  (returning NULL)")
    } else {
      if(any(chIdenCol, na.rm=TRUE)) {
        ## split& remove non-informative
        matElim <- if(sum(chIdenCol) >1) mat[,which(chIdenCol)] else matrix(mat[,which(chIdenCol)], ncol=1, dimnames=list(rownames(mat), colnames(mat)[which(chIdenCol)]))
        matUse <- if(sum(!chIdenCol) ==1) matrix(mat[,which(!chIdenCol)], ncol=1, dimnames=list(rownames(mat), colnames(mat)[which(!chIdenCol)])) else mat[,which(!chIdenCol)]
        if(debug) message(fxNa,"Removing ",sum(chIdenCol), "columns of all identical values (have no value for distinguishing lines)")
      } else matUse <- mat
      if(debug) {message(fxNa,"mML1"); mML1 <- list(mat=mat,matUse=matUse,ref=ref,matElim=matElim,chIdenCol=chIdenCol,out=out )}
      ## exclude specified column(s) : exclCol
      if(length(exclCol) >0) {
        exclCol <- if(is.character(exclCol)) naOmit(colnames(matUse) %in% exclCol) else as.integer(naOmit(exclCol[which(exclCol >0 & exclCol <= ncol(mat))] ))
        if(length(exclCol) >0) {
          exclCol <- sort(unique(exclCol)) 
          if(length(exclCol) ==ncol(matUse)) {
            msg <-"All non-uniform columns get excluded via 'exclCol', nothing remains"
            matUse <- NULL
          } else {
            matUse <- if(length(exclCol)+1 ==ncol(matUse)) matrix(matUse[,-exclCol], ncol=1, dimnames=list(rownames(matUse), colnames(matUse)[-exclCol])) else matUse[,-exclCol] }  
        }
      }
      if(length(matUse) >0) {  
        ## try simple match
        sMa <- apply(if(length(matUse) >0) matUse else mat, 2, function(x) match(ref, x))
        chNa <- colSums(is.na(sMa))
        if(any(chNa==0)) { newOr <- sMa[,which(chNa==0)[1]]
          out <- .applyOrder(mat=mat, ref=ref, newOr=newOr, goodCol=NULL, matElim=matElim, chIdenCol=NULL, addRef=addRef)  #  goodCol=chNa, matElim=matElim, chIdenCol=chIdenCol,
          byCol <- which(chNa==0)[1]
          msgM <- "direct match"
        } else {
          if(debug) {message(fxNa,"mML2"); mML2 <- list()}
          ## trim redundant text, re-try match
          mat2 <- apply(if(length(matUse) >0) matUse else mat, 2, trimRedundText, minNchar=1, side="both", silent=silent,callFrom=fxNa,debug=debug)
          ref2 <- trimRedundText(ref, minNchar=1, side="both", silent=silent,callFrom=fxNa,debug=debug)
          sMa <- apply(mat2, 2, function(x) match(ref2, x))
          chNa <- colSums(is.na(sMa))
          if(debug) {message(fxNa,"mML2b"); mML2b <- list()}
          if(any(chNa==0)) { newOr <- sMa[,which(chNa==0)[1]]
            out <- .applyOrder(mat=mat, ref=ref, newOr=newOr, addRef=addRef)  #
            #old#out <- .applyOrder(mat=mat, ref=ref, newOr=newOr, goodCol=chNa, matElim=matElim, chIdenCol=chIdenCol, addRef=addRef)  #
            byCol <- which(chNa==0)[1]
            msgM <- "direct match after trimming redundant text"
            if(debug) {message(fxNa,"mML2c"); mML2c <- list()}
          } else {
            ## direct matching not successful, check if grep possible (only when pattern not longer than x)
            if(debug) {message(fxNa,"mML3"); mML3 <- list(mat=mat,ref=ref,matElim=matElim,chIdenCol=chIdenCol,out=out,chNa=chNa,mat2=mat2,ref2=ref2,sMa=sMa )}
            leM <- nchar(as.matrix(mat2))
            leR <- nchar(ref2)
            refPoss <- apply(leM, 2, function(x) all(sort(x, decreasing=TRUE)[1:length(leR)] >= sort(leR, decreasing=TRUE)))

            if(any(refPoss)) {                           # grep each ref to each col
              chGre <- apply(mat2, 2, function(x) sapply(ref2, grep, x))
              chDL <- sapply(chGre, sapply, length)      # number of grp hits
              ch1 <- colSums(chDL ==1) ==length(ref)
              if(any(ch1)) { newOr <- chGre[[which(ch1)[1]]]
                out <- .applyOrder(mat=mat, ref=ref, newOr=newOr, goodCol=chNa, matElim=matElim, chIdenCol=chIdenCol, addRef=addRef)  #
                byCol <- which(ch1)[1]
                msgM <- "grep of ref after trimming redundant text"
              } else { refPoss <- FALSE
              msg <- c("grep matching not successful (",c("no","ambiguous hits")[1 +any(colSums(chDL >1) >0)],")") }}
            if(any(!refPoss)) {
              ## check by harmonizing/trimming enumerators
              mat3 <- apply(mat2, 2, rmEnumeratorName, nameEnum=c("Number","No","#","Replicate","Sample","Speciem"), sepEnum=c(" ","-","_"), newSep="_No", incl=c("anyCase","trim1"), silent=debug, debug=debug, callFrom=fxNa)
              chCol <- colSums(mat2 ==mat3) < nrow(mat2)      # see if change in all elements in a given column
              if(debug) {message(fxNa,"mML4"); mML4 <- list() }
              if(any(chCol)) {
                if(debug) message(fxNa,"Enumerators exist, try matching after harmonizing style ..")
                if(!all(chCol)) mat3 <- if(sum(chCol) >1) mat3[,which(chCol)] else matrix(mat3[,which(chCol)], nrow=nrow(mat), dimnames=list(rownames(mat2), colnames(mat2)[which(chCol)]))  # trim
                ref3 <- rmEnumeratorName(ref, nameEnum=c("","Number","No","#","Replicate","Sample","Speciem"), sepEnum=c(" ","-","_"), newSep="_No", incl=c("anyCase","trim1"), silent=debug, debug=debug, callFrom=fxNa)
                chDir <- apply(mat3, 2, match, ref3)
                chMa <- apply(chDir, 2, function(x) all(1:nrow(mat) %in% x, na.rm=TRUE))
                if(debug) {message(fxNa,"mML4b"); mML4b <- list() }
                if(any(chMa)) {
                  goodCol <- which(chMa)[1]
                  if(debug) message(fxNa,"Found good hit by using column '",names(goodCol),"'  ie ",pasteC(utils::head(mat2[,goodCol]))," ...")
                  out <- if(addRef) cbind(mat[chDir[,goodCol],], ref=ref) else mat[chDir[,goodCol],]
                  msgM <- "Match after harmonizing enumerators (& trimming redundant text)"
                  if(debug) {message(fxNa,"mML4c"); mML4c <- list() }
                } else {
                  if(debug) message(fxNa,"Try matching all after trimming enumerators")
                  mat3 <- apply(mat2, 2, rmEnumeratorName, nameEnum=c("Number","No","#","Replicate","Sample","Speciem"), sepEnum=c(" ","-","_"), newSep="", incl=c("anyCase","trim1","rmEnum"), silent=debug, debug=debug, callFrom=fxNa)
                  ref3 <- rmEnumeratorName(ref, nameEnum=c("","Number","No","#","Replicate","Sample","Speciem"), sepEnum=c(" ","-","_"), newSep="", incl=c("anyCase","trim1","rmEnum"), silent=debug, debug=debug, callFrom=fxNa)
                  chDir <- apply(mat3, 2, match, ref3)
                  chNa <- colSums(is.na(chDir)) <1
                  if(any(chMa)) {
                    ## need to attribute multiple hits
                    warning(fxNa,"Attribute multiple hits NOT FINISHED !!")
                  } else {
                    if(!silent) message(fxNa," STILL NO MATCH FOUND, try reverse grep of terms wo enumerators ?")
                    invePoss <- apply(leM, 2, function(x) all(sort(x, decreasing=TRUE)[1:length(leR)] <= sort(leR, decreasing=TRUE)))
                  }
                }
              } else {
                ## grep each ref to each col
                ## need to try reverse matching
                chRev <- apply(mat2, 2, function(x) sapply(x, grep, ref2))
                chRL <- sapply(chRev, sapply, length)       # number of grp hits
                ch1 <- (if(ncol(mat) >1) colSums(chRL ==1) else sum(chRL==1)) ==length(ref)
                if(any(ch1)) { newOr <- if(is.list(chRev)) order(unlist(chRev[[which(ch1)[1]]], use.names=FALSE)) else chRev[,which(ch1)[1]]         # new order for mat
                  if(debug) {message(fxNa,"mML5") }
                  out <- cbind(if(any(dim(mat)==1, length(newOr)==1)) matrix(mat[newOr,], ncol=ncol(mat), dimnames=list(rownames(mat)[newOr], colnames(mat))) else mat[newOr,],
                    if(any(dim(matElim)==1, length(newOr)==1)) matrix(matElim[newOr,], ncol=ncol(matElim), dimnames=list(rownames(matElim)[newOr], colnames(matElim))) else matElim[newOr,])
                  if(length(matElim) >0) out <- out[,match(names(chIdenCol), colnames(out))]   # adjust init col-order
                  if(addRef) out <- cbind(out, ref=ref)
                  byCol <- which(ch1)[1]
                  msgM <- "Reverse grep after trimming redundant text"
                } else msg <- c(msg,"Tried reverse grep matching, but impossible to find full set of non-ambiguous matches")
              }
            }
          }
        }
      }  
      if(debug) {message(fxNa,"mML6"); mML6 <- list(mat=mat,ref=ref,matElim=matElim,chIdenCol=chIdenCol,out=out )}
      if(length(msg) >0 && !silent) message(fxNa,msg,"!  Returning NULL")
      if(debug && length(newOr) >0) message(fxNa,"Successfully found new order by ",msgM," : ",pasteC(newOr, quoteC="'"))
    }
  }
  if(isTRUE(inclInfo)) list(mat=out, byColumn=match(names(byCol), names(chIdenCol)), newOrder=newOr, method=msgM) else out }
     
