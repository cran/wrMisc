#' Merge Multiple Matrices
#'
#' This function allows merging of multiple matrix-like objects.
#' The matix-rownames will be used to align common elements, either be returning all common elements \code{mode='intersect'} or containg all elements \code{mode='union'} (the result may contains additional \code{NA}s).
#'
#'
#' @details
#' Custom column-names can be given by entering matrices like named arguments (see examples below).
#' The choice of columns tu use may be adopted to each matrix entered, in this case the argument \code{useColumn} may be a list with matrix-names to use or a list of indexes (see examples below).
#'
#' Note, that matrices may contain repeated rownames (see examples, \code{mat3}). In this case only the first of repeated rownames will be considered (and lines of repeated names ignored).
#'
#' @param ... (matrix or data.frame) multiple matrix or data.frame objects may be entered
#' @param mode (character) allows choosing restricting to all common elements (\code{mode='intersect'}) or union  (\code{mode='union'})
#' @param useColumn (integer, character or list) the column(s) to consider, may be \code{'all'} to use all, integer to select specific indexes or list of indexes or colnames for cutom-selection per matrix
#' @param na.rm (logical) suppress \code{NA}s
#' @param extrRowNames (logical) decide whether columns with all values different (ie no replicates or max divergency) should be excluded
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix containing all selected columns of the input matrices to fuse
#' @seealso  \code{\link[base]{merge}},  \code{\link{mergeMatrixList}}
#' @examples
#' mat1 <- matrix(11:18, ncol=2, dimnames=list(letters[3:6],LETTERS[1:2]))
#' mat2 <- matrix(21:28, ncol=2, dimnames=list(letters[2:5],LETTERS[3:4]))
#' mat3 <- matrix(31:38, ncol=2, dimnames=list(letters[c(1,3:4,3)],LETTERS[4:5]))
#'
#' mergeMatrices(mat1, mat2)
#' mergeMatrices(mat1, mat2, mat3, mode="union", useCol=2)
#' ## custom names for matrix-origin
#' mergeMatrices(m1=mat1, m2=mat2, mat3, mode="union", useCol=2)
#' ## flexible/custom selection of columns
#' mergeMatrices(m1=mat1, m2=mat2, mat3, mode="union", useCol=list(1,1:2,2))
#' @export
mergeMatrices <- function(..., mode="intersect", useColumn=1, na.rm=TRUE, extrRowNames=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## merge n matrix (or data.frame) entries, either as all shared lines or as all united/get common lines
  ## 'useColumn' (integer) column used for merging
  inpL <- list(...)
  out <- useColNo <- NULL
  fxNa <- " -> mergeMatrices"    # temporary
  ## separate spectific arguments from all-input (lazy fitting)
  fixArg <- c("mode","useColumn","extrRowNames","silent","debug","callFrom")    # fixed argument names to check (and adjust)
  argL <- match.call(expand.dots = FALSE)$...            # extr arg names, based on https://stackoverflow.com/questions/55019441/deparse-substitute-with-three-dots-arguments
  pMa <- pmatch(names(argL), fixArg)                     # will only find if unique (partial) match
  pMa[which(nchar(names(argL)) <3)] <- NA                # limit to min 3 chars length
  if(any(!is.na(pMa))) for(i in which(!is.na(pMa))) {assign(fixArg[pMa[i]], inpL[[i]]); names(inpL)[i] <- "replaceReplace"}
  chRepl <- names(inpL) %in% "replaceReplace"
  if(any(chRepl)) {inpL <- inpL[-which(chRepl)]; argL <- argL[-which(chRepl)]}
  if(debug) {message(fxNa,"mM0")}

  ## more tests
  if(!isFALSE(na.rm)) silent <- TRUE
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  fxNa <- .composeCallName(callFrom, newNa="mergeMatrices")
  extrRowNames <- try(as.logical(extrRowNames, silent=TRUE))
  if(inherits(extrRowNames, "try-error")) {message(fxNa,"Invalid entry for 'extrRowNames', reset to default =FALSE"); extrRowNames <- FALSE}

  ## adjust names of matrices
  if(length(inpL) >1) {replNa <- if(length(names(inpL)) >0) names(inpL) %in% "" else rep(TRUE, length(inpL))
     if(any(replNa)) names(inpL)[which(replNa)] <- sub("[[:punct:]].*|[[:space:]].*","",as.character(unlist(argL))[which(replNa)]) }

    if(debug) {message(fxNa,"mM1"); mM1 <- list(inpL=inpL, out=out,useColumn=useColumn,extrRowNames=extrRowNames,fixArg=fixArg,argL=argL,pMa=pMa, debug=debug,silent=silent)}
  ## main
  .mergeMatrices(inpL, mode=mode, useColumn=useColumn, extrRowNames=extrRowNames, na.rm=na.rm, argL=argL, silent=silent, debug=debug, callFrom=fxNa)
}


#' Merge Multiple Matrices from List
#'
#' This function allows merging of multiple matrix-like objects from an initial list.
#' The matix-rownames will be used to align common elements, either be returning all common elements \code{mode='intersect'} or containg all elements \code{mode='union'} (the result may contains additional \code{NA}s).
#'
#'
#' @details
#' Custom column-names can be given by entering matrices like named arguments (see examples below).
#' The choice of columns tu use may be adopted to each matrix entered, in this case the argument \code{useColumn} may be a list with matrix-names to use or a list of indexes (see examples below).
#'
#' Note, that matrices may contain repeated rownames (see examples, \code{mat3}). In this case only the first of repeated rownames will be considered (and lines of repeated names ignored).
#'
#' @param matLst (list containing matrices or data.frames) main input (multiple matrix or data.frame objects)
#' @param mode (character) allows choosing restricting to all common elements (\code{mode='intersect'}) or union  (\code{mode='union'})
#' @param useColumn (integer, character or list) the column(s) to consider, may be \code{'all'} to use all, integer to select specific indexes or list of indexes or colnames for cutom-selection per matrix
#' @param na.rm (logical) suppress \code{NA}s
#' @param extrRowNames (logical) decide whether columns with all values different (ie no replicates or max divergency) should be excluded
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return This function returns a matrix containing all selected columns of the input matrices to fuse
#' @seealso  \code{\link[base]{merge}},  \code{\link{mergeMatrices}} for separate entries
#' @examples
#' mat1 <- matrix(11:18, ncol=2, dimnames=list(letters[3:6],LETTERS[1:2]))
#' mat2 <- matrix(21:28, ncol=2, dimnames=list(letters[2:5],LETTERS[3:4]))
#' mat3 <- matrix(31:38, ncol=2, dimnames=list(letters[c(1,3:4,3)],LETTERS[4:5]))
#'
#' mergeMatrixList(list(mat1, mat2))
#'
#' mergeMatrixList(list(m1=mat1, m2=mat2, mat3), mode="union", useCol=2)
#' @export
mergeMatrixList <- function(matLst, mode="intersect", useColumn=1, na.rm=TRUE, extrRowNames=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## merge list of matrices
  if(!isFALSE(na.rm)) silent <- TRUE
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  fxNa <- .composeCallName(callFrom, newNa="mergeMatrixList")
  if(!is.list(matLst) | length(matLst) <1) stop("Invalid entry for 'matLst'")
  namesX <- deparse(substitute(matLst))
  if(debug) {message(fxNa,"mML0"); mML0 <- list(matLst=matLst, useColumn=useColumn,extrRowNames=extrRowNames,namesX=namesX, debug=debug,silent=silent)}

  ## check names (and provide default names)
  lstNa <- names(matLst)
  if(length(lstNa) <1) {
    namesX2 <- unlist(strsplit(sub("\\)$","",sub("^list\\(","",namesX)), ", "))
    names(matLst) <- if(length(namesX2)==length(matLst)) namesX2 else paste0(sub("[[:punct:]].*|[[:space:]].*","",namesX),"_", 1:length(matLst))}
  if(any("" %in% lstNa)) {
    names(matLst)[which(lstNa %in% "")] <- paste0(sub("[[:punct:]].*|[[:space:]].*","",namesX),"_", which(lstNa %in% ""))
  }
  argL <- names(matLst)
  if(debug) {message(fxNa,"mML1")}

  ## main
  .mergeMatrices(matLst, mode=mode, useColumn=useColumn, extrRowNames=extrRowNames, na.rm=na.rm, argL=argL, silent=silent, debug=debug, callFrom=fxNa)
}


#' @export
.mergeMatrices <- function(inpL, mode="intersect", useColumn=1, extrRowNames=FALSE, na.rm=TRUE, argL=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## merge n matrix (or data.frame) entries, either as all shared lines or as all united/get common lines
  ## inpL (list) main input : list of matrices
  fxNa <- .composeCallName(callFrom, newNa=".mergeMatrices")
  out <- NULL
  chLe <- sapply(inpL, length)
  if(all(chLe >0)) {
    chDi <- sapply(inpL, ncol)
    if(is.list(chDi)) { useInp <- sapply(chDi, length) >0
      inpL <- inpL[which(useInp)]
      if(!silent) message(fxNa,"Removing ",sum(useInp)," empty elements from input")
      chDi <- sapply(inpL, ncol)}
    if(any(chDi) <1) {
      if(!silent) message(fxNa,"Removing ",sum(chDi <1)," non-matrix like elements from input")
      inpL <- inpL[which(chDi >0)]
      chDi <- sapply(inpL, ncol) }
    chLe <- sapply(inpL, length)       # update
    }

    if(debug) {message(fxNa,"mNM1"); mNM1 <- list(inpL=inpL, out=out,useColumn=useColumn,extrRowNames=extrRowNames,chLe=chLe,chDi=chDi,inpL=inpL)}

  if(all(chLe >0) & sum(duplicated(chDi), na.rm=TRUE)==length(inpL) -1)  {   # all valid entries

    if(debug) {message(fxNa,"mNM1a") }
    ## final column-names
    useColNa <- NULL
    if(length(inpL) >1) {
      ## names of matrices
      chName <- nchar(names(inpL)) <1
      if(any(chName)) {names(inpL)[which(chName)] <- argL[which(chName)]
        if(debug) message(fxNa,"Adding object-names to ",sum(chName)," matrices as ",pasteC(argL[which(chName)], quoteC="'")) }
      chName <- nchar(names(inpL)) <1                           # update
      ## colnames
      chCoNa <- lapply(inpL, colnames)
      chCoNa2 <- sapply(chCoNa, length) <1
      if(any(chCoNa2)) for(i in which(chCoNa2)) colnames(inpL[[which(chCoNa2)]]) <- 1:ncol(inpL[[which(chCoNa2)]])     # fill empty colnames
    }
    ## check for redundant rownames
    chRNa <- sapply(inpL, function(x) sum(duplicated(rownames(x))) )
    if(any(chRNa >0))  {if(!silent) message(fxNa,"Note : The matrices ",pasteC(names(inpL)[which(chRNa >0)], quoteC="'")," contain redundant rownames (which will be ignored)")}

    ##
    if(identical(useColumn,"all")) { useColumn <- lapply(inpL, function(x) 1:ncol(x))}

    if(debug) {message(fxNa,"mNM2"); mNM2 <- list(inpL=inpL, out=out,useColumn=useColumn,useColNa=useColNa,extrRowNames=extrRowNames,chLe=chLe,chDi=chDi,inpL=inpL)}

    ## prepare empty matrix for results
    if(!identical(mode,"union")) {      # suppose mode is 'intersect'
      ## get common rownames
      useLi <- if(na.rm) naOmit(rownames(inpL[[1]])) else rownames(inpL[[1]])
      if(length(inpL) >1) for(i in 2:length(inpL)) useLi <- intersect(useLi, if(na.rm) naOmit(rownames(inpL[[i]])) else rownames(inpL[[i]]))
      useLi <- sort(useLi)             # multiple results may get easier to compare

      ## start extracting data (mode='intersect'):  extract of 1st matrix
      if(!isTRUE(extrRowNames)) {
        uCol <- if(is.list(useColumn)) useColumn[[1]] else useColumn
        out <- if(length(inpL) >1) inpL[[1]][match(useLi, rownames(inpL[[1]])), uCol] else inpL[[1]][,uCol]
        if(length(dim(out)) <2) out <- matrix(out, ncol=1, dimnames=list(useLi, if(is.numeric(useColumn)) colnames(inpL[[1]])[useColumn] else uCol))
        if(debug) {message(fxNa,"mNM3"); mNM3 <- list(inpL=inpL, out=out,useLi=useLi,useColumn=useColumn,chDi=chDi,chDi=chDi,useColNa=useColNa)}

        ## the remaining matrices (mode='intersect')
        if(length(inpL) >1) {
          ##  get colnames concerned
          uCol <- if(is.list(useColumn)) {lapply(1:length(useColumn), function(x) {if(is.numeric(x)) colnames(inpL[[x]])[useColumn[[x]]] else x})
            } else { if(is.numeric(useColumn)) lapply(inpL, function(x) colnames(x)[useColumn]) else useColumn }
          if(is.list(uCol)) {uCol <- unlist(uCol)}
          names(uCol) <- rep(names(useColumn), sapply(useColumn, length))
          chNa <- is.na(uCol)
          if(any(chNa)) { stop("Invalid content of argument 'useColumn'") }
          chDu <- duplicated(uCol, fromLast=FALSE)
          if(any(chDu)) uCol <- if(length(useColumn) >1) paste(trimRedundText(names(uCol), side="right"), trimRedundText(uCol), sep=".") else trimRedundText(uCol)
          ## finish extracting
          for(i in 2:length(inpL)) {
            uCo2 <- if(is.list(useColumn)) useColumn[[i]] else useColumn
            out <- cbind(out, inpL[[i]][match(useLi, rownames(inpL[[i]])), uCo2])
          }
          if(length(useColumn) >1) colnames(out) <- uCol     # adjust colnames as composed
        }
      } else out <- useLi

    } else {        ## suppose mode='union'
      doSort=TRUE
        if(debug) {message("mNM4"); mNM4 <- list(inpL=inpL, out=out,useColumn=useColumn,chDi=chDi,chDi=chDi,useColNa=useColNa)}
      ## get all rownames
      useLi <- unlist(lapply(inpL, rownames))
      useLi <- if(doSort) unique(sort(useLi)) else unique(useLi)
      ## get colnames concerned
      uCol <- if(is.list(useColumn)) {lapply(1:length(useColumn), function(x) {if(is.numeric(x)) colnames(inpL[[x]])[useColumn[[x]]] else x})
        } else { if(is.numeric(useColumn)) lapply(inpL, function(x) colnames(x)[useColumn]) else useColumn }
      if(is.list(uCol)) uCol <- unlist(uCol)
      chNa <- is.na(uCol)
      if(any(chNa)) { stop("Invalid content of argument 'useColumn'") }
      if(length(useColumn) >1) names(uCol) <- if(is.list(useColumn)) rep(names(inpL), sapply(useColumn, length)) else rep(names(inpL), each=length(useColumn))
        if(debug) {message("mNM4b"); mNM4b <- list(inpL=inpL, out=out,useColumn=useColumn,chDi=chDi,chDi=chDi,useColNa=useColNa)}

      out <- matrix(NA, nrow=length(useLi), ncol=length(uCol), dimnames=list(useLi, paste0(names(uCol),".",uCol)))
      ## 1st matr
      out[match(rownames(inpL[[1]]), useLi), which(names(uCol) %in% names(inpL)[1])] <- as.matrix(inpL[[1]][,if(is.list(useColumn)) useColumn[[1]] else useColumn])
        if(debug) {message("mNM4c"); mNM4c <- list(inpL=inpL, out=out,useColumn=useColumn,chDi=chDi,chDi=chDi,useColNa=useColNa)}

      ## remaining matrices
      if(length(inpL) >1) {
        incCol <- if(is.list(useColumn)) cumsum(sapply(useColumn, length)) else seq_along(inpL)*length(useColumn)            # cumsum() gives last of series
        for(i in 2:length(inpL)) {
          uCo2 <- if(is.list(useColumn)) useColumn[[i]] else useColumn
          out[match(rownames(inpL[[i]]), useLi), which(names(uCol) %in% names(inpL)[i])] <- inpL[[i]][,uCo2] }
      }
    }
  } else if(!silent) message(fxNa,"Invalid entries, all ",length(inpL)," elements must have 2 dimensions")
  out }
  
