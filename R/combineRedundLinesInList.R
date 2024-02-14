#' Combine Redundant Lines In List
#'
#' This function provides help for combining/summarizing lines of numeric data which may be summaried according to reference vector or matrix of annotation (part of the same input-list).
#' The data and reference will be aligned and data corresponding to redundant information be combined/summarized.
#'  
#' 
#' @details 
#' All input data should be in a list, ie one or multipl matrix or data.frame for numeric data (see argument \code{datNa}), as well as the reference (see argument \code{refNa}). 
#' The refgerence may be a named character vecor or a matrix for which the column to be used should be specified using the argument \code{refColNa}.
#' In case the annotation is a matrix, the rownames will be used as unique/independent identifyers to adjust potentially different order of numeric data and annotation.
#' In absence of rownames, an additional column  \code{supRefColNa} of the annotation may be designed for adjusting the order of annotation and numeric data.
#'  
#' The numeric list may contain multiple matrixes or data.frames which will all be summarized by the same procedure as long as they have the same initial dimensions and are specified by \code{refNa}. 
#'
#' Please note that all other list elements from input not specified by \code{refNa} (or \code{datNa}) will be maintained in the output just as they are.
#'
#' @param lst (list) main input, containing matrix or data.frame of numeric data (see \code{datNa} and annotation (see \code{refNa}) and possibly unrelated stuff
#' @param refNa (character) name of list-element containing annotation 
#' @param datNa (character) name(s) of list-element(s) containing numeric/quantitation data 
#' @param refColNa (character) in case the list-element to be used as reference is \code{matrix} or \code{data.frame}, the column to be used must be specified here
#' @param supRefColNa (character) in case the \code{lst$refNa} has no rownames, the content of column \code{lst$supRefColNa} will be used instead
#' @param summarizeType (character) the summarization method gets specified here; so far 'sum','av','med','first' and 'last' are implemented
#' @param NA.rm (logical) pass to summarizing functions order to omit \code{NA}s, defaults to \code{TRUE} 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' 
#' @seealso \code{\link{findRepeated}}, \code{\link{firstOfRepLines}}, \code{\link{organizeAsListOfRepl}}, \code{\link{combineRedBasedOnCol}}
#' @return This function returns a list of same length as input
#' @examples
#' x1 <- list(quant=matrix(11:34, ncol=3, dimnames=list(letters[8:1], LETTERS[11:13])), 
#'   annot=matrix(paste0(LETTERS[c(1:4,6,3:5)],LETTERS[c(1:4,6,3:5)]), ncol=1, 
#'   dimnames=list(paste(letters[1:8]),"xx")) )
#' combineRedundLinesInList(lst=x1, refNa="annot", datNa="quant", refColNa="xx")
#' @export
combineRedundLinesInList <- function(lst, refNa="ref", datNa="quant", refColNa="GeneName", supRefColNa=NULL, summarizeType="av", 
  NA.rm=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## combine lines of (matrix) list-elements according to (line-) grouping given by reference (also part of list x); 
  ## name of reference should be in refNa; in case reference is matrix, a specific column (refColNa) may be chosen 
  fxNa <- .composeCallName(callFrom, newNa="combineRedundLinesInList")
  namesXY <- deparse(substitute(lst))
  out <- NULL
  datOK <- length(lst) >0
  if(debug) message(fxNa, "cRLL0")
   
  if(length(refNa) >1) {refNa <- refNa[1]} else if(length(refNa) <1) {datOK <- FALSE
    stop("Argument 'refNa' should design list-element of '",namesXY,"' (index or name)") }
  if(length(datNa) >1) {datNa <- datNa[1]} else if(length(datNa) <1) {datOK <- FALSE
    stop("Argument 'datNa' should design list-element of '",namesXY,"' (index or name)") }

  .chLstNa <- function(x, lst, colName=NULL, lstArgNa, datOK, fxNa, silent) {
    ## check if x (refNa, etc) is valid element of lst,  optional check if including column named colName (for 'refNa')
    ## returns $lstMatch ie position of x in names of lst 
    if(is.numeric(x)) { if(x >= 1 && x <= length(lst)) { lstMatch <- as.integer(x)
      } else { if(!silent) message(fxNa,"Invalid numeric entry for list-element"); datOK <- FALSE}
    } else {  ## check if given name exists in lst
      lstMatch <- which(names(lst) %in% x)
      if(length(lstMatch) >0) {
        names(lstMatch) <- x         
      } else { datOK <- FALSE
        if(!silent) message(fxNa,"Can't find ",x," in names of ",lstArgNa)}
    }
    ## check if lstMatch corresponds to matrix or data.frame with named columns
    if(datOK) { datOK <- length(colnames(lst[[lstMatch]])) ==ncol(lst[[lstMatch]])
      if(!datOK) { if(!silent) message(fxNa," ",lstArgNa,"$",names(lst)[refNa]," does not seem to be suitable matrix or data.frame") }}
    if(datOK  && length(colName)==1) {    ## check if colName part of colnames
      if(is.integer(colName)) { datOK <- colName <= ncol(lst[[lstMatch]]) && colName >=1
        if(!datOK && !silent) message(fxNa,"Invalid numeric entry for 'colName")
      } else {
        chRefCol <- colName %in% colnames(lst[[x]])
        if(!chRefCol) { datOK <- FALSE
          if(!silent) message(fxNa,"Can't find '",colName,"'  in names of ",lstArgNa)
      } } 
    }
    list(datOK=datOK, lstMatch=lstMatch)
  }
  
  if(length(refColNa) < 1) refColNa <- "GeneName"
  if(length(refColNa) > 1) { refColNa <- refColNa[1]
    if(!silent) message(fxNa, "Argument 'refColNa' should be of length 1, trimming ..") }
  xx <- .chLstNa(x=refNa, lst, colName=refColNa, lstArgNa=namesXY[1], datOK=datOK, fxNa, silent)
  datOK <- xx$datOK
  refNa <- xx$lstMatch     # as index (+name)
  xx <- .chLstNa(x=datNa, lst, colName=NULL, lstArgNa=namesXY[1], datOK=datOK, fxNa, silent)
  datOK <- xx$datOK
  datNa <- xx$lstMatch     # as index (+name)
  
  if(debug) { message(fxNa, "cRLL1"); cRLL1 <- list(lst=lst, refNa=refNa,datNa=datNa,refColNa=refColNa, NA.rm=NA.rm, summarizeType=summarizeType,supRefColNa=supRefColNa,datOK=datOK) }

  ## check if redundant data
  if(datOK) {
    datOK <- any(duplicated(if(length(dim(lst[[refNa]])) > 1) lst[[refNa]][,refColNa] else lst[[refNa]]), na.rm=TRUE)
    if(!datOK && debug) message(fxNa,"No redundant values foound, nothing to do ...")    
  }
  
  ## check rownames of annot 
  ## when annotation (ie lst[[refNa]]) has no (row)names, supRefColNa will be used instead (must give unique IDs)
  if(datOK) {
    if(length(dim(lst[[refNa]])) > 1) {
       if(length(rownames(lst[[refNa]])) <1) {
        chSupTRef <- supRefColNa[1] %in% colnames(lst[[refNa]]) 
        if(isTRUE(chSupTRef)) chSupTRef <- isTRUE(sum(duplicated(lst[[refNa]][,chSupTRef])) <1)
        if(!chSupTRef) { datOK <- FALSE
          if(!silent) message(fxNa," 'supRefColNa' either not in annotation-data or not unique !") }       
       }       
    } else {
      if(length(names(lst[[refNa]])) <1) { datOK <- FALSE
        if(!silent) message(fxNa," annotation has no names, can't use 'supRefColNa' (annotation is not matrix) !") }
  } }
  if(debug) { message(fxNa, "cRLL1a"); cRLL1a <- list() }
  
  if(datOK) {
    ## pick annotation concerned : if ..
    ## problem : lst[[refNa]] may be matrix wo rownames !!! (proteomics example)
    ## strategy : option to specify 2nd column (supRefColNa) to use instead of rownames for collapsing all names or picking 1st
    annS <- if(length(dim(lst[[refNa]])) > 1) lst[[refNa]][,refColNa] else lst[[refNa]]
    out <- lst    # keep also non-concerned data, instead of : list()
    newNa <- if(length(dim(annS)) > 1) tapply(rownames(annS), annS[, refColNa], paste, collapse=",") else tapply(names(annS), annS, paste, collapse=",")
    newNFi <- if(length(dim(annS)) > 1) tapply(rownames(annS), annS[, refColNa], function(z) z[1]) else tapply(names(annS), annS, function(z) z[1])
    ## check if any duplicated items exist
    chDu <- duplicated(if(length(dim(annS)) >1) annS[, refColNa] else annS)
    if(all(!chDu, na.rm=TRUE)) datOK <- FALSE
  }   
  if(debug) { message(fxNa, "cRLL2"); cRLL2 <- list(lst=lst, out=out,refNa=refNa,datNa=datNa,refColNa=refColNa, NA.rm=NA.rm, summarizeType=summarizeType,datOK=datOK)  }

    
  if(datOK) {
    ## prepare for multiple summarizing options/functions via argument summarizeType
    sumFx <- function(z, meth) {
      if(length(dim(z)) < 2) stop("Invalid entry 'z' (should be matrix)")
      if(any(c("aver", "average", "mean") %in% meth))  meth <- "av"
      if(any(c("median") %in% meth)) meth <- "med"
      out <- try(switch(meth, 
        sum=colSums(z, na.rm=NA.rm), 
        av=colMeans(z, na.rm=NA.rm), 
        med=apply(z, 2, stats::median, na.rm=NA.rm), 
        first=z[1,], last=z[nrow(z), ]), silent=TRUE)
      if(inherits(out, "try-error")) out <- NULL
      out }
    if(debug) { message(fxNa, "cRLL2a"); cRLL2a <- list()  }
          
    ## prepare $annot
    if(length(dim(annS)) > 1) {
      out[[refNa]] <- annS[which(!duplicated(annS[, refColNa])), ]
      out[[refNa]] <- out[[refNa]][order(out[[refNa]][, refColNa]), ]
    } else out[[refNa]] <- sort(annS[which(!duplicated(annS))])          # , fromLast=TRUE
    if(debug) { message(fxNa, "cRLL2b") }
    
    out[[refNa]] <- by(lst[[refNa]], if(length(dim(lst[[refNa]])) > 1) lst[[refNa]][, refColNa] else names(lst[[refNa]]), 
      function(x) if(length(dim(x)) > 1) cbind(x[1, ], allIniNames=paste(rownames(x), collapse=",")) else cbind(x[1], allIniNames=paste(names(x), collapse=",")))
    if(debug) { message(fxNa, "cRLL2c") }
    out[[refNa]] <- matrix(unlist(out[[refNa]]), nrow=length(out[[refNa]]), byrow=TRUE)
    colNa <- colnames(lst[[refNa]])
    if(length(colNa) < 1) colNa <- paste0("X.", 1:ncol(out[[refNa]]))
    dimnames(out[[refNa]]) <- list(rownames(out[[refNa]]), if(length(colNa) ==ncol(out[[refNa]])) colNa else c(colNa[1], paste0(colNa[1], 2:(ncol(out[[refNa]])))))
    if(debug) { message(fxNa, "cRLL2d") }
    if(is.null(colnames(out[[refNa]]))) {
      colnames(out[[refNa]]) <- c(if(length(refColNa) ==1) refColNa else "ref", paste("x", 1:(ncol(out[[refNa]] -1)))) } else if(colnames(out[[refNa]])[1] == "") 
    colnames(lst[[refNa]])[1] <- if(length(refColNa) ==1) refColNa else "ref"
        
    if(debug) { message(fxNa, "cRLL3; ready to combine data of ", length(datNa)," table(s) (element(s) of 'lst') as ", summarizeType)
      cRLL3 <- list(lst=lst, out=out,sumFx=sumFx,annS=annS,refNa=refNa,datNa=datNa,refColNa=refColNa,NA.rm=NA.rm,summarizeType=summarizeType) }        
    ## now summarize                                                            
    for(i in datNa) { yy <- lst[[i]]
      yz <- unlist(by(yy, if(length(dim(annS)) > 1) annS[, refColNa] else annS, sumFx, summarizeType))
      if(length(yz) > 0) out[[i]] <- matrix(yz, byrow=TRUE, ncol=ncol(yy), dimnames=list(sort(unique(if(length(dim(annS)) >1) annS[, refColNa] else annS)), colnames(yy)))
    }
    if(debug) { message(fxNa, "cRLL3b");  cRLL3b <- list()}
    names(out) <- names(lst)
    out
  } else { if(debug) message(fxNa, "Invalid entry OR no redundant ref/annnot found, nothing to do ...")
    lst
  }
}


#' Combine Redundant Lines In List, Deprecated
#'
#' The function combineRedundLinesInListAcRef() has been deprecated and replaced by combineRedundLinesInList() from the same package
#'
#' @param lst (list) main input
#' @param listNa (character)   names of list-elements containing quantitation data (1st position) and protein/line annotation (2nd position)
#' @param refColNa (character) in case the list-element to be used as reference is \code{matrix} or \code{data.frame}, the column to be used must be specified here
#' @param summarizeType (character) the summarization method gets specified here; so far 'sum','av','med','first' and 'last' are implemented
#' @param NA.rm (logical) pass to summarizing functions order to omit \code{NA}s, defaults to \code{TRUE} 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso \code{\link{combineRedundLinesInList}}
#' @return This function returns a list of same length as input
#' @examples
#' x1 <- list(quant=matrix(11:34, ncol=3, dimnames=list(letters[8:1], LETTERS[11:13])), 
#'   annot=matrix(paste0(LETTERS[c(1:4,6,3:5)],LETTERS[c(1:4,6,3:5)]), ncol=1, 
#'   dimnames=list(paste(letters[1:8]),"xx")) )
#' ## please use combineRedundLinesInList()
#' combineRedundLinesInList(lst=x1, refNa="annot", datNa="quant", refColNa="xx")
#' @export
combineRedundLinesInListAcRef <- function(lst, listNa=c("ref","quant"), refColNa="xx", summarizeType="av", NA.rm=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  .Deprecated(new="combineRedundLinesInList", package="wrMisc", msg="The function combineRedundLinesInListAcRef() has been deprecated and replaced by combineRedundLinesInList()") 
  #combineRedundLinesInList(lst=lst, listNa=listNa, refColNa=refColNa, summarizeType=summarizeType, NA.rm=NA.rm, silent=silent, debug=debug, callFrom=callFrom) 
  fxNa <- .composeCallName(callFrom, newNa="combineRedundLinesInList")
  namesXY <- deparse(substitute(lst))
  out <- NULL
  datOK <- TRUE
  if(debug) message(fxNa, "cRLL0")
  msg1 <- c(" entry for 'listNa' (must design at least 2 list-elements of '", namesXY, "', 1st sould be annotation, others numeric matrix)")
  if(length(listNa) > 1) {
   if(is.numeric(listNa)) {
      listNa <- as.integer(listNa)
      if(any(listNa < 1 | listNa > length(lst))) 
          stop(fxNa, "Invalid (numeric)", msg1)
    } else {
      lstMatch <- match(listNa, names(lst))
      chNA <- is.na(lstMatch)
      if(sum(!chNA) < 2 || chNA[1]) stop(fxNa, msg1)
      if(any(chNA)) { refColNa <- naOmit(refColNa)
          if(!silent) message(fxNa, "Removing ", sum(chNA), " element(s) not found in 'refColNa'") }
    }
  } else stop(fxNa, "Insufficient", msg1)
  
  chUni <- unique(lst[[listNa[1]]])
  if(length(chUni) < 2) datOK <- FALSE
  if(datOK) {
    if(length(refColNa) < 1) refColNa <- "GeneName"
    if(length(refColNa) > 1) { refColNa <- refColNa[1]
      if(!silent) message(fxNa, "Argument 'refColNa' should be of length 1, trimming ..") }
    ## sort listNa to have annot at beginning
    if(length(names(listNa)) == length(listNa)) {
      chO <- naOmit(match(c("ref", "reference", "annot"), tolower(names(listNa))))
      if(length(chO) > 0) {
        if(chO[1] != 1) {
          if(debug) message(fxNa, "Changing order of 'listNa' based on names of 'listNa'")
          listNa <- listNa[c(chO[1], (1:length(listNa))[-chO[1]])] }
      }
    }
    if(debug) { message(fxNa, "cRLL1"); cRLL1 <- list(lst=lst, listNa=listNa, refColNa=refColNa, NA.rm=NA.rm, summarizeType=summarizeType) }
    
    ## re-order listNa to have annotation as 1st
    nuI <- naOmit(match(listNa[-1], names(lst)))
    
    ## check presence of 'refColNa' in lst
    if(length(dim(lst[[listNa[1]]])) > 1) {
      ch1 <- refColNa %in% colnames(lst[[listNa[1]]])
      if(!any(ch1)) stop(fxNa, "Missing column '", refColNa, "' in ", namesXY[1], "$", listNa[1])
    }
    chDuRef <- duplicated(if(length(dim(lst[[listNa[1]]])) > 1) lst[[listNa[1]]][, refColNa] else lst[[listNa[1]]])
    datOK <- any(chDuRef, na.rm=TRUE)
    if(debug) { message(fxNa, "cRLL1b"); cRLL1b <- list()
    }
  }
  if(datOK) {
    chNa <- is.na(nuI)
    if(length(listNa) < 2 || all(chNa)) stop("Invalid entry for 'listNa' (must design at least one numeric matrix of 'lst')")
    if(any(chNa))  message(fxNa, "Elements ", pasteC(listNa[which(chNa)]),"missing")
    xAnn <- which(names(lst) == listNa[1])
    annS <- if(length(dim(lst[[xAnn]])) > 1) lst[[xAnn]][match(rownames(lst[[xAnn]]), rownames(lst[[nuI[1]]])), ] else lst[[xAnn]][match(names(lst[[xAnn]]), rownames(lst[[nuI[1]]]))]
    out <- list()
    newNa <- if(length(dim(annS)) > 1) tapply(rownames(annS), annS[, refColNa], paste, collapse=",") else tapply(names(annS), annS, paste, collapse=",")
    newNFi <- if(length(dim(annS)) > 1) tapply(rownames(annS), annS[, refColNa], function(z) z[1]) else tapply(names(annS), annS, function(z) z[1])
    ## prepare for multiple summarizing options/functions via argument summarizeType
    sumFx <- function(z, meth) {
      if(length(dim(z)) < 2) stop("Invalid entry 'z' (should be matrix)")
      if(any(c("aver", "average", "mean") %in% meth))  meth <- "av"
      if(any(c("median") %in% meth)) meth <- "med"
      out <- try(switch(meth, 
        sum=colSums(z, na.rm=NA.rm), 
        av=colMeans(z, na.rm=NA.rm), 
        med=apply(z, 2, stats::median, na.rm=NA.rm), 
        first=z[1,], last=z[nrow(z), ]), silent=TRUE)
      if(inherits(out, "try-error")) out <- NULL
      out }
    if(debug) { message(fxNa, "cRLL2") }
    
    ## prepare $annot
    if(length(dim(annS)) > 1) {
      out[[xAnn]] <- annS[which(!duplicated(annS[, refColNa], fromLast=TRUE)), ]
      out[[xAnn]] <- out[[xAnn]][order(out[[xAnn]][, refColNa]), ]
    } else out[[xAnn]] <- sort(annS[which(!duplicated(annS, fromLast=TRUE))])
    if(debug) { message(fxNa, "cRLL2b"); cRLL2b <- list() }
    out[[xAnn]] <- by(lst[[xAnn]], if(length(dim(lst[[xAnn]])) > 1) lst[[xAnn]][, refColNa] else names(lst[[xAnn]]), 
      function(x) if(length(dim(x)) > 1) cbind(x[1, ], allIniNames=paste(rownames(x), collapse=",")) else cbind(x[1], allIniNames=paste(names(x), collapse=",")))
    if(debug) { message(fxNa, "cRLL2c") }
    out[[xAnn]] <- matrix(unlist(out[[xAnn]]), nrow=length(out[[xAnn]]), byrow=TRUE)
    colNa <- colnames(lst[[xAnn]])
    if(length(colNa) < 1) colNa <- paste0("X.", 1:ncol(out[[xAnn]]))
    dimnames(out[[xAnn]]) <- list(names(out[[xAnn]]), if(length(colNa) ==ncol(out[[xAnn]])) colNa else c(colNa[1], paste0(colNa[1], 2:(ncol(out[[xAnn]])))))
    if(debug) { message(fxNa, "cRLL2d") }
    if(is.null(colnames(out[[xAnn]]))) {
      colnames(out[[xAnn]]) <- c(if(length(refColNa) ==1) refColNa else "ref", paste("x", 1:(ncol(out[[xAnn]] -1)))) } else if(colnames(out[[xAnn]])[1] == "") 
    colnames(lst[[xAnn]])[1] <- if(length(refColNa) ==1) refColNa else "ref"    
    if(debug) { message(fxNa, "cRLL3; Summarizing ", length(nuI)," elements as ", summarizeType)
      cRLL3 <- list(lst=lst, out=out,sumFx=sumFx,annS=annS, xAnn=xAnn,listNa=listNa,refColNa=refColNa,NA.rm=NA.rm,summarizeType=summarizeType) }
        
    ## now summarize                                                            
    for(i in nuI) { yy <- lst[[i]]
      yz <- unlist(by(yy, if(length(dim(annS)) > 1) annS[, refColNa] else annS, sumFx, summarizeType))
      if(length(yz) > 0) out[[i]] <- matrix(yz, byrow=TRUE, ncol=ncol(yy), dimnames=list(sort(unique(if(length(dim(annS)) >1) annS[, refColNa] else annS)), colnames(yy)))
    }
    names(out) <- names(lst)
    rownames(out[[xAnn]]) <- rownames(out[[nuI[1]]])
    out
  } else { if(debug) message(fxNa, "No redundant ref/annnot found, nothing to do ...")
    lst
  }
}  
   
