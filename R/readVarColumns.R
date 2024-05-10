#' Read Tabular Content Of Files With Variable Number Of Columns  
#'
#' Reading the content of files where the number of separators (eg tabulation) is variable poses problems with traditional methods for reding files, like  \code{\link[utils]{read.table}}.
#' This function reads each line independently and then parses all separators therein. The first line is assumed to be column-headers.
#' Finally, all data will be returned in a matrix adopted to the line with most separators and if the number of column-headers is insufficient, new (unique) column-headers will be generated.
#' Thus, the lines may contain different number of elements, empty elements (ie tabular fields) will always get added to right of data read 
#' and their content will be as defined by argument \code{emptyFields} (default \code{NA}).
#' 
#' Note, this functions assumes one line of header and at least one line of data !
#' Note, for numeric data the comma is assumed to be US-Style (as '.').
#' Note, that it is assumed, that any missing fields for the complete tabular view are missing on the right (ie at the end of line) !
#'
#' @param fiName (character) file-name
#' @param path (character) optional path
#' @param sep (character) separator (between columns)
#' @param header (logical) indicating whether the file contains the names of the variables as its first line.
#' @param emptyFields (\code{NA} or character) missing headers will be replaced by the content of 'emptyFields', if \code{NA} the last column-name will be re-used and a counter added 
#' @param refCo (integer) for custom choice of column to be used as row-names (default will use 1st text-column)
#' @param supNa (character) base for constructing name for columns wo names (+counter starting at 2), default column-name to left of 1st col wo colname
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix (character or numeric)
#' @seealso for regular 'complete' data \code{\link[utils]{read.table}} and its argument \code{flush}  
#' @examples
#' path1 <- system.file("extdata",package="wrMisc")
#' fiNa <- "Names1.tsv"
#' datAll <- readVarColumns(fiName=file.path(path1,fiNa))
#' str(datAll)
#' @export
readVarColumns <- function(fiName, path=NULL, sep="\t", header=TRUE, emptyFields=NA, refCo=NULL, supNa=NULL, silent=FALSE, callFrom=NULL) {
  ## slightly slower function for reading variable tabular content of files: This function allows reading variable number of elements per line (via parsing each line separately).
  ##  read content of file using separator 'sep'; 1st line is expectd to caintain some headers, missing headers will be replaced by the content of 'emptyFields'
  ## 'emptyFields'  (NA or character) content of fields
  ## 'refCo'  (integer) for custom choice of column to be used as row-names (default will use 1st text-column)
  ## 'supNa' (character) base for constructing name for columns wo names (+counter starting at 2), default column-name to left of 1st col wo colname
  fxNa <- .composeCallName(callFrom, newNa="readVarColumns")
  fiName <- if(length(path) >0) {if(nchar(path) >0) file.path(path, fiName) else fiName} else fiName
  if(!file.exists(fiName)) { dataOK <- FALSE
    warning("PROBLEM : Can't find file '",fiName,"'")
  } else {
    dataOK <- TRUE
    if(length(header) !=1) header <- FALSE
    if(!is.logical(header)) { dataOK <- FALSE 
      warning(fxNa,"Invalid command: argument 'header' should be logical and of length=1")} }
  if(dataOK) {
    out <- try(scan(fiName, what="character", sep="\n"), silent=TRUE)
    if(inherits(out, "try-error")) { dataOK <- FALSE
      warning(fxNa,"Did NOT succeed reading file '",fiName,"'")}}

  if(dataOK) {  
    ## parse each line since file does/may contain variable number of columns and/or (many) columns wo colnames
    out <- strsplit(out, sep)
    maxCo <- max(sapply(out, length))
    out <- t(sapply(out, function(x,maxC) {if(length(x)==maxC) x else {z <- rep("",maxC); z[1:length(x)] <- x; z}}, maxC=maxCo))
    supPep <- which(out[1,] =="")
    if(length(supNa) !=1) { supNa0 <- min(supPep, na.rm=TRUE)
      supNa <- if(supNa0 >1) out[1, min(supPep, na.rm=TRUE) -1] else ((1:ncol(out))[-supPep])[1]}
    out[1, supPep] <- paste(supNa, 1 +1:length(supPep),sep="_")
    if(nrow(out) <2) { dataOK <- FALSE
      warning("PROBLEM : data seem to be empty (header only ?)")} }

  if(dataOK) {  
    ## extract row- and col-names, prepare for separating numeric from text
    testNum <- function(x) all(grepl("(^([0-9]+)|(^[+-][0-9]+)|(^\\.[0-9]+))((\\.[0-9]+)?)(([eE][+-]?[0-9]+)?)$", x))
    numCol <- apply(out[-1,], 2, testNum)
    if(length(refCo) !=1) {refCo <- min(which(!numCol))
      if(isFALSE(silent)) message(fxNa,"Setting 'refCo' to '",out[1,refCo],"'")}  #
    dupNa <- duplicated(out[1,])  
    ## integrate colnames and rownames ...
    useLi <- if(header) 2:nrow(out) else 1:nrow(out)
    colNa <- if(header) {if(any(dupNa)) correctToUnique(out[1,], callFrom=fxNa) else out[1,]} else NULL
    dupNa <- duplicated(out[useLi,refCo])
    rowNa <- if(any(dupNa)) correctToUnique(out[useLi,refCo], callFrom=fxNa) else out[useLi,refCo]
    if(header) out <- out[-1,]
    if(length(dim(out)) <2) out <- matrix(out, nrow=1, dimnames=list(rowNa, names(out))) else dimnames(out) <- list(rowNa,colNa)  
    out } }
   
