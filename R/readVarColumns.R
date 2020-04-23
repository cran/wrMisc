#' Read tabular content of files with variable number of columns  
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
#' @param fi (character) file-name
#' @param path (character) optional path
#' @param sep (character) separator (between columns)
#' @param emptyFields (\code{NA} or character) missing headers will be replaced by the content of 'emptyFields', if \code{NA} the last column-name will be re-used and a counter added 
#' @param refCo  (integer) for custom choice of column to be used as row-names (default will use 1st text-column)
#' @param supNa (character) base for constructing name for columns wo names (+counter starting at 2), default column-name to left of 1st col wo colname
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix (character or numeric)
#' @seealso for regular 'complete' data \code{\link[utils]{read.table}}  
#' @examples
#' path1 <- system.file("extdata",package="wrMisc")
#' fiNa <- "Names1.tsv"
#' datAll <- readCsvBatch(fiNa,path1)
#' str(datAll)
#' @export
readVarColumns <- function(fi,path=NULL,sep="\t",emptyFields=NA,refCo=NULL,supNa=NULL,callFrom=NULL,silent=FALSE) {
  ## slightly slower function for reading variable tabular content of files: This function allows reading variable number of elements per line (via parsing each line separately).
  ##  read content of file using separator 'sep'; 1st line is expectd to caintain some headers, missing headers will be replaced by the content of 'emptyFields'
  ## 'emptyFields'  (NA or character) content of fields
  ## 'refCo'  (integer) for custom choice of column to be used as row-names (default will use 1st text-column)
  ## 'supNa' (character) base for constructing name for columns wo names (+counter starting at 2), default column-name to left of 1st col wo colname
  fxNa <- .composeCallName(callFrom,newNa="readTsv2")
  fi <- if(length(path)>0) file.path(path,fi) else fi
  if(!file.exists(fi)) stop(" PROBLEM : Can't find file '",fi,"'")
  out <- scan(fi, what="character",sep="\n")
  ## parse each line since file does/may contain variable number of columns and/or (many) columns wo colnames
  out <- strsplit(out,sep)
  maxCo <- max(sapply(out,length))
  out <- t(sapply(out,function(x,maxC) {if(length(x)==maxC) x else {z <- rep("",maxC); z[1:length(x)] <- x; z}},maxC=maxCo))
  supPep <- which(out[1,]=="")
  if(length(supNa) !=1) {supNa0 <- min(supPep,na.rm=TRUE)
    supNa <- if(supNa0 >1) out[1,min(supPep,na.rm=TRUE)-1] else ((1:ncol(out))[-supPep])[1]}
  out[1,supPep] <- paste(supNa,1+1:length(supPep),sep="_")
  if(nrow(out) <2) stop(" PROBLEM : data seem to be empty (header only ?)")
  ## extract row- and col-names, prepare for separating numeric from text
  testNum <- function(x) all(length(grep("(^([0-9]+)|(^[+-][0-9]+)|(^\\.[0-9]+))((\\.[0-9]+)?)(([eE][+-]?[0-9]+)?)$",x)) ==length(x))
  numCol <- apply(out[-1,],2, testNum)
  if(length(refCo) !=1) {refCo <- min(which(!numCol))
    if(!silent) message(fxNa," setting 'refCo' to '",out[1,refCo],"'")}  #
  dupNa <- duplicated(out[1,])
  colNa <- if(any(dupNa)) correctToUnique(out[1,]) else out[1,]
  dupNa <- duplicated(out[-1,refCo])
  rowNa <- if(any(dupNa)) correctToUnique(out[-1,refCo]) else out[-1,refCo]
  out <- out[-1,]
  dimnames(out) <- list(rowNa,colNa)  
  out }
   
