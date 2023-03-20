#' Add text before file-extension
#'
#' This function helps changing charater srings like file-names and allows adding the character vector 'add'
#' (length 1) before the extension (defined by last '.') of the input string 'x'. 
#' Used for easily creating variants/additional filenames but keeping current extension.
#' @param x main character vector
#' @param add character vector to be added
#' @param sep (character) separator between 'x' & 'add' (character, length 1)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return modified character vector
#' @examples
#' addBeforFileExtension(c("abd.txt","ghg.ijij.txt","kjh"),"new")
#' @export
addBeforFileExtension <- function(x, add, sep="_", silent=FALSE, callFrom=NULL, debug=FALSE) {
  fxNa <- .composeCallName(callFrom, newNa="addBeforFileExtension")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(add) >1) add <- add[1]
  if(length(grep("\\.",x)) >0) {
    extLoc <- sapply(gregexpr("\\.",x),function(y) y[length(y)])
    if(any(extLoc <0)) extLoc[extLoc <0] <- nchar(x[extLoc <0])+1
    paste0(substr(x,1,extLoc-1),sep,add,substr(x,extLoc,nchar(x)))
  } else paste(x,add,sep=sep) }


#' checkFileNameExtensions 
#' Function for checking file-names.
#' @param fileNa (character) file name to be checked
#' @param ext (character) file extension
#' @return modified character vector
#' @examples
#' .checkFileNameExtensions("testFile.txt","txt")
#' @export
.checkFileNameExtensions <- function(fileNa, ext){
  msg <- " need at least 1 character-string as 'fileNa' and as 'ext'"
  if(any(length(fileNa) <1,length(ext) <1)) stop(msg)
  ext <- sub("^\\.","",ext)           # remove starting point-separator
  ext <- unique(paste(".",ext,sep=""))
  che <- nchar(fileNa) > nchar(sub(paste(ext[1],"$",sep=""),"",fileNa)) 
  if(length(ext) >1) for(i in 2:length(ext)) {
    che <- che || nchar(fileNa) > nchar(sub(paste(ext[i],"$",sep=""),"",fileNa))}
  if(sum(!che) >0)fileNa[!che] <- paste(fileNa[!che],ext[1],sep="")
  fileNa }
   
