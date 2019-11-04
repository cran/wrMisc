#' Add text before file-extension
#'
#' This function helps changing charater srings like file-names and allows adding the character vector 'add'
#' (length 1) before the extension (defined by last '.') of the input string 'x'. 
#' Used for easily creating variants/additional filenames but keeping current extension.
#' @param x main character vector
#' @param add character vector to be added
#' @param sep (character) separator between 'x' & 'add' (character, length 1)
#' @return modified character vector
#' @examples
#' addBeforFileExtension(c("abd.txt","ghg.ijij.txt","kjh"),"new")
#' @export
addBeforFileExtension <- function(x,add,sep="_") {
  if(length(add) >1) add <- add[1]
  if(length(grep("\\.",x)) >0) {
    extLoc <- sapply(gregexpr("\\.",x),function(y) y[length(y)])
    if(any(extLoc <0)) extLoc[extLoc <0] <- nchar(x[extLoc <0])+1
    paste(substr(x,1,extLoc-1),sep,add,substr(x,extLoc,nchar(x)),sep="")
  } else paste(x,add,sep=sep) }

#' @export
.checkFileNameExtensions <- function(fileNa,ext){
  msg <- " need at least 1 character-string as 'fileNa' and as 'ext'"
  if(any(length(fileNa) <1,length(ext) <1)) stop(msg)
  ext <- sub("^\\.","",ext)           # remove starting point-separator
  ext <- unique(paste(".",ext,sep=""))
  che <- nchar(fileNa) > nchar(sub(paste(ext[1],"$",sep=""),"",fileNa)) 
  if(length(ext) >1) for(i in 2:length(ext)) {
    che <- che  | nchar(fileNa) > nchar(sub(paste(ext[i],"$",sep=""),"",fileNa))}
  if(sum(!che) >0)fileNa[!che] <- paste(fileNa[!che],ext[1],sep="")
  fileNa }
   
