#' Check If File Is Available For Reading
#'
#' This function allows tesing if a given file-name corresponds to an existing file (eg for reading lateron).
#' Indications to the path and file-extensions may be given separately. If no files do match .gz compressed versions may be searced, too.
#' 
#' @details
#' 
#' When the filename given by the user exists but it's file-extension is not matching \code{expectExt} 
#' the argument \code{strictExtension} allows to decide if the filename will still be returned or not.
#' 
#' When \code{expectExt} is given, initial search will look for perfect matches. 
#' However, if nothing is found and \code{strictExtension=FALSE}, a more relaxed and non-case-sensitive search will be performed.
#' 
#' @param fileName (character) name of file to be tested; may also include an absolute or relative path
#' @param path (character, length=1) optional separate entry for path of \code{fileName}
#' @param expectExt (character) file extension (will not be considerd if \code{""})
#' @param compressedOption (logical) also look for .gz compressed files
#' @param strictExtension (logical) decide if extesion (\code{expectExt}) - if given - should be considered obligatory
#' @param stopIfNothing (logical) decide if function should give error or warning if no files found
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a character vector with verified path and file-name(s), returns \code{NULL} if nothing 
#' @seealso \code{\link[base]{file.exists}} 
#' @examples
#' (RhomeFi <- list.files(R.home()))
#' file.exists(file.path(R.home(), "bin"))
#' checkFilePath(c("xxx","unins000"), R.home(), expectExt="dat")
#' @export
checkFilePath <- function(fileName, path, expectExt="", compressedOption=TRUE, strictExtension=FALSE, stopIfNothing=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## check file-input if available to read
  fxNa <- .composeCallName(callFrom, newNa="checkFilePath")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  msg <- "Invalid entry for 'path'  "
  ## check path
  if(length(path) >0) { path <- path[1]
     if(is.na(path)) path <- NULL else {
       if(!dir.exists(path)) { path <- "."
         if(!silent) message(fxNa, msg, path[1],"'  (not existing), ignoring...")}
     } }
  if(length(expectExt) <1) expectExt <- "" else if(any(is.na(expectExt))) expectExt <- ""
  #if(isTRUE(compressedOption)) compressedOption <-

  ## check for 'fileName'
  msg <- "Invalid entry for 'fileName'"
  if(length(fileName) <1) stop(msg) else if(any(is.na(fileName)) || any(nchar(fileName) <1)) stop(msg)
  if(nchar(expectExt) >0) if(grepl("^\\.",expectExt)) expectExt <- sub("^\\.", "", expectExt)      # remove heading '.' if accidently given

  ## init check for presence of 'fileName'
  paFi <- if(length(path) >0) file.path(path, fileName) else fileName
  chFi <- file.exists(paFi)
  if(nchar(expectExt) >0 && isTRUE(strictExtension)) chFi <- chFi & grepl(paste0("\\.",expectExt,"$|\\.",expectExt,"\\.gz$"), paFi)   # correct for imposed file-extension
  if(debug) {message(fxNa,"cFP1 "); cFP1 <- list(fileName=fileName,path=path,paFi=paFi,chFi=chFi,expectExt=expectExt,compressedOption=compressedOption )}
  if(any(chFi)) {
    paFi <- paFi[which(chFi)]
    chFi <- chFi[which(chFi)] 
  } 

  if(!any(chFi)) {
    ## check extension
    if(nchar(expectExt) >0) {
      ##if(!grepl("^\\.",expectExt)) expectExt <- paste0("\\.", expectExt)      # add heading '.' if not yet given
      paFi <- paste0(sub(paste0("\\.",expectExt,"$"),"", paFi),".",expectExt)   # avoid doubling extension
      chFi <- file.exists(paFi)
      if(any(chFi)) { if(debug) message(fxNa,"When including file-extension found file(s)  ",pasteC(paste0(fileName,".",expectExt))[which(chFi)])
        paFi <- paFi[which(chFi)]
        chFi <- chFi[which(chFi)]
      } else {    
        if(isFALSE(strictExtension)) {                  # check for upper/lower case of extension
          fiLi <- list.files(path=if(length(path) ==1) path else ".", pattern=paste0(sub("\\.[[:alpha:]]+$","", fileName),"\\.",expectExt,"$"), full.names=TRUE, ignore.case=TRUE)
          if(length(fiLi) >0) {
            paFi <- fiLi
            chFi <- rep(TRUE, length(fiLi))
            if(debug) message(fxNa,"Found file(s) with different lower/upper case spelling of extension: ",pasteC(basename(paFi), quoteC="'"))
          }  
        }
      }
    }
    if(debug) {message(fxNa,"cFP3 "); cFP3 <<- list(fileName=fileName,path=path,paFi=paFi,chFi=chFi,expectExt=expectExt,compressedOption=compressedOption )}

    ## now check for compressed (wo or with extension)
    if(compressedOption && !any(chFi)) {
      msg <- "  not found, BUT a .gz compressed version exists, using compressed file(s).."
      paFi0 <- paFi
      paFi <- paste0(paFi,".gz")
      chFi <- file.exists(paFi)
      if(any(chFi)) {
        if(!silent) message(fxNa,"Note : File(s) ",pasteC(fileName[which(chFi)], quoteC="'"), msg)
        paFi <- paFi[which(chFi)]
        chFi <- chFi[which(chFi)]
      }
      if(!any(chFi) && nchar(expectExt) >0 && isFALSE(strictExtension)) {
        fiLi <- list.files(path=if(length(path) ==1) path else ".", pattern=paste0(sub("\\.[[:alpha:]]+\\.gz$","", fileName),"\\.",expectExt,"\\.gz$"), full.names=TRUE, ignore.case=TRUE)        
        if(length(fiLi) >0) {
          paFi <- fiLi
          chFi <- rep(TRUE, length(fiLi))
          if(!silent) message(fxNa,"Note : Found compressed version of file(s) ",pasteC(basename(paFi), quoteC="'"))
        }
      }
    }
    if(!silent && length(paFi) < length(fileName)) message(fxNa,"Note ",length(fileName) - length(paFi)," files were NOT found !")
  }
  if(!any(chFi)) {
    msg <- c(" File(s) ",pasteC(fileName,quoteC="'")," NOT found ",if(length(path) >0) paste0(" in path '",path,"'")," !")
    if(isTRUE(stopIfNothing)) stop(msg) else warning(msg, "  (returning NULL)")
    if(nchar(expectExt) >0 && !silent) {
      chOth <- grepl(fileName, paFi)
      if(any(chOth)) message(fxNa,"Note : The file-extension might not be correct, found other file(s) with different extension(s)")
    }

    paFi <- NULL
  }
  if(debug) {message(fxNa,"cFP3 "); cFP3 <- list(fileName=fileName,path=path)}
  paFi }
  
