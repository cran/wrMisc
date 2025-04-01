#' Verify File-name If Existing (in specified path), If Has Proper Extension Or Select Files With Proper Extension From Given Path
#'
#' This function allows tesing if a given file-name corresponds to an existing file (eg for reading lateron).
#' Indications to the path and file-extensions may be given separately. If no files do match .gz compressed versions may be searched, too.
#' 
#' @details
#' 
#' When the filename given by the user exists but it's file-extension is not matching \code{expectExt} 
#' the argument \code{strictExtension} allows to decide if the filename will still be returned or not.
#' 
#' When \code{expectExt} is given, initial search will look for perfect matches. 
#' However, if nothing is found and \code{strictExtension=FALSE}, a more relaxed and non-case-sensitive search will be performed.
#' 
#' @param fileName (character) name of file to be tested; may also include an absolute or relative path; 
#'   if \code{NULL} and \code{path} as well as \code{expectExt} will take 1st file in given path and proper extension
#' @param path (character, length=1) optional separate entry for path of \code{fileName}
#' @param expectExt (character) file extension (will not be considered if \code{""})
#' @param mode (character) further details if function should give error or warning if no files found
#'   integrates previous argument \code{compressedOption} to also look for look for .gz compressed files; 
#'   \code{strictExtension} to decide if extension (\code{expectExt}) - if given - should be considered obligatory;
#'   \code{stopIfNothing} to stop with error if no files found
#' @param compressedOption deprected  (logical) also look for .gz compressed files
#' @param strictExtension deprected  (logical) decide if extesion (\code{expectExt}) - if given - should be considered obligatory
#' @param stopIfNothing deprected, please use argument \code{mode} instead !
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a character vector with verified file-name(s) (and path), returns \code{NULL} if nothing found - unless \code{mode="stopIfNothing"} 
#' @seealso \code{\link[base]{file.exists}} 
#' @examples
#' (RhomeFi <- list.files(R.home()))
#' file.exists(file.path(R.home(), "bin"))
#' checkFilePath(c("xxx","unins000"), R.home(), expectExt="dat")
#' @export
checkFilePath <- function(fileName=NULL, path="./", expectExt="", mode="compressedOption", compressedOption=NULL, strictExtension=NULL, stopIfNothing=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## verify file if existing (in specified path), if has proper extension or select files with proper extension from given path 
  fxNa <- .composeCallName(callFrom, newNa="checkFilePath")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  mode2 <- NULL
  
  if(debug) {message(fxNa,"cFP0"); cFP0 <- list(fileName=fileName,path=path,expectExt=expectExt,mode=mode,compressedOption=compressedOption)}
  if(length(mode) >0) {
    mode <- unique(as.character(mode))
    if(any(c("s","st") %in% mode)) warning(fxNa,"Content for argument 'mode' unclear, ignoring")
    if(any(c("strictExtension","strictExtens","strictExt","strictEx","strictE","strict","stri","str") %in% mode)) mode2 <- "strictExtension"
    if(any(c("compressedOption","compressedOpt","compressed","compr","comp","com","co","c") %in% mode)) mode2 <- c(mode2, "compressedOption")
    if(any(c("stopIfNothing","stopIfNo","stopIfN","stopIf","stopI","stop","sto") %in% mode)) mode2 <- c(mode2, "stopIfNothing")
  } 
  if(isTRUE(strictExtension)) mode2 <- c(mode2, "strictExtension")          # back compatobilty
  if(length(compressedOption) >0 && !isFALSE(compressedOption)) mode2 <- c(mode2, "compressedOption")       # back compatobilty
  mode2 <- unique(as.character(mode2))
  
  comprExt <- if(length(compressedOption) >0 && is.character(compressedOption)) compressedOption else {
    if("compressedOption" %in% mode2) c("gz", "zip", "7z") else NULL } # Supported compressed file extensions

  validExtension <- function(fiNa, extLst, comprExt=c("gz","zip")) {
    ## check if filenames do correspond to expected extensions
    ## if comprExt given, double-extensions due to compression will be recognized/validated, too
    ## return F & T for each file if extLst (& comprExt) correspond
    ##  add name of (compressed/initial) file as name of output
    #example# fiNa <- c("ab","ab.c","ab.c.gz","ab.d","ab.d.zip","ab.e"); extLst <- c("c","d"); validExtension(fiNa, extLst)  # ok
    fiNaExt1 <- tools::file_ext(fiNa)
    out <- fiNaExt1 %in% extLst
    if(length(comprExt) >0 && any(comprExt %in% fiNaExt1)) {
      fiNaExtD <- .doubleExt(fiNa, termExt=comprExt)
      out <- fiNaExtD %in% c(extLst, paste0(rep(extLst, each=length(comprExt)),".",rep(comprExt,length(extLst))))
    } 
    names(out)[which(out)] <- fiNa[which(out)]
  out }

  ## Ensure path exists and replace NA by default './'
  if(length(path) >0) {
    chNA <- is.na(path)
    if(any(chNA)) path[which(chNA)] <- "./"
    if(any(!dir.exists(path))) stop(sum(!dir.exists(path))," Provided 'path' do(es) not exist !")
  } else path <- "./" 
    
  ## Check 'expectExt'
  if(length(expectExt) >0) {
    expectExt <- unique(as.character(expectExt))
    chBad <- nchar(expectExt) <1 | is.na(expectExt)   # '' and NAs get ignored/removed
    if(any(chBad)) expectExt <- expectExt[which(!chBad)]
  } else chBad <- NULL
  if(debug) {message(fxNa,"cFP1"); cFP1 <- list(fileName=fileName,path=path,expectExt=expectExt,mode2=mode2,compressedOption=compressedOption, chBad=chBad)}
  
  ## Construct combined path & fileName
  if(length(fileName) <1) {
    if(length(path) ==1) {
      ## single path : pick all matching
      pat <-  if(length(expectExt) <1) NULL else {if(length(comprExt) <1) paste0("\\.",sub("\\$$","",expectExt),"$") else paste0("\\.",rep(expectExt,each=length(comprExt)), paste0(sub("\\$$","",comprExt),"$") ) }
      pat <- if(length(expectExt) <1) unique(c("", pat))
      files <- list.files(path[1], full.names=TRUE, pattern=pat) 
    } else {                  
      ## multiple paths : pick 1st matching of each path
      files <- sapply(path, function(pa) list.files( pa, 
        pattern= if(length(expectExt) <1) NULL else {if(length(comprExt) >0) paste0("\\.",rep(expectExt, each=length(comprExt)), 
          sub("\\$$","",comprExt),"$") else  paste0("\\.",sub("\\$$","",expectExt),"$")}, full.names=TRUE)[1] )
    }
    paFile <- files
    if(debug) {message(fxNa,"cFP2"); cFP2 <- list(fileName=fileName,path=path,expectExt=expectExt,mode2=mode2,compressedOption=compressedOption,comprExt=comprExt, chBad=chBad, files=files,paFile=paFile)}

  } else {
    ## fileName(s) given
    ## if path is given and meaningful -> add
    paFile <- fileName                                                                           
    if(length(path) >0) {  
      if(length(path)==1) path <- rep(path, length(fileName))
      chPa <- is.na(path) | nchar(path) <1 | path %in% c(".","./")         # identify unusable path: exclude non-given -> meaning current dir
      if(any(!chPa)) paFile[which(!chPa)] <- file.path(path[which(!chPa)], fileName[which(!chPa)])
    }  
  }   
  
  if(debug) {message(fxNa,"cFP3"); cFP3 <- list(fileName=fileName,path=path,expectExt=expectExt,mode2=mode2,compressedOption=compressedOption,comprExt=comprExt, chBad=chBad, paFile=paFile)}
  ## need to integrate "strictExtension"  & "stopIfNothing"

  ### MAIN CHECK
  ## if  length(expectExt) > 1 treat list  : attach ext
  ## if strict & extensions given: check extensions
  if(length(paFile) >0) {
    if(debug) {message(fxNa,"cFP4"); cFP4 <- list(fileName=fileName,path=path,expectExt=expectExt,mode2=mode2,compressedOption=compressedOption,comprExt=comprExt, paFile=paFile)}

    if("strictExtension" %in% mode2) {    # assume each extension should fit to each paFile
      #check length# length(expectExt) ==1 
      if(length(expectExt) >0) {    # standard case, expectExt given -> check & set not-conform to NA (which will be F after file.exists)
        chFi0 <- validExtension(paFile, expectExt, comprExt) 
        if(any(!chFi0)) paFile[which(!chFi0)] <- NA
      } 
      chEx <- file.exists(paFile)   
      out <- paFile
      if(any(!chEx)) out[!chEx] <- NA
    } else {           #  if not strict, pick first of possibly multiple hits
      out <- rep(NA, length(paFile))
      names(out) <- paFile
      fxCh <- function(x) {z <- file.exists(x); if(any(z)) x[which(z)[1]] else NA}
      chFi1 <- file.exists(paFile)     # check which fit right away
      if(any(chFi1)) out[which(chFi1)] <- paFile[which(chFi1)]
      if(any(!chFi1)) {      # cases of no direct hit 
        ext <- NULL
        ## check & remove extension if already in paFi
        fiExt <- tools::file_ext(paFile) %in% expectExt
        if(any(fiExt)) expectExt <- c("",expectExt)    # add 'no extension' in case extension seen somewhere in paFile        
        if(length(expectExt) >0 && length(comprExt) >0) {
          ext <- unique(sub("\\.$","", c(expectExt,paste0( rep(expectExt, each=length(comprExt)),".",rep(comprExt, length(expectExt))) ) ))
        } else if(length(expectExt) >0) ext <- unique(sub("\\.$","", expectExt)) else {
          if(length(comprExt) >0) ext <- unique(sub("\\.$","", comprExt))
        } 
        if(length(ext) >0) { ch2 <- sub("\\.\\.",".", sapply(paFile[which(!chFi1)], paste0, ".", ext))  # append extensions to fileNames
          out[which(!chFi1)] <- if(is.matrix(ch2)) apply(ch2, 2, fxCh) else unlist(sapply(ch2, fxCh))[1:sum(!chFi1)]     # check for presence
        }
      }

      if(any(!is.na(out))) out <- sub("\\.$","", out)      # remove terminal '.' (may be artifact from paste) 
    }
    if(debug) {message(fxNa,"cFP5"); cFP5 <- list(out=out,fileName=fileName,path=path,expectExt=expectExt,mode2=mode2,compressedOption=compressedOption,comprExt=comprExt, paFile=paFile)}
    if("stopIfNothing" %in% mode2 && any(is.na(out))) { stop(sum(is.na(out))," File(s) not found !")}
 
  } else out <- NULL  
  out }



#' Return File-name Extensions Including Double Extensions (eg txt.gz)
#'
#' This function allows retreiving extensions to filenames similar to \code{\link[tools]{file_ext}}, but also alllows to retreive selected double-extensions (txt.gz).
#' The leading dor will ne excluded similar to \code{\link[tools]{file_ext}}
#' 
#' @details
#' 
#' The argument \code{termExt} allows specifying additional (terminal) extensions to be recognized.
#' 
#' @param fiNa (character) name of file to be tested (may include path)
#' @param termExt (character) additional terminal extensions
#' @return This function returns a character vector with file-extensions (excluding the leading dot). (Only purely alphanumeric extensions are recognized.) 
#' @seealso \code{\link[tools]{file_ext}},  \code{\link{checkFilePath}} 
#' @examples
#' fi <- c("ab","ab.c","ab.c.gz","ab.d","ab.d.zip","ab.e")
#' .doubleExt(fi)
#' @export
.doubleExt <- function(fiNa, termExt=c("gz","zip")) {
  ## get file-extension similar to tools::file_ext() BUT allows retreiving double-extensions (like txt.gz)
  lastExt <- tools::file_ext(fiNa)
  if(length(termExt) >0 && all(nchar(termExt) >0)) {
    chDou <- lastExt %in% termExt
    if(any(chDou)) {      #tools::file_ext(tools::file_ext(fiNa)) 
      lastEx2 <- tools::file_ext(mapply(sub, paste0("\\.",lastExt[which(chDou)],"$"), rep("",sum(chDou)), fiNa[which(chDou)])) 
      lastExt[which(chDou)] <- paste0( lastEx2, ".", lastExt[which(chDou)])
    } }            
  lastExt
}
    
