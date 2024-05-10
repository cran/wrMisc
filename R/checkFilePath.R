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
#' @param fileName (character) name of file to be tested; may also include an absolute or relative path; 
#'   if \code{NULL} and \code{path} as well as \code{expectExt} will take 1st file in given path and proper extension
#' @param path (character, length=1) optional separate entry for path of \code{fileName}
#' @param expectExt (character) file extension (will not be considered if \code{""})
#' @param mode (character) further details     if function should give error or warning if no files found
#'   integrates previous argument \code{compressedOption} to also look for look for .gz compressed files; 
#'   \code{strictExtension} to decide if extension (\code{expectExt}) - if given - should be considered obligatory;
#'   \code{stopIfNothing} to stop with error if no files found
#' @param compressedOption deprected  (logical) also look for .gz compressed files
#' @param strictExtension deprected  (logical) decide if extesion (\code{expectExt}) - if given - should be considered obligatory
#' @param stopIfNothing deprected, please use argument \code{mode} instead !
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
checkFilePath <- function(fileName, path, expectExt="", mode="byFile", compressedOption=NULL, strictExtension=NULL, stopIfNothing=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## check file-input if available to read
  fxNa <- .composeCallName(callFrom, newNa="checkFilePath")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  msg <- "Invalid entry for 'path'  "   
  if(debug) {message(fxNa,"cFP0a"); cFP0a <- list(fileName=fileName,path=path,expectExt=expectExt,mode=mode)}
   
  .NaRepl <- function(x, repl) {   # replace NAs
    if(length(x) <1) x <- repl else {
      chNa <- is.na(x)
      if(any(chNa)) x[which(chNa)] <- repl }
    x } 
     
   .adLen <- function(x,y) {  ## adjust length of 'x' to 'y'
     if(length(x) < length(y)) x <- rep(x, length(y))[1:length(y)]                
     if(length(x) > length(y)) x <- x[1:length(y)]                
     x  }
   
  .findSingleFile <- function(pa, fi, ext, strict=FALSE, compr=FALSE, sil=FALSE, deb=FALSE, caFr=NULL) {    # main function for SINGLE pa (path), fi, ext; testing, presumes extP & ext are checked/adjusted
    ## return filename found to be present (if multiple possible ONLY 1st will be given)
    if(length(fi) >0 && is.na(fi[1])) fi <- NULL
    argLe <- c(pa=length(pa), fi=length(fi), ext=length(ext))
    if(argLe[1] < 0) pa <- "." 
    fiN <- NULL
    debugFx <- deb         # for debugging
    compExt <- c(".zip",".7z",".gz")
    if(argLe[2] <1 && argLe[3] <1) {            # neither extension nor file-name, shortcut/option to pick 1st of path
      if(argLe[1] >0 && dir.exists(pa[1])) { fiN <- list.files(pa)[1] } }
    if(debugFx) {message("cSF0b"); cSF0b <- list(fiN=fiN,argLe=argLe,pa=pa,fi=fi,ext=ext)}
    ## check for direct hit
    if(length(fiN) <1 && argLe[2]==1) { 
      fiN <- list.files(pa, pattern=paste0("^", sub("\\.","\\\\.", fi), "$")) }
    if(length(fiN) <1 && argLe[3] >0) {            # remove empty
      chEL <- nchar(ext) <1
      if(any(chEL)) { ext <- ext[which(!chEL)]
        argLe[3] <- length(ext) } }
    if(length(fiN) <1 && argLe[3] >0) {
      ext <- paste0(".",sub("^\\.","", ext)) }     # add heading '.' if missing
      
    ## check for direct hit with extesion
    if(length(fiN) <1 && argLe[2]==1 && argLe[3]==1) {
      fi2 <- sub("\\.","\\\\.", paste0("^",fi, ext,"$"))  
      if(length(ext) >1) fi2 <- paste(paste0("(",fi2,")"), collapse="|")
      fiN <- list.files(pa, pattern=fi2) }
      
    ## no filename but extension - look as pattern & pick 1st  
    if(length(fiN) <1 && argLe[2] <1 && argLe[3] >0) {
      ext2 <- paste0(c(ext, if(compr) paste0(rep(ext, length(compExt)), rep(compExt, each=length(ext)))), "$")
      chLe <- nchar(ext2) >0
      if(any(chLe)) ext2[which(chLe)] <- sub("\\.","\\\\.", ext2[which(chLe)] )
      fiN <- list.files(pa, pattern=ext2)
      if(length(fiN) >0) { if(!sil) message(caFr, if(debug) "cSF1 ","Found ",length(fiN)," matches, picking 1st ..")
        fiN <- fiN[1] }      
    }    
    if(debugFx) {message("cSF1"); cSF1 <- list(fiN=fiN,argLe=argLe,pa=pa,fi=fi,ext=ext,compr=compr,compExt=compExt)}

    ## integrate ext & compression (fi & ext given)
    if(length(fiN) <1 && argLe[2]==1 && argLe[3] >0) { 
      ext2 <- c(ext, if(compr) sapply(ext, paste0, compExt))    # add compressed versions
      chELe <- nchar(ext2) >0
      if(any(chELe)) ext2[which(chELe)] <- paste0(".",sub("\\.","", ext2[which(chELe)]))   # check/add heading point at extension     
      fiN <- paste0(fi,ext2) %in% list.files(path=pa)    # search as strict
      fiN <- if(any(fiN)) paste0(fi,ext2)[which(fiN)] else NULL
      if(length(fiN) <1 && compr) {                              # now check if ext is already in fi & adjust
        chExt <- ext %in% paste0(".",tools::file_ext(fi))
        if(any(chExt)) { 
          fiN <- paste0(fi,compExt) %in% list.files(path=pa)    # search as strict
          fiN <- if(any(fiN)) paste0(fi,compExt)[which(fiN)] else NULL
        }
      }
    }
    if(length(fiN) <1 && argLe[2] >0 && argLe[3] >0 && compr) { 
      ## check if ext already in fi (while compr) : construct fi wo extension
      fi2 <- fi  
      if(compr) { chExt <- tools::file_ext(fi) %in% compExt
        if(chExt) { ex2 <- tools::file_ext(sub("\\.([[:alnum:]]+)$","",fi))       # access extension underneith compr
          chEx2 <- ex2 %in% ext && nchar(ex2) >0
          if(chEx2) {            ## extension (underneith compr found present); test only compr
            fiN <- paste0(fi, compExt) 
            fiN <- list.files(pa, pattern=paste0("^",fiN,"$"))
            chFi <- fiN %in% list.files(pa)
            if(length(fiN) >0) { if(!sil) message(caFr, if(debug) "cSF2 ","Found ",length(fiN)," matches, picking 1st ..")
              fiN <- fiN[1] }      
        } }
      }
    }
    if(debugFx) {message("cSF2"); cSF2 <- list(fiN=fiN,argLe=argLe,pa=pa,fi=fi,ext=ext)}

    ## nothing found as direct hit
    if(length(fiN) <1 && isFALSE(strict) && argLe[2] ==1) {
      ## look as pattern
      fi2 <- gsub("\\.","\\\\.", fi)
      fi3 <- gsub("\\.","\\\\.", paste0(fi, compExt))       # fi wo ext but with compr
      ex2 <- gsub("\\.","\\\\.", c(ext, if(compr) sapply(ext, paste, compExt)))    # add compressed versions
      fiN <- if(all(argLe >0)) list.files(pa, pattern=paste0(fi2,ex2)) else {if(all(argLe[-3] >0)) list.files(pa, pattern=fi2) }
      if(debugFx) {message("cSF3"); cSF3 <- list(pa=pa,fi=fi,ext=ext,argLe=argLe,fiN=fiN)}
        
      if(isFALSE(compr) && length(fiN) >0) {
        ## remove compressed from resuls if isFALSE(compr)   
        ch1 <- sapply(paste0(gsub("\\.","\\\\.", compExt),"$"), grepl, fiN)
        if(any(ch1)) { if(length(dim(ch1)) <1) fiN <- NULL else { ch2 <- rowSums(ch1) >0; if(any(ch2)) fiNa <- fiNa[-which(ch2)] }}
      }
      if(length(fiN) >0) { if(!sil) message(caFr, if(debug) "cSF3b ","Found ",length(fiN)," matches in pattern search, picking 1st ..")
        fiN <- fiN[1] }
    }
    ## final : add path (unless already good wdir)
    if(length(fiN) >0 && !("." %in% pa)) { fiN <- file.path(pa[1], fiN)}
  fiN }  
    
    
   if(debug) {message(fxNa,"cFP0b"); cFP0b <- list()}
                    
  ## check path
  path <- .NaRepl(path, repl=".")
   
  ## check for paths as existing
  chPa <- dir.exists(path)   
  if(any(!chPa)) { 
    if(!silent) message(fxNa, "path(s) ", pasteC(path[which(!chPa)], quoteC="'"),"'  not existing, ignoring...")
    path[which(!chPa)] <- "." }

  if(length(expectExt) <1) expectExt <- "" else if(any(is.na(expectExt))) expectExt <- ""  
  if(debug) {message(fxNa,"cFP1"); cFP1 <- list(fileName=fileName,path=path,expectExt=expectExt,mode=mode)}

  path2 <- .adLen(path, fileName)
 

  if(isTRUE(strictExtension)) mode <- c(mode, "strict")        # back compatobilty
  stri2 <- rep(if(any(c("strictExtension","strict","stri","str") %in% mode)) TRUE else FALSE, length(fileName))

  ext2 <- if(length(expectExt) >1) as.list(as.data.frame(matrix(rep(expectExt, length(fileName)), ncol=length(fileName)))) else {
    rep(if(length(expectExt) <1) FALSE else expectExt, length(fileName)) }
  if(debug) {message(fxNa,"cFP1c"); cFP1 <- list()}
  
  if(length(compressedOption)==1 && isTRUE(compressedOption)) mode <- unique(c(mode,"compressed"))        # stay back-compatible with deprecated argument (since wrMisc-1.15.0)... 
  comp2 <- if(any(c("compressedOption","compressed","compr","com") %in% mode)) rep(TRUE, length(fileName)) else rep(FALSE, length(fileName))
  if(any(c("compressed","compr","com") %in% names(mode))) {comp2 <- mode[which(c("compressed","compr","com") %in% names(mode))]
    comp2 <- if(length(comp2) >1) as.list(as.data.frame(matrix(rep(comp2, length(fileName)), ncol=length(fileName)))) else rep(comp2, length(fileName)) }  
  
  if(debug) {message(fxNa,"cFP1d"); cFP1d <- list(fileName=fileName,path=path,expectExt=expectExt,mode=mode,path2=path2,comp2=comp2,ext2=ext2,stri2=stri2)}
  
  ## main
  out <- mapply(.findSingleFile, fi=fileName, pa=path2, ext=ext2, strict=stri2, compr=comp2, deb=debug)
  if(debug) {message(fxNa,"cFP1e"); cFP1e <- list()}

  if(any(c("stop","stopIfNothing") %in% mode) && length(unlist(out, use.names=FALSE)) <1) stop(fxNa,"No files matching found for ", pasteC(fileName, quoteC="''"))
  if("byFile" %in% mode) {unlist(sapply(out, function(x) if(length(x) <1) NA else x[1]))} else unlist(out) 
  
}
  
