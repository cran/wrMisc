#' Btach reading of tabulated (text-) files
#'  
#' This function allows batch reading of tabulated text files. The files can be designed specifically, alternatively all files from a given directory can be read.
#' If package \code{data.table} is 
#' 
#' 
#' @param query (character) vector of file-names to be read, if \code{"."} all files will be read (no matter what their extension might be)
#' @param path (character) path for reading files, if \code{NULL} or \code{NA} the current directory will be used
#' @param dec (character, length=1) decimals to use, will be passed to \code{\link[data.table]{fread}} or \code{\link[utils]{read.delim}}
#' @param header (character, length=1) path for reading files, if \code{NULL} or \code{NA} the current directory will be used, will be passed to \code{\link[data.table]{fread}} or \code{\link[utils]{read.delim}}
#' @param strip.white (logical, length=1) Strips leading and trailing whitespaces of unquoted fields, will be passed to \code{\link[data.table]{fread}} or \code{\link[utils]{read.delim}}
#' @param blank.lines.skip (logical, length=1)  If \code{TRUE} blank lines in the input are ignored. will be passed to \code{\link[data.table]{fread}} or \code{\link[utils]{read.delim}}
#' @param fill (logical, length=1) If \code{TRUE} then in case the rows have unequal length, blank fields are implicitly filled, will be passed to \code{\link[data.table]{fread}} or \code{\link[utils]{read.delim}}
#' @param filtCol (integer, length=1) which columns should be used for filtering, if \code{NULL} or \code{NA} all data will be returned
#' @param filterAsInf (logical, length=1) filter as inferior or equal (\code{TRUE}) or superior or equal threshold \code{filtVal} 
#' @param filtVal (numeric, length=1) which numeric threshold should be used for filtering, if \code{NULL} or \code{NA} all data will be returned
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param debug (logical) display additional messages for debugging
#' @return list of data.frames
#' @seealso  \code{\link[data.table]{fread}}, \code{\link[utils]{read.delim}}, for reading csv : \code{\link{readCsvBatch}}
#' @examples
#' path1 <- system.file("extdata", package="wrMisc")
#' fiNa <-  c("a1.txt","a2.txt")
#' allTxt <- readTabulatedBatch(fiNa, path1)
#' str(allTxt)
#' 
#' @export
readTabulatedBatch <- function(query, path, dec=".", header="auto", strip.white=FALSE, blank.lines.skip=TRUE, fill=FALSE, 
  filtCol=2, filterAsInf=TRUE, filtVal=5000, silent=FALSE, callFrom=NULL, debug=FALSE) {
  ## read tabulated data from local download 
  fxNa <- wrMisc::.composeCallName(callFrom, newNa="readTabulatedBatch")
  ## check for packages  
  reqPa <- c("utils","wrMisc","data.table")
  chPa <- sapply(reqPa, requireNamespace, quietly=TRUE)
  if(any(!chPa[1:2])) stop("package(s) '",paste(reqPa[which(!chPa)], collapse="','"),"' not found ! Please install first from CRAN")  
  if(!chPa[2]) message(fxNa, "NOTE : package 'data.table' for fast reading of files absent, please consider installing from CRAN for faster reading")  
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  ## check path & file as valid entries
  msg <- "invalid entry for 'query'"
  if(length(query) <1) stop(msg) else {
    chNa <- is.na(query)
    if(all(chNa)) stop("All queries are NA !!") else if(any(chNa)) {
      if(!silent) message(fxNa," removing ",sum(chNa)," queries at NA-status")
      query <- query[which(!is.na(chNa))]}}
  chChar <- nchar(query) >0
  if(all(!chChar)) stop(msg) else if(any(!chChar)) { query <- query[which(chChar)] }
  if(length(path) >0) {
    chNaPa <- is.na(path)
    if(any(chNaPa)) path <- path[which(!chNaPa)]
  }
  ## combine file & path, main testing of existing
  if(length(path) >1 & length(path) != length(query)) { path <- NULL
    if(!silent) message(fxNa,"Length (of valid) entries of 'path' not matching length of queries ! ignoring path")}
  if(identical(".", query)) {
    if(debug) message(fxNa,"Reading all files from dir selected")
    chPa <- if(length(path) ==1) {if(is.na(path)) FALSE else file.exists(path)} else FALSE
    query <- list.files(if(chPa) path=path else ".")
    fiPa <- if(chPa) file.path(path, query) else query 
  } else {
    if(debug) message(fxNa,"Reading specific files selected")  
    fiPa <- if(length(path) >0) file.path(path, query) else query
    chfiPa <- file.exists(fiPa)
    if(all(!chfiPa)) warning(msg,"Can't find ANY of the files !") else {
      if(any(!chfiPa)) {   
        if(!silent) message(fxNa," removing ",sum(!chfiPa)," out of ",length(fiPa), " non-existing queries/files")
        query <- query[which(chfiPa)]
        fiPa <- fiPa[which(chfiPa)]}
  } }
     
  ## main reading of files (& integrated filtering)
  if(debug) message(fxNa,"Ready to start reading ",length(fiPa)," files using ",if(chPa[3]) "data.table::fread()" else "utils::read.delim()")
  if(length(fiPa) >0) {
    if(length(filtCol) ==1) {if(is.na(filtCol)) filtCol <- NULL } else filtCol <- NULL
    if(length(filtVal) ==1) {if(is.na(filtVal) | !is.numeric(filtVal)) {
      filtVal <- NULL; if(debug) message(fxNa,"Invalid 'filtVal'")}} else filtVal <- NULL
    if(identical(header,"auto") & !chPa[3]) header <- FALSE
    if(!isFALSE(filterAsInf)) filterAsInf <- TRUE
    datLi <- lapply(fiPa, function(x) { 
      y <- if(chPa[3]) {as.data.frame(data.table::fread(x, sep="\t", dec=dec, header=header,strip.white=strip.white, blank.lines.skip=blank.lines.skip,fill=fill))
        } else utils::read.delim(file=x, dec=dec, header=header, strip.white=strip.white, blank.lines.skip=blank.lines.skip, fill=fill)
      if(length(filtCol)==1) { if(ncol(y) < filtCol) filtCol <- NULL else if(!is.numeric(y[,filtCol])) filtCol <- NULL}
      if(length(filtVal)==1 & length(filtCol) ==1) y <- y[which( if(filterAsInf) y[,filtCol] <= filtVal else y[,filtCol] >= filtVal),]
      y })
    names(datLi) <- query
    datLi
}}
  