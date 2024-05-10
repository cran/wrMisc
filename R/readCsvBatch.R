#' Read batch of csv-files
#'
#' This function was designed to read screening data split in parts (with common structure) and saved to multiple files,  
#' to extract the numeric columns and to compile all (numeric) data to a single array (or list).  Some screening platforms save results while progressing 
#' through a pile of microtiter-plates separately. The organization of the resultant files is structured through file-names and all files have exactely the same organization of lines and columns/ 
#' European or US-formatted csv files can be read, if argument \code{fileFormat} is \code{NULL} both types will be tested, otherwise it allows to specify a given format.
#' The presence of headers (to be used as column-names) may be tested using \code{checkFormat}.
#'
#' @param fileNames (character) names of files to be read, if \code{NULL} all files fitting 'fileFormat'
#' @param path (character) where files should be read (folders should be written in R-style)
#' @param fileFormat (character) may be \code{NULL} (both US and European formats will be tried), 'Eur' or 'US' 
#' @param checkFormat (logical) if \code{TRUE}: check header, remove empty columns, 1st line if all empmty, set output format for each file to matrix, if rownames are increasing integeres try to use 2nd of 'columns' as rownames
#' @param returnArray (logical) allows switching from array to list-output
#' @param columns (NULL or character) column-headers to be extracted (if specified), 2nd value may be comlumn with rownames (if rownames are encountered as increasing rownames)
#' @param excludeFiles (character) names of files to exclude (only used when reading all files of given directory)
#' @param simpleNames (logical) allows truncating names (from beginning) to get to variable part (using .trimLeft()), but keeping 'minNamesLe'
#' @param minNamesLe (interger) min length of column-names if simpleNames=TRUE
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns an array (or list if \code{returnArray=FALSE}) of all numeric data read (numerical columns only) from individual files
#' @seealso \code{\link[utils]{read.table}}, \code{\link{writeCsv}}, \code{\link{readXlsxBatch}} 
#' @examples
#' path1 <- system.file("extdata", package="wrMisc")
#' fiNa <-  c("pl01_1.csv","pl01_2.csv","pl02_1.csv","pl02_2.csv")
#' datAll <- readCsvBatch(fiNa, path1)
#' str(datAll)
#' ## batch reading of all csv files in specified path :
#' datAll2 <- readCsvBatch(fileNames=NULL, path=path1, silent=TRUE)
#' @export
readCsvBatch <- function(fileNames=NULL, path=".", fileFormat="Eur", checkFormat=TRUE, returnArray=TRUE,
  columns=c("Plate","Well","StainA"), excludeFiles="All infected plates", simpleNames=TRUE, minNamesLe=4, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="readCsvBatch")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  HeaderInData <- if(is.null(columns)) FALSE else TRUE
  if(length(path) >0) {
    chPath <- dir.exists(path)
    if(!chPath) {path <- "."
      message(fxNa," Cannot find path '",path,"' !  ... Setting to default='.'")}
  } else path <- "." 
  if(is.null(fileNames)) {               # read batch of files
    fileNames <- file.path(path, dir(path=path, pattern="\\.csv$"))
    if(length(fileNames) <1) message(fxNa,"Could not find ANY suitable files !!") else {
      if(!silent) message(fxNa,"Batch mode: Found ",length(fileNames)," files to extract (eg ",pasteC(utils::head(fileNames,3),quoteC="'"),")")}
  } else {                              # read explicit set of files
    if(length(path) >0) { chPath <- grep(paste0("^",path), fileNames)  # check if path already given with filenames
      if(length(chPath) <1) fileNames <- file.path(path,fileNames)}
    checkFi <- file.exists(fileNames)
    if(any(!checkFi)) { 
      if(!silent) message(fxNa,"Could not find ",sum(!checkFi)," files (eg ",pasteC(utils::head(fileNames[which(!checkFi)],3),quoteC="'"),")")
      fileNames <- fileNames[which(checkFi)] }
  if(length(fileNames) >0) {
    ## treat files to exclude
    checkFi <- if(!is.null(excludeFiles)) grepl(excludeFiles, fileNames) else rep(FALSE, length(fileNames))
    if(any(!checkFi))
      if(!silent) message(fxNa,"Based on 'excludeFiles': excluding ",sum(checkFi)," files (out of ",length(fileNames),")")
      fileNames <- fileNames[which(!checkFi)]} 
  }  
  ## check package(s)
  reqPa <- c("utils")
  chPa <- sapply(reqPa, requireNamespace, quietly=TRUE)
  if(any(!chPa)) { if(!silent) message(fxNa,"Package '",reqPa[1],"' missing, please install first from CRAN  (returning NULL)")
    fileNames <- NULL    
  } 

  ## check if US or European format
  if(length(fileNames) >0) {
    if(!silent) message(fxNa,"Ready to start reading ",length(fileNames)," files;  expecting header(s) : ", HeaderInData)
    tmp1 <- if(!identical(fileFormat,"Eur")) try(utils::read.csv(fileNames[1], header=HeaderInData, stringsAsFactors=FALSE), silent=TRUE) else NULL
    tmp2 <- if(!identical(fileFormat,"US")) try(utils::read.csv2(fileNames[1], header=HeaderInData, stringsAsFactors=FALSE), silent=TRUE) else NULL
    cheF <- c(inherits(tmp1, "try-error"), inherits(tmp2, "try-error"))
    if(debug) message(fxNa,"Done with inital try of reading files")
    
    if(all(cheF)) { fileNames <- NULL
      warning(fxNa,"\n Problem with file format, can't read 1st file neither as European nor US-type CSV !!")    
    } else {
      chDim <- prod(dim(tmp1)) > prod(dim(tmp2))
      datDim <- if(chDim) dim(tmp1) else dim(tmp2)
      if(!silent) message(fxNa,"csv-format used ",if(chDim) "'US'" else "'Europe'","  with ",datDim[1]," lines & ",datDim[2]," cols")
    } }

  ## main reading
  if(length(fileNames) >0) {
    datL <- list()
    arrNames <- if(isTRUE(simpleNames)) .trimLeft(fileNames, minNchar=minNamesLe) else fileNames
    if(debug) message(fxNa,"Ready for main reading of  ",pasteC(utils::head(fileNames)))
    for(i in 1:length(fileNames)) {
      if(!silent) message(fxNa," ..reading file ",fileNames[i])
      curFileNa <- fileNames[i]
      tmp <- if(chDim) {
        utils::read.csv(curFileNa, header=HeaderInData, stringsAsFactors=FALSE)
      } else utils::read.csv2(curFileNa, header=HeaderInData, stringsAsFactors=FALSE) 
      if(isTRUE(checkFormat)) {
        tmp <- .inspectHeader(tmp, headNames=columns, silent=i !=1 | silent, callFrom=fxNa)
        tmp <- .removeEmptyCol(tmp, fromBackOnly=FALSE, silent=i !=1 | silent, callFrom=fxNa)
      }
      if(i ==1) {                         # (re)define new format based on 1st file after format-checking (ie remove empty cols, extract col of well-names,...)
        datDim <- dim(tmp)
        dat <- if(isTRUE(returnArray)) array(NA, dim=c(datDim[1],length(fileNames),datDim[2])) else NULL }
      if(!returnArray) datL[[i]] <- as.matrix(tmp) else {
        if(identical(dim(tmp),datDim)) dat[,i,] <- as.matrix(tmp) else {
          message(fxNa, i,"th file ",fileNames[i]," seems not to have format consistent with prev files (",dim(tmp)[1]," lines & ",dim(tmp)[2]," rows)")
      } }
    }
    if(sum(nchar(arrNames) <1) >0) {
      if(!silent) message(fxNa,"Some arrays seem to have no plate-name specified from within data, using file-names")
      arrNames <- substr(fileNames[i],1,nchar(fileNames[i])-4)}
    if(returnArray) {
      dimnames(dat) <- list(rownames(tmp), arrNames, colnames(tmp))
    } else { dat <- datL
      if(length(fileNames) !=length(dat)) message(fxNa,"Problem ?  Got ",length(fileNames)," fileNames  BUT ",length(dat)," list-elements !")
      names(dat) <- substr(fileNames,1,nchar(fileNames)-4)
      if(isTRUE(simpleNames)) names(dat) <- .trimLeft(names(dat), minNchar=minNamesLe) }
  dat } else NULL }


#' Inspect 'matr' and check if 1st line can be used/converted as header
#'
#' This function inspects 'matr' and check if 1st line can be used/converted as header.
#' If colnames of 'matr' are either NULL or 'V1',etc the 1st row will be tested if it contains any of the elements (if not, 1st line won't be used as new colnames)
#' If 'numericCheck'=TRUE, all columns will be tested if they can be converted to numeric
#' 
#' @param matr (matrix or data.frame) main input to be instected
#' @param headNames (character) column-names t look for
#' @param numericCheck (logical) allows reducing complexity by drawing for very long x or y
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix vector or data.frame similar to input
#' @seealso  \code{\link[utils]{head}} for looking at first few lines
#' @examples
#' ma1 <- matrix(letters[1:6], ncol=3, dimnames=list(NULL,c("ab","Plate","Well")))
#' .inspectHeader(ma1) 
#' 
#' @export
.inspectHeader <- function(matr, headNames=c("Plate","Well","StainA"), numericCheck=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## inspect 'matr' and check if 1st line can be used/converted as header
  ## if colnames of 'matr' are either NULL or 'V1',etc the 1st row will be tested if it contains any of the elements (if not, 1st line won't be used as new colnames)
  ## if'numericCheck'=TRUE, all columns will be tested if they can be converted to numeric
  extr1stLine <- TRUE
  fxNa <- .composeCallName(callFrom, newNa=".inspectHeader")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  msg <- character()
  if(!is.null(colnames(matr))) {if(identical(colnames(matr), paste0("V",1:ncol(matr)))) extr1stLine <- FALSE}
  if(extr1stLine & sum(tolower(headNames) %in% tolower(as.matrix(matr[1,]))) <1) extr1stLine <- FALSE
  if(extr1stLine) {
    chNewNa <- length(matr[1,]) ==length(unique(matr[1,]))
    if(!chNewNa && !silent) message(fxNa,"Transferring text to now column-names, but they are not unique !")
    matr <- as.data.frame(matr)
    colnames(matr) <- matr[1,]
    matr <- matr[-1,] }
  if(numericCheck) {
    ## try to locate 1st column that can be used as rownames (before removing text-content of non-nuleric columns)
    rowNa <- rownames(matr)
    if(is.null(rowNa) || identical(rowNa,as.character(1:nrow(matr)))) {
      chUniq <- apply(matr, 2, function(x) length(unique(x)) ==length(x))
      if(any(chUniq)) rowNa <- matr[,which(chUniq)[1]]
    }
    ## extract numerical values only (text will be converted to NA)
    matr <- apply(matr, 2, convToNum, spaceRemove=TRUE, convert=c(NA,"allChar"), remove=NULL,sciIncl=TRUE,euroStyle=TRUE,callFrom=fxNa,silent=silent)
    rownames(matr) <- rowNa }
  matr }
   
