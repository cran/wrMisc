#' Read batch of Excel xlsx-files
#'
#' \code{readXlsxBatch} reads data out of multiple xlsx files, the sheet indicated by 'sheetInd' will be considered. 
#' All files must have the same organization of data, as this is typically the case when high-throughput measurements are automatically saved while experiments progress.
#' In particular, the first file read is used to structure the output.
#' 
#' @details
#' By default all columns with text-content may be eliminated to keep the numeric part only, which may then get organized to a 3-dim numeric array 
#'  (where the additional files will be used as 2nd dimension and multiple columns per file shown as 3rd dimension). 
#' 
#' NOTE : (starting from version wrMisc-1.5.5) requires packages \href{https://CRAN.R-project.org/package=readxl}{readxl} and 
#' \href{https://CRAN.R-project.org/package=Rcpp}{Rcpp} being installed ! 
#' (This allows much faster and memory efficient processing than previous use of package '\code{xlsx}') 
#' 
#' @param fileNames (character) provide either explicit list of file-names to be read or leave \code{NULL} for reading all files ending with 'xlsx' in path specified with argument \code{path}
#' @param path (character) there may be a different path for each file
#' @param fileExtension (character) extension of files (default='\code{xlsx}')
#' @param excludeFiles (character) names of files to exclude (only used when reading all files of given directory)
#' @param sheetInd (character or integer) specify which sheet to extract (as exact name of sheed or sheet-number, eg \code{sheetInd=2} will extract always the 2nd sheet (no matter the name); if given as sheet-name but nor present in file an empty list-elements wil be returned
#' @param checkFormat (logical) if \code{TRUE}: check header, remove empty columns, if rownames are increasing integeres it will searh for fisrt column with different entries to use as rownames
#' @param returnArray (logical) allows switching from array to list-output
#' @param columns (NULL or character) column-headers to be extracted (if specified, otherwise all columns will be extracted)
#' @param simpleNames (integer), if \code{NULL} all characters of fileNames will be maintained, otherwise allows truncating names (from beginning) to get to variable part (using .trimFromStart()), but keeping at least the number of charcters indicated by this argument
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return list
#' @seealso \code{\link[readxl]{read_excel}}; for simple reading of (older) xls-files under 32-bit R one may also see the package \href{https://CRAN.R-project.org/package=RODBC}{RODBC} 
#' @examples
#' path1 <- system.file("extdata", package="wrMisc")
#' fiNa <- c("pl01_1.xlsx","pl01_2.xlsx","pl02_1.xlsx","pl02_2.xlsx")
#' datAll <- readXlsxBatch(fiNa, path1)
#' str(datAll)
#' ## Now let's read all xlsx files of directory
#' datAll2 <- readXlsxBatch(path=path1, silent=TRUE)
#' identical(datAll, datAll2)
#' @export
readXlsxBatch <- function(fileNames=NULL, path=".", fileExtension="xlsx", excludeFiles=NULL, sheetInd=1, checkFormat=TRUE,
  returnArray=TRUE, columns=c("Plate","Well","StainA"), simpleNames=3, silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="readXlsxBatch")  
  chPa <- c(class(try(find.package("readxl"), silent=TRUE)), class(try(find.package("Rcpp"), silent=TRUE)))
  if(any("try-error" %in% chPa)) { out <- NULL
    warning(fxNa,"package 'readxl' and/or 'Rcpp' not found ! Please install first")
  } else {
    ## prepare
    if(is.null(path)) path <- "."
    chPath <- file.exists(path)
    if(!chPath) {message(fxNa," Cannot find path '",path,"' !  ... Setting to default='.'")}
    if(is.null(fileNames)) {
      ## automatic reading of all files in directory
      fileNames <- dir(path=path,pattern=paste0(fileExtension,"$"))
      if(length(fileNames) <1) message(fxNa," Could not find ANY suitable files !!") else {
        if(isFALSE(silent)) message(fxNa," found ",length(fileNames)," files to extract (eg ",pasteC(utils::head(fileNames,3),quoteC="'"),")")}
      useFi <- file.path(path,fileNames)  
    } else {
      ## reading of specfied files in directory
      douPath <- grep(path,fileNames)
      useFi <- if(length(douPath) <1) file.path(path, fileNames) else fileNames
      checkFi <- file.exists(useFi)
      if(sum(!checkFi) >0) { 
        if(isFALSE(silent)) message(fxNa," could not find ",sum(!checkFi)," files out of ",length(useFi),
          "  (eg ",pasteC(utils::head(fileNames[which(!checkFi)],3),quoteC="'"),")")
        useFi <- fileNames[which(checkFi)] 
        fileNames <- fileNames[which(checkFi)] }}
    ## files to omit from reading, ie exclude
    checkFi <- if(!is.null(excludeFiles)) grep(excludeFiles, fileNames) else NULL
    if(length(checkFi) >0) {
      if(isFALSE(silent)) message(fxNa," based on 'excludeFiles': excluding ",length(checkFi)," files (out of ",length(fileNames),")")
      useFi <- useFi[-1*checkFi]
      fileNames <- fileNames[-1*checkFi]}
    if(!any("try-error" %in% chPa)) useFi <- NULL
    ## main reading
    outL <- list()
    if(length(useFi) >0) {
      for(i in 1:length(useFi)) {
        sheets <- try(readxl::excel_sheets(useFi[i]))
        if("try-error" %in% class(sheets)) { sheetInd <- NULL
          message(fxNa," unable to read '",fileNames[i],"' Check if you have sufficient rights to open the file !?!")
        } else {
          ## inspect for sheet to load
          if(is.numeric(sheetInd)) { sheetInd <- as.integer(sheetInd)
            if(length(sheets) < sheetInd | sheetInd <1) {  sheetInd <- NULL
          }} else sheetInd <- naOmit(match(sheetInd,sheets))
          if(isFALSE(silent) & length(sheetInd) <1) message(fxNa," unable to read '",fileNames[i],"', the sheet '",sheetInd,
            "' was not found (existing: ",pasteC(sheets,quoteC="'"),")")
        }  
        if(isFALSE(silent) & length(sheetInd) >1) message(fxNa," only '",sheets[sheetInd],"', ie first match of 'sheetInd' will be read !")
        ## read xls and xlsx
        tmp <- if(length(sheetInd)==1) readxl::read_excel(useFi[i], sheet=sheets[sheetInd]) else NULL   # may also use readxl::read_xlsx    
        if(length(tmp) >0) {
          outL[[i]] <- as.data.frame(tmp)  
          if(isTRUE(checkFormat)) {
            ## display messages only for first file (others are presumed to repeat...)
            silent2 <- isTRUE(silent) | i !=1
            outL[[i]] <- .inspectHeader(outL[[i]], headNames=columns,silent=silent2, callFrom=fxNa)
            outL[[i]] <- .removeEmptyCol(outL[[i]], fromBackOnly=FALSE,silent=silent2, callFrom=fxNa)
          }
          ## for case array-output : define object out with dimensions based on 1st file
          if(i ==1) {                         # (re)define new format based on 1st file after format-checking (ie remove empty cols, extract col of well-names,...)
            outDim <- dim(outL[[i]])       
            out <- if(isTRUE(returnArray)) array(NA, dim=c(outDim[1],length(fileNames),outDim[2]), 
              dimnames=list(rownames(outL[[i]]), basename(fileNames),colnames(outL[[i]]))) else NULL }
          ## for case array-output : fill directly into object out
          if(isFALSE(returnArray)) outL[[i]] <- as.matrix(outL[[i]]) else {
            if(identical(dim(outL[[i]]),outDim)) out[,i,] <- as.matrix(outL[[i]]) else {
              message(fxNa," Omit ",i,"th file ''",fileNames[i],"': format (",dim(outL[[i]])[1]," rows & ",dim(outL[[i]])[2]," cols) NOT consistent with previous files - omitting")
          } }
      }
      }
      names(outL) <- fileNames
      if(returnArray) {
        ## refine column-names in array
        chColNa <- length(unique(colnames(out)))==1
        arrNames <- if(chColNa) paste(colnames(out),sub("\\.xlsx$","",if(length(simpleNames) >1) .trimFromStart(fileNames,minNchar=simpleNames, callFrom=fxNa) else fileNames),sep=".") else colnames(out)
      } else { out <- outL
        ## refine names in list-output
        if(length(fileNames) !=length(out)) message(fxNa," Problem ?  Got ",length(fileNames)," fileNames  BUT ",length(out)," list-elements !")
        names(out) <- substr(basename(fileNames), 1, nchar(basename(fileNames)) -4)   # remove extesions
        if(simpleNames) names(out) <- if(length(simpleNames) >1) .trimFromStart(fileNames, minNchar=simpleNames, callFrom=fxNa) else fileNames } 
    } else out <- NULL }       
  out }
      
