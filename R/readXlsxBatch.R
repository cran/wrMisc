#' Read batch of Excel xlsx-files
#'
#' \code{readXlsxBatch} reads data out of multiple xlsx files, the sheet indicated by 'sheetInd' will be considered. 
#' All files must have a very similar organization of data, as this is typically the case when high-throughput measurments are automatically saved while the screen progresses.
#' The file-names will be used to structure the data read.
#' By default all columns with text-content may be eliminated to extract the numeric part only, which may then get organized to a 3-dim array. 
#' NOTE : requires package \href{https://CRAN.R-project.org/package=xlsx}{xlsx} being installed !  Uses a considerable amount of RAM ! Reading multiple xlsx files does take some time.
#' 
#' @param fileNames (character) provide either explicit list of file-names to be read or leave \code{NULL} for reading all files ending with 'xlsx' in path specified with argument \code{path}
#' @param path (character) there may be a different path for each file
#' @param fileExtension (character) extension of files (default='xlsx')
#' @param excludeFiles (character) names of files to exclude (only used when reading all files of given directory)
#' @param sheetInd (integer) specify which sheet to extract (must be number, eg \code{sheetInd=2} will extract always the 2nd sheet (no matter the name)
#' @param checkFormat (logical) if \code{TRUE}: check header, remove empty columns, if rownames are increasing integeres it will searh for fisrt column with different entries to use as rownames
#' @param returnArray (logical) allows switching from array to list-output
#' @param columns (NULL or character) column-headers to be extracted (if specified, otherwise all columns will be extracted)
#' @param simpleNames (integer), if \code{NULL} all characters of fileNames will be maintained, otherwise allows truncating names (from beginning) to get to variable part (using .trimFromStart()), but keeping at least the number of charcters indicated by this argument
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return list
#' @seealso \code{\link[xlsx]{read.xlsx}}, for simple reading of xls-files under 32-bit R see also: \code{odbcConnectExcel} via \code{\link[RODBC]{odbcConnect}} 
#' @examples
#' \donttest{
#' path1 <- system.file("extdata",package="wrMisc")
#' fiNa <- c("pl01_1.xlsx","pl01_2.xlsx","pl02_1.xlsx","pl02_2.xlsx")
#' datAll <- readXlsxBatch(fiNa,path1)
#' str(datAll)
#' datAll2 <- readXlsxBatch(path=path1,silent=TRUE)
#' identical(datAll,datAll2)
#' }
#' @export
readXlsxBatch <- function(fileNames=NULL,path=".",fileExtension="xlsx",excludeFiles=NULL,sheetInd=1,checkFormat=TRUE,
  returnArray=TRUE,columns=c("Plate","Well","StainA"),simpleNames=3,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="readXlsxBatch")  
  if(is.null(path)) path <- "."
  chPath <- file.exists(path)
  if(!chPath) {message(fxNa," Cannot find path '",path,"' !  ... Setting to default='.'")}
  if(is.null(fileNames)) {
    ## automatic reading of all files in directory
    fileNames <- dir(path=path,pattern=paste(fileExtension,"$",sep=""))
    if(length(fileNames) <1) message(fxNa," Could not find ANY suitable files !!") else {
      if(!silent) message(fxNa," found ",length(fileNames)," files to extract (eg ",pasteC(utils::head(fileNames,3),quoteC="'"),")")}
    useFi <- file.path(path,fileNames)  
  } else {
    ## reading of specfied files in directory
    douPath <- grep(path,fileNames)
    useFi <- if(length(douPath) <1) file.path(path,fileNames) else fileNames
    checkFi <- file.exists(useFi)
    if(sum(!checkFi) >0) { 
      if(!silent) message(fxNa," could not find ",sum(!checkFi)," files out of ",length(useFi),
        "  (eg ",pasteC(utils::head(fileNames[which(!checkFi)],3),quoteC="'"),")")
      useFi <- fileNames[which(checkFi)] 
      fileNames <- fileNames[which(checkFi)] }}
  checkFi <- if(!is.null(excludeFiles)) grep(excludeFiles,fileNames) else NULL
  if(length(checkFi) >0) {
    if(!silent) message(fxNa," based on 'excludeFiles': excluding ",length(checkFi)," files (out of ",length(fileNames),")")
    useFi <- useFi[-1*checkFi]
    fileNames <- fileNames[-1*checkFi]}
  outL <- list()
  if(length(useFi) >0) {
    for(i in 1:length(useFi)) {
      tmp <- try(xlsx::read.xlsx(file=useFi[i],sheetIndex=sheetInd))
      if(class(tmp) == "try-error") message(fxNa," unable to read '",fileNames[i],"''",
        if(i==1) "\n   Check if xlsx package is installed !?!")
      if(checkFormat) {
        ## display messages only for first file (others are presumed to repeat...)
        tmp <- .inspectHeader(tmp,headNames=columns,silent=silent | i !=1,callFrom=fxNa)
        tmp <- .removeEmptyCol(tmp,fromBackOnly=FALSE,silent=silent | i !=1,callFrom=fxNa)
      }
      if(i ==1) {                         # (re)define new format based on 1st file after format-checking (ie remove empty cols, extract col of well-names,...)
        outDim <- dim(tmp)
        out <- if(returnArray) array(NA, dim=c(outDim[1],length(fileNames),outDim[2])) else NULL }
      if(!returnArray) outL[[i]] <- as.matrix(tmp) else {
        if(identical(dim(tmp),outDim)) out[,i,] <- as.matrix(tmp) else {
          message(i,"th file ''",fileNames[i],"'' does not have format consistent with previous files (",dim(tmp)[1]," rows & ",dim(tmp)[2]," cols) - omitting")
      } }
    }
    if(returnArray) {
      ## refine column-names in array
      chColNa <- length(unique(colnames(out)))==1
      arrNames <- if(chColNa) paste(colnames(out),sub("\\.xlsx$","",if(length(simpleNames) >1) .trimFromStart(fileNames,minNchar=simpleNames) else fileNames),sep=".") else colnames(out)
      dimnames(out) <- list(rownames(tmp), arrNames, colnames(tmp))
    } else { out <- outL
      ## refine names in list-output
      if(length(fileNames) !=length(out)) message(fxNa," Problem ?  Got ",length(fileNames)," fileNames  BUT ",length(out)," list-elements !")
      names(out) <- substr(fileNames,1,nchar(fileNames)-4)
      if(simpleNames) names(out) <- if(length(simpleNames) >1) .trimFromStart(fileNames,minNchar=simpleNames) else fileNames } 
  } else out <- NULL     
  out }
      
