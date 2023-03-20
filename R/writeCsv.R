#' Write (and convert) csv files
#'
#' This functions is absed on \code{write.csv} allows for more options when writing data into csv-files.
#' The main input may be gven as R-object or read from file 'input'. Then, one can (re-)write using specified conversions. 
#' An optional filter to select columns (column-name specified via 'filterCol') is available. 
#' The output may be simultaneaously written to multiple formats, as specified in 'expTy', 
#' tabulation characters may be converted to avoid accidentally split/shift text to multiple columns. 
#' Note: Mixing '.' and ',' as comma separators via text-columns or fused text&data may cause problems lateron, though.
#'
#' @param input either matrix or data.frame
#' @param inPutFi (character or \code{NULL}) file-name to be read (format as US or Euro-type may specified via argument \code{imporTy})
#' @param expTy (character) 'US' and/or 'Eur' for sparator and decimal type in output
#' @param imporTy (character) default 'Eur' (otherwise set to 'US')
#' @param filename (character) optional new file name(s)
#' @param quote (logical) will be passed to function \code{write.csv}
#' @param filterCol (integer or character) optionally, to export only the columns specified here
#' @param replMatr optional, matrix (1st line:search, 2nd li:use for replacing) indicating which characters need to be replaced )
#' @param returnOut (logical) return output as object
#' @param SYLKprevent (logical) prevent difficulty when opening file via Excel. In some cases Excel presumes (by error) the SYLK format and produces an error when trying to open files : 
#'  To prevent this, if necessary, the 1st column-name will be changed from 'ID' to 'Id'.
#' @param digits (interger) limit number of signif digits in output (ie file)
#' @param silent (logical) suppress messages
#' @param debug (logical) for bug-tracking: more/enhanced messages  
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function writes a file to disk and returns \code{NULL} unless \code{returnOut=TRUE}
#' @seealso \code{write.csv} in \code{\link[utils]{write.table}}, batch reading using this package \code{\link{readCsvBatch}}
#' @examples
#' dat1 <- data.frame(ini=letters[1:5],x1=1:5,x2=11:15,t1=c("10,10","20.20","11,11","21,21","33.33"),
#'   t2=c("10,11","20.21","kl;kl","az,az","ze.ze"))
#' fiNa <- file.path(tempdir(), paste("test",1:2,".csv",sep=""))
#' writeCsv(dat1, filename=fiNa[1])
#' dir(path=tempdir(), pattern="cs")
#'
#' (writeCsv(dat1, replM=rbind(bad=c(";",","), replBy="__"), expTy=c("Eur"),
#'   returnOut=TRUE, filename=fiNa[2]))
#'
#' @export
writeCsv <- function(input, inPutFi=NULL, expTy=c("Eur","US"), imporTy="Eur", filename=NULL, quote=FALSE, filterCol=NULL, replMatr=NULL, returnOut=FALSE, SYLKprevent=TRUE, digits=22, silent=FALSE,debug=FALSE,callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="writeCsv")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  argN <- deparse(substitute(input))
  doWrite <- TRUE
  if(!requireNamespace("utils", quietly=TRUE)) { doWrite <- FALSE
    warning(fxNa,"package 'utils' not found ! Please install first") 
  }  
  if(length(input) <1) { doWrite <- FALSE; warning(fxNa," 'input'  should be data or filename")}
  
  if(doWrite) {
    if(is.character(input)) {if(file.exists(input)) {inPutFi <- as.character(dat)[1]; dat <- NULL
      if(!silent) message(fxNa,"trying to read  ",inPutFi,"  as format: ",imporTy)
        dat <- if(imporTy=="Eur") try(utils::read.csv2(inPutFi,stringsAsFactors=FALSE),silent=TRUE) else {
          if(imporTy=="US") try(utils::read.csv(inPutFi, stringsAsFactors=FALSE),silent=TRUE) else try(utils::read.table(inPutFi,stringsAsFactors=FALSE),silent=TRUE) }
        if("try-error" %in% class(dat)) {message(fxNa,"PROBLEM when trying tp open file '",inPutFi,"' - abandon"); doWrite <- FALSE }
      } else { dat <- input
        if(!silent) message(fxNa," 'input' is character but since no corresponding to existing filename, trying to interpret as data to be written to file")}
    } else  dat <- input
    if(doWrite) {
      if(!is.null(dat)) { if(length(inPutFi) >1) message(fxNa," ignoring content of 'inPutFi'")
         inPutFi <- NULL; imporTy <- ""}
      if(length(dim(dat)) <2) dat <- as.matrix(dat)     # fx created for typical case of data.frame or matrix
      datColCl <- rep(NA,ncol(dat))
      for(i in 1:ncol(dat)) datColCl[i] <- class(dat[,i])                            # document class for each column (won't work using apply)
      if(length(expTy) <1) {expTy <- "Eur"; if(!silent) message(fxNa,"unkown format for 'expTy', setting to 'Eur'")}
      expTy <- sort(stats::na.omit(expTy))
      if(length(expTy) <1) {expTy <- "Eur"; if(!silent) message(fxNa,"unkown format for 'expTy', setting to 'Eur'")}
      ## FILTERING  (independent to expTy)
      ## check if 'filterCol' in dat
      if(is.list(filterCol) && length(filterCol) >0) {
        useCol <- sapply(filterCol,function(x) x[1])
        useCol <- which(useCol %in% colnames(dat))
        if(!silent && length(useCol)<1) message(fxNa," none of the columns from 'filterCol' found in ",inPutFi)
        filtThr <- sapply(filterCol[useCol],function(x) if(length(x>1)) x[2] else NA)
        filterCol <- sapply(filterCol[useCol],function(x) x[1])
        }
      if(debug) {message(fxNa,"..xxWriteC0")}
      if(length(filterCol) >0) {
        for(i in 1:length(filterCol)) {                          
          chLogi <- TRUE                                         
          ## this may be further deveoped: check if column has usable logical content
          chLi <- if(is.na(filtThr[i]) && chLogi) which(dat[,filterCol[i]]) else which(dat[,filterCol[i]] < filtThr[i])
          if(length(chLi) <nrow(dat) & length(chLi) >0) dat <- dat[chLi,] else if(length(chLi) <1) {
            message(fxNa," filtering for ",filterCol[i]," nothing left !!")
            dat <- NULL; return(NULL) }}
      } else if(!is.character(inPutFi)) expTy <- expTy[which(!expTy  %in% imporTy)]        # no need to re-write same file if no filtering (unless inPutFi is filename)
      ##
      ## treat non-conform characters (only in non-numeric cols, dependent on expTy)
      ##  locate non-conform characters, then subtitute in non-numeric part of dat
      datExp <- list()
      if(!all(datColCl %in% c("numeric","integer"))){                               # nothing to substitute if only numeric data, otherwise :
        chCols <- which(!datColCl %in% c("numeric","integer"))
        dat0 <- as.matrix(dat[,chCols])
        ## first try to locate numeric cols with bad separator (digit+comma+digit)
        zz <- sub("^[[:digit:]]+,[[:digit:]]+$|^[[:digit:]]+\\.[[:digit:]]+$|^,[[:digit:]]+$|^\\.[[:digit:]]+$","", dat0)     # no need to test for digits only wo separators
        chNA <- is.na(zz)
        if(any(chNA)) zz[which(chNA)] <- 0
        toNum <- colSums(nchar(zz)) <1
        if(debug) {message(fxNa,"..xxWriteC1b")}
        if(any(toNum)) {
          if(!silent) message(fxNa," adjusting ",sum(toNum)," column(s) with Euro comma-separator")
          dat[,chCols[which(toNum)]] <- as.numeric(sub(",",".", dat[,chCols[which(toNum)]]))
          datColCl[chCols[which(toNum)]] <- "numeric"
          chCols <- which(!datColCl %in% c("numeric","integer")) }                   # refresh
        ## locate & replace 'bad' characters interfering with tabular separation -> need multiple versions
        if(debug) {message(fxNa,"..xxWriteC2")}
        if(length(chCols) >0) for(ty in expTy) {
          dat0 <- as.matrix(dat[,chCols])                                             # refresh
          replMat <- if(is.null(replMatr))  array(c(";"," ", ","," ", "\t"," "),            
             dim=c(2,1,3),dimnames=list(c("bad","subst"),c("sep1"),c("Eur","US","txt"))) else replMatr
          if(length(dim(replMat)) >2) {
            chTy <- ty %in% dimnames(replMat)[[3]]                    
            replMat <- as.matrix(if(!chTy) replMat[,,1] else replMat[,,which(ty==dimnames(replMat)[[3]])])}  # matrix with characters to test for (1st line) & 2nd line for replacing
          locCh <- lapply(replMat[1,], grep, dat0)
          locCh <- locCh[which(sapply(locCh, length) >0)]                                            # indexes where substitution should take place
          if(length(locCh) >0) for(i in 1:length(locCh)) dat0[locCh[[i]]] <- gsub(replMat[1,i],replMat[2,i],dat0[locCh[[i]]])
          if(debug) {message(fxNa,"..xxWriteC3")}
          if(length(expTy) >1) {datExp[[ty]] <- dat; datExp[[ty]][,chCols] <- dat0} else dat[,chCols] <- dat0 }}
      ## idea (future) -make optional non-redundant version ? allowing to replace completely .exportFilteredCSV()
      ##  prevent Excel trying SYLK format : replace 1st col from 'ID' to 'Id'
      if(SYLKprevent) {if(is.list(datExp)) { chID <- grep("^ID",sapply(datExp,function(x) colnames(x)[1])) >0
        if(any(chID)) for(i in which(chID)) colnames(datExp[[i]])[1] <- sub("^ID","Id",colnames(datExp[[i]])[1])
      } else { chID <- grep("^ID",colnames(datExp)[1])
        if(chID) colnames(datExp)[1] <- sub("^ID","Id",colnames(datExp)[1])}}
      ## write to file
      if(length(filename) <1) filename <- paste0(if(is.character(inPutFi)) sub("\\.csv$","",inPutFi) else argN,".",expTy,".csv")
      if(length(filename) < length(expTy)) {
        if(!silent) message(fxNa," adding type to name(s) of file(s) to be written")
        filename <- paste0(sub("\\.csv$","",filename),".",expTy,".csv")
        if("txt" %in% expTy) filename <- sub("\\.txt.\\csv$",".txt",filename)
        names(filename) <- expTy }
      if(debug) {message(fxNa,"..xxWriteC4")}
      if(!silent & any(file.exists(filename))) message(fxNa,"file(s) ",pasteC(filename[which(file.exists(filename))],quo="'")," will be overwritten !")
      if( "US" %in% expTy) tryW <- try(utils::write.csv(as.matrix(format(if(length(datExp)>0) datExp$US else dat,digits=digits)), filename["US"], row.names=FALSE, quote=quote),silent=silent)
      if("txt" %in% expTy) tryW <- try(utils::write.table(as.matrix(format(if(length(datExp)>0) datExp$txt else dat,digits=digits)), filename["txt"], row.names=FALSE, quote=quote),silent=silent)
      ## idea (relaed to problem when input is fused numeric&text): in case of Eur test all cols if factor/text and then (optional?) convert '.' to ','
      if("Eur" %in% expTy) tryW <- try(utils::write.csv2(as.matrix(format(if(length(datExp)>0) datExp$Eur else dat,digits=digits)), filename[1], row.names=FALSE, quote=quote),silent=silent)
      ## possibility to return values :
      if(returnOut) {if(length(expTy) <2) dat else datExp} } } }
       
