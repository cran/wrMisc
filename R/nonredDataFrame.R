#' Filter for unique elements
#'
#' \code{nonredDataFrame}  filters 'x' (list of char-vectors or char-vector) for elements unique (to 'ref' or if NULL to all 'x') and of character length. 
#' May be used for different 'accession' for same pep sequence (same 'peptide_id').
#' Note : made for treating data.frames, may be slightly slower than matrix equivalent
#' @param dataFr (data.frame) main inpput
#' @param useCol (character,length=2) comlumn names of 'dataFr' to use : 1st value designates where redundant values should be gathered; 2nd value designes column of which information should be concatenated 
#' @param sepCollapse (character) conatenation symbol
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a data.frame of filtered (fewer lines) with additional 2 columns 'nSamePep' (number of redundant entries) and 'concID' (concatenated content)
#' @seealso \code{\link{combineRedBasedOnCol}}, \code{\link{correctToUnique}}, \code{\link[base]{unique}}
#' @examples
#' df1 <- data.frame(cbind(xA=letters[1:5], xB=c("h","h","f","e","f"), xC=LETTERS[1:5])) 
#' nonredDataFrame(df1, useCol=c("xB","xC")) 
#' @export
nonredDataFrame <- function(dataFr, useCol=c(pepID="peptide_id", protID="accession", seq="sequence",mod="modifications"), sepCollapse="//", callFrom=NULL) {
  fxNa <- .composeCallName(callFrom,newNa="nonredDataFrame")
  chColNa <- useCol %in% colnames(dataFr)
  if(any(!chColNa)) message(" TROUBLE AHEAD : can't find ",pasteC(useCol[which(!chColNa)]))
  if(!is.data.frame(dataFr)) dataFr <- as.data.frame(dataFr,stringsAsFactors=FALSE)
  i <- which(colnames(dataFr)==useCol[1]); dataFr[,i] <- as.character(dataFr[,i])
  i <- which(colnames(dataFr)==useCol[2]); dataFr[,i] <- as.character(dataFr[,i])
  dataFr <- cbind(dataFr, nSamePep=1, concID="")
  class(dataFr[,ncol(dataFr)-1]) <- "integer"
  class(dataFr[,ncol(dataFr)]) <- "character"
  dataFr[,ncol(dataFr)] <- as.character(dataFr[,useCol[2]])
  dupL <- duplicated(dataFr[,useCol[1]], fromLast=TRUE)                        # search in col with petide_id
  if(any(dupL)) {
    dupB <- duplicated(dataFr[,useCol[1]], fromLast=FALSE)
    firOfRep <- which(dupL & !dupB)
    anyDup <- which(dupL | dupB)
    out <- matrix(unlist(tapply(dataFr[anyDup,useCol[2]],dataFr[anyDup,useCol[1]], function(x) c(length(x),paste(x,collapse=sepCollapse)))),
      ncol=2, byrow=TRUE, dimnames=list(dataFr[firOfRep,useCol[1]],c("n","protIDs")))  
    dataFr <- dataFr[-1*which(dupB),]                          # remove lines of mult repeated 
     replLi <- match(rownames(out),as.character(dataFr[,useCol[1]]) ) 
    dataFr[match(rownames(out),as.character(dataFr[,useCol[1]]) ),ncol(dataFr)-(1:0)] <- out 
    class(dataFr[,ncol(dataFr)-1]) <- "integer" }
  dataFr }    
     
