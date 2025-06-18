#' Get first of repeated by column
#'
#' \code{get1stOfRepeatedByCol} sorts matrix 'mat' and extracts only 1st occurance of values in column 'sortBy'.
#' Returns then non-redundant matrix (ie for column 'sortBy', if 'markIfAmbig' specifies existing col, mark ambig there).
#' Note : problem when sortSupl or sortBy not present (or not intended for use)
#'
#' @param mat (matrix or data.frame) numeric vector to be tested
#' @param sortBy (character) column name for which elements should be made unique, numeric or character column; 'sortSupl' .. add'l colname to always select specific 1st)
#' @param sortSupl (character) default="ty"
#' @param asFirstLast (character,length=2) to force specific strings from coluln 'sortSupl' as first and last when selecting 1st of repeated terms, default=c("full","inter")
#' @param markIfAmbig (character,length=2) 1st will be set to 'TRUE' if ambiguous/repeated, 2nd will get (heading) prefix, default=c("ambig","seqNa")
#' @param asList (logical) to return list with non-redundant ('unique') and removed lines ('repeats') 
#' @param abmiPref (character) prefix to note ambiguous entries/terms, default="_"
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns depending on argumnet 'asList' either list with non-redundant ('unique') and removed lines ('repeats') 
#' @seealso \code{\link{firstOfRepeated}} for (more basic) treatment of simple vector, \code{\link{nonAmbiguousNum}} for numeric use (much faster !!!)
#' @examples
#' aa <- cbind(no=as.character(1:20),seq=sample(LETTERS[1:15],20,repl=TRUE),
#'   ty=sample(c("full","Nter","inter"),20,repl=TRUE),ambig=rep(NA,20),seqNa=1:20)
#' get1stOfRepeatedByCol(aa)
#' @export
get1stOfRepeatedByCol <- function(mat, sortBy="seq", sortSupl="ty", asFirstLast=c("full","inter"), markIfAmbig=c("ambig","seqNa"), asList=FALSE, abmiPref="_", silent=FALSE, debug=FALSE, callFrom=NULL){
  msg <- " 'mat' should be matrix or data.frame with >1 lines & >1 columns"
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(dim(mat)) !=2) stop(msg) else if(any(dim(mat) <2)) stop(msg)
  if(is.character(sortBy)) if(!sortBy %in% colnames(mat)) stop(" invalid 'sortBy'")     # check better to allow alternative use of vector instead of colname ??
  num <- if(is.numeric(mat[,sortBy])) mat[,sortBy] else as.numeric(as.factor(mat[,sortBy]))
  if(is.data.frame(mat)) mat <- as.matrix(mat)
  reps <- which(mat[,sortBy] %in% names(which(table(mat[,sortBy]) >1)))
  if(length(reps) >0){
    sel <- mat[reps,]  
    supl <- gsub(asFirstLast[1],"a",gsub(asFirstLast[2],"z",sel[,sortSupl]))                  # allow 'full' to come 1st & 'inter' as last
    sel <- sel[order(sel[,sortBy],supl),]
    selFi <- sel[which(diff(c(-1,as.numeric(as.factor(sel[,sortBy])))) >0),]                  # main selection of 1st of repeated
    if(length(dim(selFi))<2) selFi <- matrix(selFi,nrow=1,dimnames=list(NULL,names(selFi)))
    if(markIfAmbig[1] %in% colnames(mat)) selFi[,markIfAmbig[1]] <- TRUE                         # mark ambiguous as 'TRUE'
    if(markIfAmbig[2] %in% colnames(mat)) selFi[,markIfAmbig[2]] <- paste(abmiPref,selFi[,markIfAmbig[2]],sep="")
    out <- rbind(mat[-1*reps,],selFi)
    if(asList) out <- list(unique=out,repeats=mat[reps,])
  } else out <- if(asList) list(unique=out) else mat
  out }
   
