#' Count Number Of NAs Per Row And Group Of Columns
#' 
#' This functions allows easy counting of the number of NAs per row in data organized in multiple sub-groups as columns.
#'  
#' @details 
#' First of all, this function allows counting the number of \code{NAs} per line and sub-group of columns as defined by argument \code{grp}. 
#' Furthermore, the counting can be expressed relative to the size of the sub-groups, either as ratio or by giving the \code{NA}-count 
#' using \code{mode="ratio"} and the size of the sub-group using \code{mode="complete"} .
#' Finally, all results may get collaped in as single (character) vector, however, the names of the groups do not get conserved.
#' 
#' 
#' 	 
#' @param mat (matrix, data.frame, list or 'MArrayLM') data to count the number of \code{NAs}; 
#'   in case of furnishing a list or MArrayLM-class a list-element called 'isNA' will be used as \code{mat} or all \code{NA} positions of list-element called 'raw' 
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates). If \code{grp} is not provided and \code{mat} is list or 'MArrayLM'-class containg a
#' list-element 'setup' (as produced with testing from package  \href{https://CRAN.R-project.org/package=wrProteo}{wrProteo} the group-association will be taken from there
#' 
#' @param initColOrder (logical) if\code{FALSE} the columns may be in (alphabetically) sorted order
#' @param mode (character) switch between different types of output :
#'  - \code{mode="simple"} will return simply the number of \code{NAs} per line and group as integer
#'  - \code{mode="simpleCollapse"} will collapse all columns into single chain of characters with the number of \code{NAs} per line and group (as character)
#'  - \code{mode="complete"} will return number of \code{NAs} per line and group accompanied by the number of columns associated with the very group (as character)
#'  - \code{mode="completeCollapse"} will collapse all columns into single chain of characters with the number of \code{NAs} per line and group accompanied by the number of columns associated with the very group (as character)
#'  - \code{mode="ratio"} will return the ratio of \code{NAs} per line and group (as numeric)
#'  - \code{mode="ratioCollapse"} will collapse all columns into single chain of characters with the ratio of \code{NAs} per line and group(as character)
#' @param digits (integer, length=1) number of decimal digits from rounding (when ratios are calculated)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#'  
#' @return This function returns typically a matrix (or numeric vector if all grouping falls in a single class or charcter-vector if all groups get collapsed in single chain of characters)  with number of \code{NA}s per group
#' @seealso \code{\link{rowGrpMeans}}, \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' mat2 <- matrix(1, nrow=200, ncol=40)
#' mat2[sample.int(prod(dim(mat2)), nrow(mat2), replace=FALSE)] <- NA
#' grp2 <- as.factor(sample.int(n=8, size=ncol(mat2), replace=TRUE))
#'   table(grp2)
#' ## overal number of NAs per row
#' rowSums(is.na(mat2)) 
#' 
#' ## number of NAs per row and group
#' head(rowGrpNA(mat2, grp2))
#' head(rowGrpNA(mat2, grp2, mode="simple"))
#' head(rowGrpNA(mat2, grp2, mode="complete"))
#' 
#' ## mimick output from testing from package wrProteo
#' dat1 <- list(isNA =mat2, setup=list(grp=grp2))
#' head(rowGrpNA(dat1, mode="simple"))
#' head(rowGrpNA(dat1, mode="complete"))
#' head(rowGrpNA(dat1, mode="ratioCollapse"))
#' @export
rowGrpNA <- function(mat, grp=NULL, initColOrder=TRUE, mode="simple", digits=2, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## get number of NAs per line & group of replicates
  fxNa <- .composeCallName(callFrom, newNa="rowGrpNA")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  if(length(mat) !=0) {
    datOK <- TRUE
    lstObj <- length(mat) !=0 && ("MArrayLM" %in% class(mat) || "list" %in% class(mat)) && "setup" %in% names(mat) && ("isNA" %in% names(mat) || "raw" %in% names(mat)) && length(mat$setup$grp) != 0
    if(lstObj) { if(length(mat$isNA) ==0 && length(mat$raw) !=0) mat$isNA <- is.na(mat$raw) }
  } else { datOK <- lstObj <- FALSE }

  if(datOK) {
    if(lstObj && length(grp) != ncol(mat$isNA))  grp <- mat$setup$grp 
    if(length(grp) != ncol(if(lstObj) mat$isNA else mat)) {
      grp <- as.factor(1: ncol(if(lstObj) mat$isNA else mat))
      warning(fxNa,"misssing or invalid 'grp', assuming as all different groups") }
    if(!is.factor(grp)) grp <- as.factor(grp)
    if(lstObj) { matIni <- mat           # needed ??
       mat <- mat$isNA
      if(length(mat) ==0) {datOK <- FALSE
        warning(fxNa,"mat$isNA found but empty !") }}      # reduce to main objects    
  }  
  if(datOK && any(length(dim(mat)) !=2, dim(mat) < 1)) {datOK <- FALSE; warning(fxNa,"Invalid argument 'mat'; must be matrix (or data.frame) with min 1 line and 1 column (returning NULL)")}
  if(datOK && length(grp) != ncol(mat)) stop(fxNa,"Length of 'grp' and number of columns of 'mat' do not match !")       
  
  if(datOK) {   
    if(is.data.frame(mat)) mat <- as.matrix(mat)
    ## main : basic count per group;  recognize all different case
    if(sum(duplicated(grp)) ==0) out <- 0 + mat else {
      colGr <- split(1:ncol(mat), grp)  
      out <- sapply(colGr, function(cols) rowSums(is.na(mat[, cols]))) 
      out <- if(isTRUE(initColOrder)) out[,match(naOmit(unique(grp)), colnames(out))] else out }  
    
    ## other options
      dimNa <- dimnames(out) <- list( rownames(mat), paste0(colnames(out),".nNA", if(grepl("^ratio",mode)) "rat"))
      out <- switch(mode,
        simple = out,                 # simple counts
        simpleCollapse = do.call(paste, c(as.data.frame(out), sep=" ")), # all counts pasted in single column
        complete = matrix(paste(out, matrix(rep(table(grp), each=nrow(out)), nrow=nrow(out)), sep="/"), ncol=ncol(out)),              # counts/ n.samples
        completeCollapse= do.call(paste, c(as.data.frame(paste(out, matrix(rep(table(grp), each=nrow(out)), nrow=nrow(out)), sep="/")), sep=" ")),  # counts/ n.samples pasted in single column
        ratio = round(out / matrix(rep(table(grp), each=nrow(out)), nrow=nrow(out)), digits),                     # ratio counts/n.samples
        ratioCollapse = do.call(paste, c(as.data.frame(round(out / matrix(rep(table(grp), each=nrow(out)), nrow=nrow(out)), digits)), sep=" "))      # ratio counts/n.samples pasted in single column
      )
      if(length(dim(out))==2) dimnames(out) <- dimNa else names(out) <- dimNa[[1]]
    out }
  } 
   
