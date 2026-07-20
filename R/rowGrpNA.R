#' Count Number Of NAs Per Row And Group Of Columns
#' 
#' This functions allows easy counting of the number of NAs per row and group in data organized multiple sub-groups (in columns).
#'  
#' @details 
#' First of all, this function allows counting the number of \code{NAs} per line and sub-group of columns as defined by argument \code{grp}. 
#' Furthermore, the counting can be expressed relative to the size of the sub-groups, either as ratio or by giving the \code{NA}-count 
#' using \code{mode} and all results may also get collasped in as single (character) vector, however, the names of the groups do not get conserved.
#' 
#'   This function has several options via tehe argument 'mode':
#'     - \code{mode="simple"} will return simply the number of \code{NAs} per line and group as integer;
#'     - \code{mode="simpleCollapse"} will collapse all columns into single chain of characters with the number of \code{NAs} per line and group (as character);
#'     - \code{mode="complete"} will return number of \code{NAs} per line and group accompanied by the number of columns associated with the very group (as character);
#'     - \code{mode="completeCollapse"} will collapse all columns into single chain of characters with the number of \code{NAs} per line and group accompanied by the number of columns associated with the very group (as character);
#'     - \code{mode="ratio"} will return the ratio of \code{NAs} per line and group (as numeric);
#'     - \code{mode="ratioCollapse"} will collapse all columns into single chain of characters with the ratio of \code{NAs} per line and group(as character);
#' 
#' With \code{mode="complete"} or \code{mode="ratio"} the resulting column-names will state 'nNA.' plus 'ratio.' (if a ratio-mode was chosen). 
#'  	 
#' @param mat (matrix, data.frame, list or 'MArrayLM') data to count the number of \code{NAs}; 
#'   in case of furnishing a list or MArrayLM-class a list-element called 'isNA' will be used as \code{mat} or all \code{NA} positions of list-element called 'raw' 
#' @param grp (character or factor) defining which columns should be grouped (considered as replicates). If \code{grp} is not provided and \code{mat} is list or 'MArrayLM'-class containg a
#' list-element 'setup' (as produced with testing from package  \href{https://CRAN.R-project.org/package=wrProteo}{wrProteo} the group-association will be taken from there
#' 
#' @param initColOrder (logical) if\code{FALSE} the columns may be in (alphabetically) sorted order (and thus not in the order of argument \code{grp})
#' @param mode (character) allows to switch between different types of output :
#'   \code{mode="simple"} will return simply the number of \code{NAs} per line and group as integer;
#'   \code{mode="simpleCollapse"} will collapse all columns into single chain of characters with the number of \code{NAs} per line and group (as character);
#'   \code{mode="complete"} will return number of \code{NAs} per line and group accompanied by the number of columns associated with the very group (as character);
#'   \code{mode="completeCollapse"} will collapse all columns into single chain of characters with the number of \code{NAs} per line and group accompanied by the number of columns associated with the very group (as character);
#'   \code{mode="ratio"} will return the ratio of \code{NAs} per line and group (as numeric);
#'   \code{mode="ratioCollapse"} will collapse all columns into single chain of characters with the ratio of \code{NAs} per line and group(as character);
#' 
#' @param digits (integer, length=1) number of decimal digits from rounding (when ratios are calculated);
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#'  
#' @return This function returns a matrix (or numeric vector if all grouping falls in a single class or charcter-vector if all groups get collapsed in single chain of characters)  with number of \code{NA}s per group
#' @seealso \code{\link{rowGrpMeans}}, \code{\link{rowSds}}, \code{\link[base]{colSums}}
#' @examples
#' mat2 <- matrix(1:72, nrow=9, ncol=8, dimnames=list(letters[1:9],LETTERS[1:8]))
#' set.seed(2025); mat2[sample.int(prod(dim(mat2)), 2*nrow(mat2), replace=FALSE)] <- NA
#' set.seed(2026); grp2 <- as.factor(sample.int(n=3, size=ncol(mat2), replace=TRUE))
#'   table(grp2)
#' 
#' ## overal number of NAs per row
#' rowSums(is.na(mat2)) 
#' 
#' ## number of NAs per row and group
#' head(rowGrpNA(mat2, grp2))
#' head(rowGrpNA(mat2, grp2, mode="complete"))
#' head(rowGrpNA(mat2, grp2, mode="ratio"))   
#' 
#' ## mimick output from testing from package wrProteo
#' dat1 <- list(isNA =is.na(mat2), setup=list(grp=grp2))
#' head(rowGrpNA(dat1, mode="simple"))
#' head(rowGrpNA(dat1, mode="complete"))
#' head(rowGrpNA(dat1, mode="ratioCollapse"))
#' @export
rowGrpNA <- function(mat, grp=NULL, initColOrder=TRUE, mode="simple", digits=2, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## get number of NAs per line & group of replicates
  fxNa <- .composeCallName(callFrom, newNa="rowGrpNA")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) { silent <- FALSE } else { debug <- FALSE }
  namesXY <- c(deparse(substitute(mat)), deparse(substitute(grp)))
  chLst <- chLstNa <- NULL
  
  if(length(mat) !=0) {
    datOK <- TRUE
    lstObj <- length(mat) !=0 && ("MArrayLM" %in% class(mat) || "list" %in% class(mat)) && "setup" %in% names(mat) && length(mat$setup$grp) != 0   # && ("isNA" %in% names(mat) || "raw" %in% names(mat)) && 
    if(lstObj) { if(length(mat$isNA) ==0 && length(mat$raw) !=0) mat$isNA <- is.na(mat$raw) }
  } else { datOK <- lstObj <- FALSE }
  if(debug) {message("rGN1"); rGN1 <- list(mat=mat,grp=grp,initColOrder=initColOrder,mode=mode,digits=digits,datOK=datOK, lstObj=lstObj)}

  if(datOK) {
    chMo <- c("simple","complete","ratio")
    chMo <- c(chMo, paste0(chMo,"Collapse"))
    if(length(mode) ==0) {mode <- "simple"; if(debug) message(fxNa,"Setting 'mode' to default")}
    if(length(mode) !=1 || !mode %in% chMo) stop(fxNa,"Invalid entry for argument 'mode'")
  }  

  if(datOK) {
    chLstNa <- c("isNA","raw","quant","data")
    if(lstObj) { chLst <- match(chLstNa, names(mat))
      if(all(!chLst)) stop(fxNa,"Unable to find any of the list-elements 'isNA', 'raw' or 'quant' in main input '",namesXY,"'")
      #  matIni <- mat           # needed ??
      ## recuperate grp from list (if given not fittin)
      if(length(grp) == 0 && "grp" %in% names(mat$setup)) grp <- mat$setup$grp                                         # recuperate grp from $setup
      mat <- if(is.na(chLst[1])) is.na(mat[[naOmit(chLst)[1]]]) else mat[[chLst[1]]]    # mat as matrix of F/T (indicating results of is.na())
      ## check grp (main)      
      if(length(grp) != 0 && length(grp) != ncol(mat)) stop(fxNa,"Invalid $setup$grp information")      
    } else {
      if(length(dim(mat)) !=2) stop(fxNa,"Invalid '",namesXY[1],"' (must be matrix or data.frame)")
      if(!is.logical(mat)) mat <- is.na(mat)
      if(length(grp) != ncol(mat)) {
        grp <- as.factor(1: ncol(mat)) }
    }
    if(!is.factor(grp)) grp <- as.factor(grp)
    if(length(mat) ==0) { datOK <- FALSE
      warning(fxNa,"mat$isNA found but empty !  (nothing to do)") }      # reduce to main objects           
  } 
  if(debug) {message("rGN2"); rGN2 <- list(mat=mat,grp=grp,initColOrder=initColOrder,mode=mode,digits=digits,lstObj=lstObj,chLst=chLst,chLstNa=chLstNa,datOK=datOK)}

  if(datOK && all(is.na(mat))) { datOK <- FALSE ; warning(fxNa,"Bizzare entry in argument 'mat' :  Cannot handle when data is NA - nothing to do ..")} 
  if(datOK && any(length(dim(mat)) !=2, dim(mat) < 1)) {datOK <- FALSE; warning(fxNa,"Invalid argument 'mat'; must be matrix (or data.frame) with min 1 line and 1 column (returning NULL)")}
  if(datOK && length(grp) != ncol(mat)) stop(fxNa,"Length of 'grp' and number of columns of 'mat' do not match !")       
  
  if(datOK) {   
    if(is.data.frame(mat)) mat <- as.matrix(mat)
    ## main : basic count per group;  recognize all different case
    nDup <- sum(duplicated(grp)) 
    if(nDup ==0) out <- 0 + mat else {
      if(nDup == length(grp) -1) out <- rowSums(mat) else {
        ## standard case  (multiple groups, not all singlets)
        colGr <- split(1:ncol(mat), grp)  
        out <- sapply(colGr, function(cols) rowSums(mat[, cols, drop=FALSE]))
        out <- if(isTRUE(initColOrder)) out[, match(naOmit(unique(grp)), colnames(out))] else out     
        if(debug) {message("rGN3"); rGN3 <- list(out=out,mat=mat,grp=grp,initColOrder=initColOrder,mode=mode,digits=digits,lstObj=lstObj,chLst=chLst)}
      
      ## other options
        dimNa <- dimnames(out) <- list( rownames(mat), paste0(if(!grepl("^simple", mode)) "nNA.", if(grepl("^ratio",mode)) "rat.", colnames(out)))
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
   } } else { if(!silent) message(fxNa,"Entry is empty or invalid, returning NULL")}
  } 
     
   