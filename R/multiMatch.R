#' Simple Multi-to-Multi Matching of (Concatenated) Terms
#'
#' This function allows convenient matching of multi-to-multi relationships between two objects/vectors. 
#' It was designed for finding common elements in multiple to multiple matching situations (eg when comparing \code{c("aa; bb", "cc")}  to \code{c("bb; ab","dd")}, 
#' ie to find 'bb' as matching between both objects).
#'  
#' @param x (vector or list) first object to compare; if vector, the (partially) concatenated identifyers (will be split using separator \code{sep}), or list of items to be matched (ie already split) 
#' @param y (vector or list) second object to compare; if vector, the (partially) concatenated identifyers (will be split using separator \code{sep}), or list of items to be matched (ie already split) 
#' @param sep (character, length=1) separator used to split concatenated identifyers (if \code{x} or  \code{y} is vector)
#' @param sep2 (character, length=1) optional separator used when \code{method="matched"} to concatenate all indexes of \code{y} for column \code{y.allInd}
#' @param method (character) mode of operation: 'asIndex' to return index of y (those hwo have matches) with names of x (which x are the correpsonding match)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#'
#' @details
#' \code{method='byX'} .. returns data.frame with view oriented towards entries of \code{x}: character column \code{x} for entire content of \code{x}; integer column \code{x.Ind} for index of \code{x}; 
#'  character column \code{TagBest} for most frequent matching isolated tag/ID; integer column \code{y.IndBest} index of most frequent matching \code{y};
#'  character column \code{y.IndAll} index for all \code{y} matching any of the tags;
#'  character column \code{y.Match} for entire content of best matching \code{y}; 
#'  character column \code{y.Adj} for \code{y} adjusted to best matching \code{y} for easier subsequent perfect matching.
#'                                                                                                                                                       
#' \code{method=c("byX","filter")} .. combinded argument to keep only lines with any matches 
#'                                                                                                                                                       
#' \code{method='byTag'} .. returns matrix (of integers) from view of isolated tags from \code{x} (a separate line for each tag from  \code{x} matching to \code{y});
#' 
#' \code{method=c("byTag","filter")} ..if combined as arguments, this will return a data.frame for all unique tags with any matches between \code{x} and \code{y}, with 
#'   additional colunms \code{x.AllInd} for all matching \code{x}-indexes,  \code{y.IndBest} best matching \code{y} index; \code{x.n} for number of different \code{x} conatining this tag;
#' \code{y.AllInd} for all matching \code{y}-indexes 
#' 
#' \code{method='adjustXtoY'} .. returns vector with \code{x} adjusted to \code{y}, ie those elements of \code{x} matching are replace by the exact corresponding term of \code{y}.
#'                                                                                                                                                       
#' \code{method=NULL} .. If no term matching the options shown above is given, another version of 'asIndex' is returned, but indexes to \code{y} _after_ spliting by \code{sep}. 
#' Again, this method can be filtered by using \code{method="filter"} to focus on the best matches to \code{x}.  
#'                                                                                                                                                       
#' @return matrix, data.frame or list with matching results depending on \code{method} chosen
#' @seealso \code{\link[base]{match}}; \code{\link[base]{strsplit}}
#'                                                                                                                                                       
#' @examples
#' aa <- c("m","k", "j; aa", "m; aa; bb; o; ee", "n; dd; cc", "aa", "cc")
#' bb <- c("dd; r", "aa", "ee; bb; q; cc", "p; cc")
#' (match1 <- multiMatch(aa, bb, method=NULL))      # match bb to aa
#' (match2 <- multiMatch(aa, bb, method="byX"))     # match bb to aa
#' (match3 <- multiMatch(aa, bb, method="byTag"))   # match bb to aa
#' (match4 <- multiMatch(aa, bb, method=c("byTag","filter")))   # match bb to aa
#' 
#' @export
multiMatch <- function(x, y, sep="; ", sep2=NULL, method="byX", silent=FALSE, callFrom=NULL) {
  ## for finding common in multiple to multiple matching (eg c("aa; bb", "cc")  vs c("bb; ab","dd"))
  ## tells which x (names of output) was found matching (at least by 1 subunit) to which (part of) y, names of y tell which x (index)
  ## note: multiple matches are ignored (only 1st match reported)
  fxNa <- .composeCallName(callFrom, newNa="multiMatch")
  doMa <- TRUE
  if(length(x) <1) {doMa <- FALSE; if(!silent) message(fxNa,"argument 'x' is empty, nothing to do !")}
  if(length(y) <1) {doMa <- FALSE; if(!silent) message(fxNa,"argument 'y' is empty, nothing to do !")}
  out <- outF <- NULL
  if(doMa) {
    if(is.list(x)) {xL <- x; x <- sapply(x, paste, collapse=sep)} else xL <- strsplit(x, sep)
    if(is.list(y)) {yL <- y; y <- sapply(y, paste, collapse=sep)} else yL <- strsplit(y, sep)
    names(xL) <- 1:length(x)
    yV <- unlist(yL)
    names(yV) <- rep(1:length(y), sapply(yL,length))
    if(length(sep2) !=1) sep2 <- sep
    
    ## 'asIndex' .. return list (length matches x) where values indicate index of which y is matched by which elment of x (after split)
    ## check each x; determine  that in 3rd x 'aa' matches to 1st of 1st y; in 4th x 'aa' 
    out <- lapply(xL, function(z) {w <- match(yV,z); v <- !is.na(w); if(any(!v)) { v <- yV[which(v)]; u <- as.integer(names(v)); names(u) <- v;
      chDu <- duplicated(paste(names(u),u)); if(any(chDu)) u <- u[which(!chDu)]; u } else NULL })
    
    ## filter1 : filter to most frequent (or first of same freq)
    if(length(grep("^filter",method)) >0 & !"byTag" %in% method) out <- outF <- lapply(out, function(z) if(length(z) >1) z[which(z ==names(which.max(table(z))) )] else z)    

    ## by isolated tag/ID
    if("byTag" %in% method) { nLi <- sapply(out,length); tmp <- rep(nLi,nLi)
      mat <- cbind(x.Ind=as.integer(names(tmp)), yInd=unlist(out))
      rownames(mat) <- as.character(unlist(sapply(out,names)))
      chNa <- is.na(mat[,2])
      if(any(chNa)) mat <- mat[which(!chNa),]
      mat <- mat[order(rownames(mat), mat[,1]),]
      if(length(grep("^filter",method)) >0) {
        xV <- unlist(xL)
        tmX <- tapply(mat[,1], rownames(mat), function(z) c(IndBest=z[which.max(z)], n=length(unique(z)), allInd=paste(unique(z),collapse=sep2)))
        tmY <- tapply(mat[,2], rownames(mat), function(z) c(IndBest=z[which.max(z)], n=length(unique(z)), allInd=paste(unique(z),collapse=sep2)))
        tmp <- cbind(matrix(unlist(tmX), nrow=length(tmX), byrow=TRUE), matrix(unlist(tmY), nrow=length(tmY), byrow=TRUE))
        dimnames(tmp) <- list(sort(unique(rownames(mat))), paste(rep(c("x","y"),each=3),rep(c("IndBest","n","AllInd"),2),sep="."))
        mat <- data.frame(x.IndBest=as.integer(tmp[,1]), x.n=as.integer(tmp[,2]), x.AllInd=tmp[,3], 
          y.IndBest=as.integer(tmp[,4]), x.n=as.integer(tmp[,5]), y.AllInd=tmp[,6])
      }
      out <- mat }
    if("byX" %in% method) { 
      df1 <- data.frame(x=x,x.Ind=1:length(x), TagBest=NA, y.IndBest=NA, y.IndAll=NA, y.Match=NA, y.Adj=NA)
      nLi <- sapply(out,length)
      df1[which(nLi >0),"y.IndAll"] <- sapply(out[which(nLi >0)], function(z) paste(z, collapse=sep2))
      ## for IndBest
      outF <- if(length(outF) <1 | any(sapply(outF,length) >1)) lapply(out, 
        function(z) if(length(z) >1) z[which( z ==names(which.max(table(z))) )][1] else z[1]) else outF 
      df1[which(nLi >0),"y.IndBest"] <- as.integer(sapply(outF[which(nLi >0)], function(z) paste(z, collapse=sep2)))
      df1[which(nLi >0),"TagBest"] <- naOmit(sapply(outF, function(z) names(z)[1]))
      df1[which(nLi >0),"y.Match"] <- y[df1[which(nLi >0),"y.IndBest"]]
      df1[which(nLi >0),"y.Adj"] <- df1[which(nLi >0),"x"]
      if(length(grep("^filter", method)) >0) df1 <- df1[which(!is.na(df1[,"TagBest"])),]      
      out <- df1 }
    if(is.list(out) & length(grep("^filter", method)) >0) out <- out[which(sapply(out,length) >0)]  
  }      
  out }   
     
