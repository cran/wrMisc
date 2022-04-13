#' Convert Pairs of Node-Names to Non-Oriented Propensity Matrix 
#'
#' @description
#' Numerous network query tools produce a listing of pairs of nodes (with one pair of nodes per line). 
#' Using this function such a \code{matrix} (or \code{data.frame}) can be combined to this more comprehensive view as propensity-matrix.  
#' 
#' @details
#' Note, this has been primarily developed for undirected interaction networks, the resulting propensity-matrix does not show any orientation any more.   
#' In a number of applications (eg in protein-protein interaction networks, PPI) the resulting matrix may be rather sparse.    
#' 
#' @param mat (matrix) main input, matrix of interaction partners with each line as a separate pair of nodes; 
#'   the first two columns should contain identifiers of the nodes
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns matrix or data.frame
#' @seealso uses typically input from \code{\link{filterNetw}}
#' @examples
#' pairs3L <- matrix(LETTERS[c(1,3,3, 2,2,1)], ncol=2)    # loop of 3
#' (netw13pr <- pairsAsPropensMatr(pairs3L))              # as prop matr
#' 
#' @export
pairsAsPropensMatr <- function(mat, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## convert pairs of node-names (non-oriented) to propensity matrix  
  ## sparse matrix solution
  fxNa <- .composeCallName(callFrom, newNa="pairsAsPropensMatr")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  if(!is.matrix(mat)) mat <- try(as.matrix(mat), silent=TRUE)
  msg <- " 'mat'; must be matrix or data.frame with min 1 line and 2 col"
  if(inherits(mat, "try-error")) stop("Invalid input in",msg)
  if(any(length(dim(mat)) !=2, dim(mat) < 1:2)) stop("Invalid argument",msg)
  
  nodeNa <- sort(unique(as.character(mat[,1:2])))
  nodeIM <- matrix(match(as.character(mat[,1:2]), nodeNa), ncol=2)    # as matrix
  di <- matrix(0, nrow=length(nodeNa), ncol=length(nodeNa))
  for(i in 1:ncol(di)) { j <- which(nodeIM[,1]==i)
    if(length(j) >0) { di[i,nodeIM[j,2]] <- di[i,nodeIM[j,2]] +1
      di[nodeIM[j,2],i] <- di[nodeIM[j,2],i] +1 }
    }
  dimnames(di) <- list(seq(nrow(di)), seq(nrow(di))) 
  di }
  
