#' Convert pairs of node-names to propensity matrix (non-oriented)
#'  
#' This function allows converting pairs of node-names to a propensity matrix (non-oriented).
#' 
#' 
#' @param mat (matrix or data.frame, 2 columns) main input, pairs of nodes describing one edge per line
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return (symmetrix) matrix with numeric content based on counting occurances a given node has edges conected to it
#' @seealso  in \code{\link[base]{cbind}}
#' @examples
#' pairs3L <- matrix(LETTERS[c(1,3,3, 2,2,1)], ncol=2)      # loop of 3
#' (netw13pr <- pairsAsPropensMatr(pairs3L))   # as prop matr
#' 
#' @export
pairsAsPropensMatr <- function(mat, silent=FALSE, callFrom=NULL) {
  ## convert pairs of node-names (non-oriented) to propensity matrix
  fxNa <- .composeCallName(callFrom, newNa="pairsAsPropensMatr")
  if(any(length(dim(mat)) !=2, dim(mat) < 1:2)) stop("Invalid content of argument 'mat'; must be matrix or data.frame with min 1 lines and 2 cols")
  
  ## sparse matrix solution
  nodeNa <- sort(unique(as.character(mat[,1:2])))
  nodeIM <- matrix(match(as.character(mat[,1:2]), nodeNa), ncol=2)    # as matrix
  di <- matrix(0, nrow=length(nodeNa), ncol=length(nodeNa))
  for(i in 1:ncol(di)) { j <- which(nodeIM[,1]==i)
    if(length(j) >0) { di[i,nodeIM[j,2]] <- di[i,nodeIM[j,2]] +1
      di[nodeIM[j,2],i] <- di[nodeIM[j,2],i] +1 }
    }
  dimnames(di) <- list(seq(nrow(di)), seq(nrow(di))) 
  di }
  
