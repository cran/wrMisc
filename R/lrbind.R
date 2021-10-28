#' rbind on lists
#'
#' rbind-like function to append list-elements containing matrixes (or data.frames) and return one long table. 
#' All list-elements must have same number of columns (and same types of classes in case of data.frames.
#' Simple vectors (as list-elements) will be considered as sigle lines for attaching.
#' @param lst (list, composed of multiple matrix or data.frames or simple vectors) main input (each list-element should have same number of columns, numeric vectors will be converted to number of columns of other columns/elements)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix or data.frame
#' @seealso \code{rbind} in \code{\link[base]{cbind}}
#' @examples
#' lst1 <- list(matrix(1:9, ncol=3, dimnames=list(letters[1:3],c("AA","BB","CC"))),
#'   11:13, matrix(51:56, ncol=3))
#' lrbind(lst1)
#' @export
lrbind <- function(lst, silent=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="lrbind")
  argN <- deparse(substitute(lst))
  if(!isTRUE(silent)) silent <- FALSE
  chDf <- sapply(lst, function(x) "data.frame" %in% class(x))
  if(any(chDf)) {
    iniCla <- sapply(lst[[which(chDf)[1]]], class)    # read types of classes of 1st data.frame
    for(i in which(chDf)) lst[[i]] <- as.matrix(lst[[i]])  
  }  
  chNum <- sapply(lst, function(x) any(c("numeric","integer") %in% class(x)))
  nCol <- if(any(!chNum)) ncol(lst[[which(!chNum)[1]]]) else min(sapply(lst, length), na.rm=TRUE)
  if(any(chNum)) for (i in which(chNum)) lst[[i]] <- matrix(lst[[i]], ncol=nCol)
  ldim <- sapply(lst, dim)
  if(length(unique(ldim[2,])) >1) stop("Bad dimension(s): each list-element should have same number of columns")
  outm <- matrix(nrow=sum(ldim[1,]), ncol=ldim[2,1])
  ldim <- rbind(ldim, cumsum(ldim[1,]))
  ldim <- rbind(ldim, c(1, ldim[3, -1*ncol(ldim)] +1))
  for(i in 1:ncol(ldim)) outm[ldim[4,i]:ldim[3,i],] <- lst[[i]]
  lstColn <- unlist(sapply(lst, colnames))
  if(!is.null(lstColn)) if(length(unique(lstColn)) ==ldim[2,1] & !silent) message(fxNa,"Col-names not homogenous or partially absent in ",argN)
  colnames(outm) <- colnames(lst[[1]])
  lstRown <- unlist(sapply(lst, rownames))
  if(!is.null(lstRown)) if(length(unique(lstRown)) ==ldim[3, ncol(ldim)]) {
    rownames(outm) <- unique(lstRown)} else if(!silent) message(fxNa,"Row-names not complete")
  if(any(chDf)) { outm <- as.data.frame(outm)
    chCla <- sapply(outm, class) ==iniCla
    if(any(!chCla)) for(i in which(!chCla)) class(outm[,i]) <- iniCla[i]}
  outm }   
   
