#' Normalize Blockwise Two Or Three Datasets 
#'
#' @description
#' This function provides for normalizing 2 entire data-sets against each other while preserving each set's characteristics.
#' Several methods are possible: normalize blocks to common range, blocks to common median, to common distribution per block (quantile-normalization)
#' 
#' @param x (matrix or data.frame) first data-set (must be at least 2 columns and 3 lines)
#' @param y (matrix or data.frame) second data-set (must be at least 2 columns and 3 lines)
#' @param z (\code{NULL} or matrix or data.frame) optronal 3rd data-set (if not \code{NULL} at least 2 columns and 3 lines)
#' @param method Character string or function specifying the normalization method:
#'   - `"quantile.block"` (default): Aligns the distributions of all input datasets
#'     to a **common target distribution** (the mean of sorted values at each quantile).
#'     After normalization, `sort(x)`, `sort(y)`, and `sort(z)` (if provided) are **identical**.
#'     This is useful for making datasets directly comparable.
#'   - `"rescale"`: Rescales each dataset **independently** to the range specified by `range`.
#'     Preserves the shape of each distribution but standardizes the scale (e.g., to `[0, 1]`).
#'   - `"median"`: This precedure works in a proportional matter. It centers each dataset by deviding by its group median and multiplying by the overall median.
#'     This removes location differences while preserving spread.
#'   - `"logMedian"`: This precedure is adoped for log-data. It centers each dataset by subtracting its group-median and addding the overall median.
#'     Removes location differences while preserving spread.
#'   - `"none"`: No normalization
#' @param range (numeric vector, length=2) vector specifying the target values to be used in case \code{method="rescale"} for rescaling;
#'   Default values 0 and 1 point to the min and max values to be adjusted to the value specified in this argument, ie the 'range'.
#'   When argument \code{method="q"} is other than 0 and 1, the respective quantiles will get adjusted to the values of this arument, other data will treated in a linear fashion.
#' @param q (numeric vector, length=2) vector specifying the quantiles to be used in case \code{method="rescale"} for rescaling;
#'   Default values 0 and 1 point to the min and max values to be adjusted to the value specified in this argument      
#' 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' 
#' @return This function returns a list of the normalized sets for x and y (and z if given)
#' @seealso  \code{\link{normalizeThis}}
#' 

#'
#' @details
#' Main methods for choice :
#'
#' - quantile.block:
#'   This method runs a quantile-normalization on each block (but NOT on each column).
#'   Thus, all datasets will get the same (overall) distributions.  
#'
#' - rescale:
#'   Apply a linear transformation to each dataset to fit within given `range`.
#'
#' - median:
#'   Normalize each block to get the same overall median (individual columns may deviate, the original order is preserved).
#'
#' - logMedian:
#'   This procedure is an median normalization adopted to log-data. Instead of dividing and multiplying per-group medians will be subtracted and the target median will be added.
#'
#' - none: Besides, it is also possible to not do any normalization, the output will be identical to the input
#'
#' NA Handling:
#' For all approaches \code{NAs} will get ignored.  
#' With \code{method='quantile.block'} all precise positions where there is an NA in any of the data x, y or z will be ignored to
#' maintain equal numbers of data. 
#' This is due to the fact that regular quantile-normalization requires equal numbers of data per column 
#' (here one dataset is treated like a column in regular normalization).
#' This means that the dataset with the highest number of \code{NAs} will indirectly has the capacity 
#' to mask valid data in other data-sets.
#' 
#' @examples
#' ## Basic usage with vectors 
#' x <- c(1, 5, 3, 7, 2)
#' y <- c(10, 20, 30, 40, 50)
#' ## Align distributions (default: x and y will have identical distributions)
#' norm1 <- blockNormalize(x, y)
#' table(sort(norm1$x) == sort(norm1$y))  # all TRUE with 'quantile.block'
#'
#' ## matrix-example (like with omics-data)
#' set.seed(2026); mat1 <- matrix(rnorm(70), nrow=10) *5  # 10 lines x 10 samples
#' set.seed(2025); mat2 <- matrix(rnorm(70, mean=5), nrow=10)^2 -15
#' mat2[which(mat2 < 1.8)] <- mat2[which(mat2 < 1.8)] + 32
#' norm2 <- blockNormalize(mat1, mat2, method="rescale", range=c(0.1, 10))
#' sapply(norm2, range)
#' norm3 <- blockNormalize(mat1, mat2, method="median")
#' sapply(norm3, quantile, c(0.25,0.5,0.75), na.rm=TRUE)
#' norm4 <- blockNormalize(mat1, mat2, method="quantile.block")
#' sapply(norm4, quantile, c(0.25,0.5,0.75), na.rm=TRUE)
#' 
#' ## the resulting distribution 
#' layout(matrix(1:4, ncol=2))
#' boxplot(cbind(mat1, NA, mat2), main="initial", las=1)
#' boxplot(cbind(norm2$x, NA, norm2$y), main="rescale block", las=1)
#' boxplot(cbind(norm3$x, NA, norm3$y), main="median block", las=1)
#' boxplot(cbind(norm4$x, NA, norm4$y), main="quantile.block", las=1)
#' 
#' ## the overall distribution of blocks
#' layout(matrix(1:4, ncol=2))
#' boxplot(cbind(mat1=as.numeric(mat1), mat2=as.numeric(mat2)), main="initial (overall)",las=1)
#' boxplot(cbind(mat1=as.numeric(norm2$x), mat2=as.numeric(norm2$x)), 
#'   main="rescale block norm (overall)",las=1)
#' boxplot(cbind(mat1=as.numeric(norm3$x), mat2=as.numeric(norm3$x)), 
#'   main="median block norm (overall)",las=1)
#' boxplot(cbind(mat1=as.numeric(norm4$x), mat2=as.numeric(norm4$x)), 
#'   main="quantile.block norm (overall)",las=1)
#' 
#'
#' @seealso \code{\link{normalizeThis}} for normalizing all columns of single data-set, \code{\link[stats]{quantile}},  \code{\link[base]{scale}} for standard scaling
#' 
#'
#' @export
blockNormalize <- function(x, y, z=NULL, method="quantile.block", range=c(0, 1), q=c(0,1), silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="blockNormalize")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  
  ## check input
  stopifnot(
    is.numeric(x), is.numeric(y), !is.null(x),
    length(x) == length(y),
    is.null(z) || (is.numeric(z) && length(z) == length(x))
  )
  method <- match.arg(method, c("quantile.block", "rescale", "median", "logMedian", "none"))       

  ## functions 
  replaceNA <- function(dat) {  # identify NAs, replace NAs 
    if(is.null(dat)) return(list(values=NULL, isNA=NULL, newDim=NULL))
    isNA <- is.na(dat)
    if(!any(isNA)) {
      return(list(values=dat, isNA=isNA, newDim=dim(dat)))
    }
    if(is.matrix(dat)) {
      roMeans <- rowMeans(dat, na.rm=TRUE)
      dat[isNA] <- roMeans[isNA]
    } else {
      dat[isNA] <- mean(dat, na.rm=TRUE)
    }
    list(values=dat, isNA=isNA, newDim=dim(dat))
  }

  
  medNormXYZ <- function(dat1, dat2, dat3) {  # median normalize to common median
    med <- c(m1=stats::median(dat1, na.rm=TRUE), m2=stats::median(dat2, na.rm=TRUE), m3=if(length(dat3) !=0) stats::median(dat3, na.rm=TRUE))
    medN <- stats::median(med, na.rm=TRUE)
    out <- if(!is.na(medN)) list(x=dat1*medN/med[1], y=dat2*medN/med[2], if(length(dat3) !=0) z=dat3*medN/med[3])
  }

  medNormLogXYZ <- function(dat1, dat2, dat3) {  # median normalize to common median
    med <- c(m1=stats::median(dat1, na.rm=TRUE), m2=stats::median(dat2, na.rm=TRUE), m3=if(length(dat3) !=0) stats::median(dat3, na.rm=TRUE))
    medN <- stats::median(med, na.rm=TRUE)
    out <- if(!is.na(medN)) list(x=dat1 -med[1] +medN, y=dat2 -medN +med[2], if(length(dat3) !=0) z=dat3 -med[2] +medN)
  }

  ## Main : Process x, y, z 
  x2 <- replaceNA(x)
  y2 <- replaceNA(y)
  z2 <- if(!is.null(z)) replaceNA(z) else list(values=NULL, isNA=NULL, newDim=NULL)
  x <- x2$values
  y <- y2$values
  z <- z2$values
  if(debug) {message(fxNa,"bNo1")}
      
  out <- switch( method,
    "none"= list(x, y, z),

    ## quantNormBlock logic 
    "quantile.block"={
      useAllData =TRUE   # use all data for qqN: impute NAs (my median), qqN (ensured equal n), reset init NA-positions to NA as alternative to remove points/positions with any NAs amonst the 2 or 3 data-sets

      isNA <- list(x=is.na(x), y=is.na(y))
      if(!is.null(z)) isNA$z <- is.na(z)
      if(debug) message(fxNa,"mNo1 quantile.block,  length isNA ",length(isNA))

      ## check content of NAs
      sumNa <- sapply(isNA, sum)
      leDat <- sapply(isNA, function(xx) if(is.data.frame(xx)) prod(dim(xx)) else length(x) )
      ratNa <- sumNa / leDat 
      if(any(ratNa > 0.7) || min(ratNa) >0.3) { useAllData <- FALSE
        if(!silent && isTRUE(useAllData)) message(fxNa,"Note : Data have too many NAs (min > 30% or any >70%) for mode useAllData =TRUE") 
      } else if(any(ratNa > 0.3) && !silent)  message(fxNa,"Note : Data have elevated rate of NAs  ( ",100*round(pasteC(ratNa),2),"%)")  

      if(isTRUE(useAllData)) {
        ## (temporarily) replace NAs
        if(any(isNA$x)) x[which(isNA$x)] <- stats::median(x, na.rm=TRUE)
        if(any(isNA$y)) y[which(isNA$y)] <- stats::median(y, na.rm=TRUE)
        if(!is.null(z) && any(isNA$x)) z[which(isNA$z)] <- stats::median(z, na.rm=TRUE)}
      if(debug) message(fxNa,"mNo1b quantile.block ")

      nonNA.idx <- if(is.null(z)) {
        which(!isNA$x & !isNA$y)
      } else {
        which(!isNA$x & !isNA$y & !isNA$z)
      }  ## too severe ?? (rather use all data : impute NAs (my median), qqN , reset positions to NA)
      if(debug) {message(fxNa,"bNo2a")}  
      if(length(nonNA.idx) < 2) {
        warning(fxNa,"Too few non-NA values !")
        return(list(x=x, y=y, z=z))
      } else {
        sortedMatr <- cbind(
          sort(x[nonNA.idx]),
          sort(y[nonNA.idx]),
          if(!is.null(z)) sort(z[nonNA.idx])
        )
        targetVal <- rowMeans(sortedMatr, na.rm=TRUE)
        if(debug) message(fxNa,"mNo2 quantile.block ")

        out <- list(
          x=targetVal[rank(x, ties.method="first")],
          y=targetVal[rank(y, ties.method="first")]
        )
        if(!is.null(z)) out$z <- targetVal[rank(z, ties.method="first")]
        if(debug) {message(fxNa,"mNo2b quantile.block "); mNo2b <- list(x=x,y=y,z=z,isNA=isNA,sortedMatr=sortedMatr,targetVal=targetVal,out=out)}

        ## Restore dimensions and NAs
        if(length(dim(x))==2) out$x <- matrix(out$x, nrow=x2$newDim[1], dimnames=dimnames(x2$isNA)) 
        if(length(dim(y))==2) out$y <- matrix(out$y, nrow=y2$newDim[1], dimnames=dimnames(y2$isNA))
        if(!is.null(z) && length(dim(z)==2)) {
          out$z <- matrix(out$z, nrow=z2$newDim[1], dimnames=dimnames(z2$isNA))
        }
        if(debug) { message(fxNa,"mNo3 quantile.block "); mNo3 <- list()}
        if(TRUE) {
          out$x[x2$isNA] <- NA
          out$y[y2$isNA] <- NA
          if(!is.null(z)) out$z[z2$isNA] <- NA
        }
        if(debug) message(fxNa,"mNo4 quantile.block ")
        if(isTRUE(useAllData)) {
          if(any(isNA$x)) out$x[which(isNA$x)] <- NA
          if(any(isNA$y)) out$x[which(isNA$y)] <- NA
          if(!is.na(z) && any(isNA$z)) out$z[which(isNA$z)] <- NA
        } 
      }
      out
    },

    ## normalize as "rescale" 
    "rescale"= list(
        #if(debug) message(fxNa,"mNo2 rescale ")
        #x=scaleDat(x, x2$isNA, x2$newDim, min=range, max=NULL, q=q),
        x=scaleXY(x, min=range, max=NULL, q=q),
        y=scaleXY(y, min=range, max=NULL, q=q),
        z=if(!is.null(z)) scaleXY(z, min=range, max=NULL, q=q)
    ) ,

    ## normalize as "median" 
    "median"= medNormXYZ(x, y, z),
    "logMedian"= medNormLogXYZ(x, y, z)

  )
  if(is.null(z)) out[1:2] else out
}
  
  
