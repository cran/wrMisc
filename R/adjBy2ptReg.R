#' Adjust Values By Two-Point Regression
#'
#' This function performs linear rescaling of numeric data based on specified limits (ie a specified window).
#' Values are adjusted to fit within a target range defined by `regrTo`.
#' 
#' 
#' @details
#' Since this function shares sigificant overlap with \code{\link{scaleXY}} (which has more advanced options) it is rather suggested to use _scaleXY()_ instead. 
#' The function _adjBy2ptReg()_ will may deprecated in the future.
#'
#' @param dat A numeric vector or matrix of values to adjust.
#' @param lims A list of two numeric vectors, each of length 1 or 2, representing the lower and upper limits for rescaling.
#'             Example: `lims = list(c(5, 9), c(60, 90))`.
#' @param regrTo A numeric vector of length 2 defining the target range for rescaling (default: `c(0.1, 0.9)`).
#' @param refLines Optional numeric or character vector specifying reference lines (rows) in `dat` to use for rescaling.
#'                 If `NULL`, all rows are used.
#' @param silent Logical. If `TRUE`, suppresses messages (default: `FALSE`).
#' @param debug Logical. If `TRUE`, prints debugging messages (default: `FALSE`).
#' @param callFrom Character string for tracking messages (default: `NULL`).
#'
#' @return A numeric vector or matrix with adjusted values.
#'
#' @examples
#' set.seed(2016); dat1 <- round(runif(50, 0, 100), 1)
#' adjBy2ptReg(dat1, lims = list(c(5, 9), c(60, 90)))
#' plot(dat1, adjBy2ptReg(dat1, lims=list(c(5,9), c(60,90))))
#' 
#' @export
adjBy2ptReg <- function(dat, lims, regrTo = c(0.1, 0.9), refLines = NULL, silent = FALSE, debug = FALSE, callFrom = NULL) {
  fxNa <- .composeCallName(callFrom, newNa = "adjBy2ptReg")
  ## Set silent and debug modes
  if (!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  ## Handle refLines
  if (length(refLines) > 0) {
    if (is.character(refLines) && !is.null(rownames(dat))) {
      refLines <- match(refLines, names(dat))
    } else {
      refLines <- as.integer(refLines)
      refLines <- refLines[which(refLines > 0 & refLines <= length(dat))]
    }
  }
  if(debug) {message(fxNa,"aB2r1")}

  ## Subset dat if refLines are provided
  dat1 <- if (length(refLines) > 0) dat[refLines] else dat

  ## Ensure dat1 has names
  if (is.null(names(dat1))) {
    names(dat1) <- seq_len(length(dat1))
  }

  ## Validate lims
  msg <- "'lims' should be a list giving limits (list(lo = c(min, max), hi = c(min, max)))"
  if (!is.list(lims)) lims <- as.list(lims)
  if (length(lims) != 2)  stop(fxNa, msg)
  if(debug) {message(fxNa,"aB2r2")}

  ## Precompute min and max for lims[[1]] and lims[[2]] to avoid redundant calculations
  lim1_min <- min(lims[[1]], na.rm = TRUE)
  lim1_max <- max(lims[[1]], na.rm = TRUE)
  lim2_min <- min(lims[[2]], na.rm = TRUE)
  lim2_max <- max(lims[[2]], na.rm = TRUE)
  if(debug) {message(fxNa,"aB2r3"); aB2r3 <- list(dat=dat,lims=lims,regrTo=regrTo,lim1_min=lim1_min,lim1_max=lim1_max )}

  # Determine indices for lo and hi
  lim2 <- if (all(sapply(lims, length) == 1)) {
    list(lo = which(dat1 == lims[[1]]), hi = which(dat1 == lims[[2]]))
  } else if (all(sapply(lims, length) == 2)) {
    list(
      lo = which(dat1 >= lim1_min & dat1 <= lim1_max),
      hi = which(dat1 >= lim2_min & dat1 <= lim2_max)
    )
  } else {
    stop(fxNa, "Cannot figure out 'lims'!\n", msg)
  }
  if(debug) {message(fxNa,"aB2r4")}

  ## Check if limits are too tight
  chLe <- sapply(lim2, length)
  if (any(chLe < 1) && !silent) {
    message(fxNa, "Limits seem too tight: lo ", chLe[1], " & hi ", chLe[2], " entries found!")
  }

  ## Compute mean values at lo and hi indices
  lim3 <- sapply(lim2, function(x) sum(dat1[x], na.rm = TRUE) / length(x))
  if(debug) {message(fxNa,"aB2r5"); aB2r5 <- list(dat=dat,lims=lims,regrTo=regrTo,lim1_min=lim1_min,lim1_max=lim1_max,lim2=lim2,lim3=lim3,chLe=chLe )}

  ## Compute normalization factor (slope and intercept)
  normFact <- if(length(lims) ==1) (regrTo[1]/lim3[1]) else (regrTo[2] -regrTo[1])/ (lim3[2] -lim3[1])       # slope
  normFact <- c(k=normFact, d= if(length(lims) ==1) 0 else regrTo[1] -normFact*lim3[1])  
  
  ## Apply normalization
  norD <- dat1 * normFact[1] + normFact[2]
  if(debug) {message(fxNa,"aB2r6")}

  ## Update dat with normalized values
  dat[which(is.finite(dat))] <- as.numeric(norD)

  return(dat)
}
  
  
#' Model linear regression and optional plot
#'
#' This function allows to model a linear regression and optionally to plot the results
#' 
#' @param dat (vector or matrix) main input
#' @param typeOfPlot (character)
#' @param toNinX (logical) 
#' @param plotData (logical) 
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return numeric vector with intercept and slope, optional plot
#' @seealso  \code{\link[base]{append}}; \code{\link{lrbind}}
#' @examples
#' .datSlope(c(3:6))
#' @export
.datSlope <- function(dat, typeOfPlot="sort", toNinX=FALSE, plotData=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa=".datSlope")
  if(length(typeOfPlot) >1) {
    if(!silent) message(fxNa,"Invalid entry for typeOfPlot ",paste(typeOfPlot, collapse=", ")," (use 1st)")
    typeOfPlot <- typeOfPlot[1] }
  dat <- as.numeric(naOmit(sort(dat)))
  y <- if(tolower(typeOfPlot) %in% "cumsum") cumsum(sort(dat)) else sort(dat)
  y <- data.frame(out=y, n=1:length(dat))
  out <- if(toNinX) stats::lm(n ~ out, data=y)$coefficients else stats::lm(out ~ n, data=y)$coefficients
  if(plotData) { if(toNinX) {
      chF <- try(graphics::plot(y, type="s", ylab=if("cumsum" %in% tolower(typeOfPlot)) "cumsum" else "sorted"), silent=TRUE)
      if(!inherits(chF, "try-error")) { for(i in 2:ncol(out)) graphics::points(y); graphics::abline(out, lty=2, col=2)
      } else {
        if(!silent) message(fxNa,"UNABLE to plot data !")}
    } else {
      chF <- try(graphics::plot(1:nrow(y), y[,1], type="s", ylab=if("cumsum" %in% tolower(typeOfPlot)) "cumsum" else "sorted"), silent=TRUE)
      if(!inherits(chF, "try-error")) {graphics::points(1:nrow(y), y[,1]); graphics::abline(out, lty=2, col=2)} else { 
        if(!silent) message(fxNa,"UNABLE to plot data !")}
    } 
  }
  names(out) <- c("intercept","slope")
  out }
 
  
#' Check regression arguments
#'
#' This function allows to check arguments for linear regression. Used as argument checking for \code{regrBy1or2point} and \code{regrMultBy1or2point}
#' 
#' @param inData (numeric vector) main input
#' @param refList (list)
#' @param regreTo (numeric vector) 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return list
#' @seealso  \code{\link[base]{append}}; \code{\link{lrbind}}
#' @examples
#' .datSlope(c(3:6))
#' @export
.checkRegrArguments <- function(inData, refList, regreTo, callFrom=NULL){
  ## argument checking for regrBy1or2point() & regrMultBy1or2point()
  ## 'inData' should be
  ## returns corrected/adjusted 'refList' and 'regreTo' (as list)
  fxNa <- .composeCallName(callFrom, newNa=".checkRegrArguments")
  if(!is.list(refList)) refList <- list(refList)
  if(is.list(inData) && !is.data.frame(inData)) message(fxNa,"Potential problem : not expecting list (length=",length(inData),") at place of 'inDat' !!")
  if(min(sapply(refList, length)) <2) {
    warning(fxNa,sum(sapply(refList, length) <2)," entries of 'refLst' contain less than 2 array-positions (well-names)")
    refList <- refList[which(sapply(refList, length) >1)] }
  datIsMa <- !is.null(dim(inData)) 
  checkRefFields <- unlist(refList) %in% (if(datIsMa) rownames(inData) else names(inData))
  if(sum(checkRefFields) <1) {
    message(fxNa," 'refList'  ",paste(unlist(refList),collapse=" "))
    message("Head(names(inDat) ",paste(if(datIsMa) utils::head(rownames(inData)) else utils::head(names(inData)),collapse=" ")," ...")
    stop(" none of the fields given in 'refList' appear in names of 'inDat' !") }
  if(sum(checkRefFields) < sum(sapply(refList, length))) message(fxNa,"Some of the fields given in 'refLst' do not appear in names of 'inDat' !")
  if(length(refList) > length(regreTo)) message(fxNa,"Need as many values 'regrTo' as types (of samples) given in 'refLst' !")
  list(refLst=refList, regrTo=regreTo) }
       
