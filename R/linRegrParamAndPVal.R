#' Fit linear regression, return parameters and p-values
#'
#' This function fits a linear regression and returns the parameters, including p-values from Anova.
#' Here the vector 'y' (scalar response or dependent variable, ie the value that should get estimated) will be estimated according to 'dep' (explanatory or independent variable).
#' Alternatively, 'dep' may me a \code{matrix} where 1st column will be used as 'dep and the 2nd column as 'y'.
#' 
#' @param dep (numeric vector, matrix or data.frame) explanatory or dependent variable, if matrix or data.frame the 1st column will be used, if 'y'=\code{NULL} the 2nd column will be used as 'y'  
#' @param y (numeric vector) independent variable (the value that should get estimated based on 'dep')
#' @param asVect (logical) return numeric vector (Intercept, slope, p.intercept, p.slope) or matrix or results 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return numeric vector (Intercept, slope, p.intercept, p.slope), or if \code{asVect}==\code{TRUE} as matrix (p.values in 2nd column)
#' @seealso \code{\link[stats]{lm}}
#' @examples
#' linRegrParamAndPVal(c(5,5.1,8,8.2),gl(2,2))
#' @export
linRegrParamAndPVal <- function(dep, y=NULL, asVect=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## fit linear regression and return parameters, including p-values from Anova
  fxNa <- .composeCallName(callFrom, newNa="linRegrParamAndPVal")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(debug) message(fxNa,"lRP1")
  
  msg <- "'dep' and 'y' should be numeric vectors of equal length, or 'dep' may be matrix where the 1st column will be used as 'dep' and the secod as 'y'" 
  if(length(dim(dep)) ==2) {                         
    if(ncol(dep) >1 & length(y) <1) {dF <- as.data.frame(dep[,1:2]); colnames(dF) <- c("x","y")
    } else {
      if(length(y) == nrow(dep)) dF <- data.frame(x=dep[,1],y=y) else stop(msg)}
  } else {
    if(length(y) == length(dep)) dF <- data.frame(x=dep,y=y) else stop(msg)}
  ## main
  if(is.factor(dF[,2])) dF[,2] <- as.numeric(as.character(dF[,2])) 
  z <- stats::coef(summary(stats::lm(y~x,data=dF)))[,c("Estimate","Pr(>|t|)")]
  rownames(z)[2] <- "slope"
  if(asVect){ na <- dimnames(z)
    z <- as.numeric(z)
    names(z) <- c(na[[1]],paste("p",na[[1]],sep="."))}
  z }
    
