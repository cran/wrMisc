#' Confidence Interval To Given Alpha  
#'
#' This little function returns the confidence interval associated to a given significance level \code{alpha} under the hypothesis of the Normal distribution is valid.	
#' 
#' @param x (numeric) main input 
#' @param alpha (numeric) significance level, accepted type I error 
#' @param distrib (character) distribution, so far only \code{Normal} is implemented 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns the confidence interval to a given \code{alpha} under the hypothesis of the Normal distribution. 	
#' @seealso \code{\link[stats]{TDist}}; \code{\link[stats]{confint}}  
#' @examples
#' 
#' confInt(c(5,2:6))
#' 
#' @export
confInt <- function(x, alpha=0.05, distrib="Normal", silent=FALSE, callFrom=NULL) {            ## return Confidence Interval (CI) (range) based on sd ('x')
  fxNa <- .composeCallName(callFrom, newNa="confInt")
  if(!"normal" %in% tolower(distrib)) warning(fxNa,"So far only distrib='Normal' has been implemented, all data will be considered as originating from Normal distribution")
  tVal <- stats::qt((1 -alpha)/2 + 0.5, length(x) -1)    
  tVal*stats::sd(x, na.rm=TRUE) / sqrt(length(x))}
  
