#' t.test on all individual values against all other values
#'
#' Run t.test on each indiv value of x against all its neighbours (=remaining values of same vector) in order to test if tis value is likely to belong to vector x. 
#' This represents a repeated leave-one-out testing. Mutiple choices for multiple testing correction are available.
#'
#' @param x matrix or data.frame
#' @param alph (numeric) threshold alpha (passed to \code{t.test})
#' @param alternative (character) will be passed to \code{t.test} as argument 'alternative', may be "two.sided",..
#' @param p.adj (character) multiple test correction : may be NULL (no correction), "BH","BY","holm","hochberg" or "bonferroni"  (but not 'fdr' since this may be confounded with local false discovery rate), see \code{\link[stats]{p.adjust}}
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a numeric vector with p-values or FDR (depending on argument \code{p.adj}) 
#' @seealso \code{\link[stats]{t.test}}, \code{\link[stats]{p.adjust}}
#' @examples
#' set.seed(2016); x1 <- rnorm(100)
#' allTests1 <- tTestAllVal(x1)
#' hist(allTests1,breaks="FD")
#' @export
tTestAllVal <- function(x, alph=0.05, alternative="two.sided", p.adj=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="tTestAllVal")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(debug) message(fxNa,"tTA1")

  x <- cbind(x=as.numeric(x), no=1:length(x))
  pVal <- sapply(1:length(x), function(z) stats::t.test(x[-z], x[z], conf.level=alph, alternative=alternative, var.equal=TRUE)$p.value )
  if(!is.null(p.adj)) if(any(p.adj %in% c("BH","BY","holm","hochberg","bonferroni"))) pVal <- stats::p.adjust(pVal,method=p.adj)
  pVal }
   
