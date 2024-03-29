#' 2-factorial Anova on single line of data
#'
#' This function runs 2-factorial Anova on a single line of data (using \code{\link[stats]{aov}} from package \code{stats}) 
#' using a model with two factors (without factor-interaction) and extracts the correpsonding p-value. 
#'
#' @param dat numeric vector
#' @param fac1 (character or factor) vector describing grouping elements of dat for first factor, must be of same langth as fac2
#' @param fac2 (character or factor) vector describing grouping elements of dat for second factor, must be of same langth as fac1
#' @param inclInteraction (logical) decide if factor-interactions (eg synergy) should be included to model
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced

#' @return This function returns the (uncorrected) p for factor 'Pr(>F)' (see \code{\link[stats]{aov}}) 
#' @seealso \code{\link[stats]{aov}}, \code{\link[stats]{anova}}; for repeated tests using the  package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} including \code{\link[limma]{lmFit}} and \code{eBayes} see \code{\link{test2factLimma}}
#' @examples
#' set.seed(2012); dat <- round(runif(8),1)
#' singleLineAnova(dat, gl(2,4),rep(1:2,4))
#' @export
singleLineAnova <- function(dat, fac1, fac2, inclInteraction=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="singleLineAnova")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(debug) message(fxNa,"sLA1")
  
  if(!identical(length(fac1),length(fac2))) stop("Arguments 'fac1' & 'fac2' should be of same length !")
  tmp <- data.frame(dat=as.numeric(dat), fac1=fac1, fac2=fac2)
  tmp.aov <- if(inclInteraction) stats::aov(dat ~ fac1 + fac2 + fac1:fac2, data=tmp) else {
    stats::aov(dat ~ fac1 + fac2, data=tmp)}
  tmp <- stats::anova(tmp.aov)[["Pr(>F)"]]
  tmp[-1*length(tmp)] }
    
