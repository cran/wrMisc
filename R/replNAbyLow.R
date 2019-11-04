#' Replace NAs by low values
#'
#' With several screening techniques used in hight-throughput biology values at/below detection limit are returned as \code{NA}.
#' However, the resultant \code{NA}-values may be difficult to analyse properly, simply ignoring \code{NA}-values mat not be a good choice.
#' When (technical) replicate measurements are available, one can look for cases where one gave an \code{NA} while the other did not 
#' with the aim of investigating such 'NA-neighbours'.  
#' \code{replNAbyLow} locates and replaces \code{NA} values by (random) values from same line & same group 'grp'. 
#' The origin of NAs should be predominantly absence of measure (quantitation) due to signal below limit of detection
#' and not saturation at upper detection limit or other technical problems.  
#' Note, this approach may be not optimal if the number of NA-neighbours is very low.
#' Replacamet is done -depending on agrument 'unif'- by Gaussian random model based on neighbour values (within same group),
#' using their means and sd, or a uniform random model (min and max of neighbour values) .
#' Then numeric matrix (same dim as 'x') with \code{NA} replaced is returned.
#'
#' @param x (numeric matrix or data.frame) main input
#' @param grp (factor) to organize replicate columns of (x)
#' @param quant (numeric) quantile form 'neighbour' values to use as upper limit for random values
#' @param signific number of signif digits for random values
#' @param unif (logical) toggle between uniform and Gaussian random values
#' @param absOnly (logical) if TRUE, make negative NA-replacment values positive as absolute values
#' @param seed (integer) for use with set.seed for reproducible output
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return numeric matrix (same dim as 'x') with \code{NA} replaced
#' @seealso \code{\link{naOmit}}, \code{\link[stats]{na.fail}}
#' @examples
#' dat <- matrix(round(rnorm(30),2),ncol=6); grD <- gl(2,3)
#' dat[sort(sample(1:30,9,repl=FALSE))] <- NA
#' dat; replNAbyLow(dat,gr=grD)
#' @export
replNAbyLow <- function(x,grp,quant=0.8,signific=3,unif=TRUE,absOnly=FALSE,seed=NULL,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="replNAbyLow")
  if(length(dim(x))<2) stop("expecting matrix or data.frameas 'x'")
  if(is.data.frame(x)) x <- as.matrix(x)
  if(length(grp) != ncol(x)) stop(" 'grp' does NOT match number of columns in 'x'")
  nNA <- sum(is.na(x))
  if(nNA >0){
    lowVa <- unlist(apply(x,1,.extrNAneighb,grp))           # all low values with NA in same grp
    if(length(lowVa) <1) {
      lowVa <- sort(as.numeric(x))[1:round(sqrt(length(x)))]
      if(!silent) message(fxNa,"no values within lines of NA groups, use all observations")}
    lowVa <- lowVa[which(lowVa < stats::quantile(lowVa,quant))]
    nEstim <- ceiling(nNA*1.2)+5
    if(!silent) message(fxNa,"random model as ",if(unif) c("uniform ",signif(min(lowVa),signific)," to ",
      signif(max(lowVa),signific)) else c("Gaussian mean=",signif(mean(lowVa),signific),sd=signif(stats::sd(lowVa),signific)))
    if(!is.null(seed)) {if(is.numeric(seed)) set.seed(round(seed[1])) else message(fxNa,"ignoring invalid 'seed'")}
    raVa <- if(unif) signif(stats::rnorm(nEstim,mean(lowVa),stats::sd(lowVa)),digits=signific) else {
      signif(stats::runif(nEstim,min(lowVa),max(lowVa)),digits=signific)}
    if(absOnly & any(raVa <0)) {message(fxNa," force ",sum(raVa <0)," to positive"); raVa <- abs(raVa)}
    tmp <- unique(raVa)
    raVa <- if(length(tmp) > nNA) unique(raVa)[1:nNA] else raVa[1:nNA]                              # avoid identical values
    x[which(is.na(x))] <- raVa }
  x }
    
