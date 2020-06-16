#' 2-factorial limma-style t-test
#'
#' The aim of this function is to provide convenient acces to two-factorial (linear) testing withing the framework of \code{\link{makeMAList}} including the emprical Bayes shrinkage. 
#' The input data 'datMatr' which should already be organized as limma-type MAList, eg using using \code{\link{makeMAList}}. 
#' Note: This function uses the Bioconductor package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}.
#'
#' @param datMatr matrix or data.frame with lines as indenpendent series of measures (eg different genes)
#' @param fac1 (character or factor) vector describing grouping elements of each line of 'datMatr' for first factor, must be of same langth as fac2
#' @param fac2 (character or factor) vector describing grouping elements of each line of 'datMatr' for second factor, must be of same langth as fac1
#' @param testSynerg (logical) decide if factor-interactions (eg synergy) should be included to model
#' @param testOrientation (character) default (or any non-recignized input) '=', otherwise either '>','gerater','sup','upper' or '<','inf','lower'
#' @param addResults (character) vector defining which types of information should be included to output, may be 'lfdr','FDR' (for BY correction), 'Mval' (M values), 'means' (matrix with mean values for each group of replicates)
#' @param addGenes (matrix or data.frame) additional information to add to output
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return object of class "MArrayLM" (from limma)
#' @seealso \code{\link{makeMAList}}, single line testing \code{\link[limma]{lmFit}} and the \code{eBayes}-family of functions in package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}
#' @examples
#' ## example for testing change of ratio for 4 sets (AA-DD) of pairs of data  
#' set.seed(2017); t8 <- matrix(round(rnorm(160,10,0.4),2),ncol=8,
#'   dimnames=list(letters[1:20],c("AA1","BB1","CC1","DD1","AA2","BB2","CC2","DD2")))
#' t8[3:6,1:2] <- t8[3:6,1:2]+3   # augment lines 3:6 (c-f) for AA1&BB1
#' t8[5:8,5:6] <- t8[5:8,5:6]+3   # augment lines 5:8 (e-h) for AA2&BB2 (c,d,g,h should be found)
#' ## via MAobj
#' maOb8 <- makeMAList(t8,MAf=gl(2,4,labels=c("R","G")))
#' fit8b <- test2factLimma(maOb8,c(1,1,1,1),c(0,0,1,1),testS=FALSE)  # same result as below (fit8e)
#' limma::topTable(fit8b,coef=1,n=5)                      # effect for c,d,g&h
#' ## explicit (long) way via limma:
#' fit8 <- limma::lmFit(maOb8, design= model.matrix(~ 0+factor(c(1,1,2,2))))
#' fit8e <- limma::eBayes(fit8)
#' limma::topTable(fit8e,coef=1,n=5)                      # effect for c,d,g&h
#' @export
test2factLimma <- function(datMatr,fac1,fac2,testSynerg=TRUE,testOrientation="=",addResults=c("lfdr","FDR","Mval","means"),addGenes=NULL,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="test2factLimma")
  chPa <- try(find.package("limma"),silent=TRUE)
  if("try-error" %in% class(chPa)) stop("package 'limma' not found ! Please install first") 
  msg1 <- " 'datMatr' should have the same number of cols as length of fac1 & fac2 !"
  if(ncol(datMatr) != length(fac1) | ncol(datMatr) != length(fac2)) stop(msg1)
  datDesign <- if(testSynerg) stats::model.matrix(~ fac1 * fac2) else stats::model.matrix(~ fac1 + fac2)
  datFit <- try(limma::lmFit(datMatr, design=datDesign))                ## Fitting linear models
  if("try-error" %in% class(datFit)) warning(fxNa," check if package 'limma' is correctly installed, it seems not be working properly !?!")  
  datFit <- limma::eBayes(datFit)                                  ## Adjusting using empirical Bayes
  altHyp <- "two.sided"                                             # default, change only if explicit sign recognized
  if(length(testOrientation) <1) testOrientation <- altHyp
  if(testOrientation %in% c("<","less","inf","lower")) altHyp <- "less"           
  if(testOrientation %in% c(">","greater","sup","upper")) altHyp <- "greater"
  chNAp <- colSums(!is.na(datFit$p.value))
  if(any(chNAp <1)) {message(fxNa," problem with redundant factors ?  Some cols of p.values are all NA !! (remove)")
    datFit$p.value <- datFit$p.value[,which(chNAp >0)]}  
  chFdr <- try(find.package("fdrtool"), silent=TRUE)
  if(length(addResults) >0) if("lfdr" %in% tolower(addResults) & "try-error" %in% class(chFdr)) {
    if(!silent) message(fxNa,"package 'fdrtool' not found ! Please install package fdrtool from CRAN for enabeling 'lfdr' estimations") 
    addResults <- addResults[which(!"lfdr" %in% tolower(addResults))] }    
  tx <- c("testing alternative hypothesis: true difference in means is "," than 0 (ie focus on "," results with A ",altHyp," than B)")
  if(identical(altHyp,"greater")){
    ch <- datFit$means[,1] > datFit$means[,2]
    if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
    if(any(ch)) datFit$p.value[which(ch),] <- datFit$p.value[which(ch),]/2
    if(any(!ch)) datFit$p.value[which(!ch),] <- 1- datFit$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
  }
  if(identical(altHyp,"less")){
    ch <- datFit$means[,2] > datFit$means[,1]
    if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
    if(any(ch)) datFit$p.value[which(ch),] <- datFit$p.value[which(ch),]/2
    if(any(!ch)) datFit$p.value[which(!ch),] <- 1- datFit$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
  }
  if(length(addResults) <1) out <- datFit$p.value[,2] else { out <- datFit
     cat(" addResults",addResults,"\n")
    ## further inspect & correct values of 'addResults' ?
    if("Mval" %in% addResults) out$Mval <- (out$means[,1] - out$means[,2]) 
    if("FDR" %in% toupper(addResults)) out$FDR <- if(length(dim(out$p.value)) >1) {
      apply(out$p.value,2,stats::p.adjust,meth="BH")} else stats::p.adjust(out$p.value,meth="BH")
    if("lfdr" %in% tolower(addResults)) {
      chPa <- try(find.package("fdrtool"),silent=TRUE)
      if("try-error" %in% class(chPa)) message("package 'fdrtool' not found ! Please install first .. running so far without 'lfdr'") 
      addResults <- addResults[which(!tolower(addResults) %in% "lfdr")] }
    if("lfdr" %in% tolower(addResults)) {out$lfdr <- if(is.matrix(out$p.value)) {
      apply(out$p.value,2,pVal2lfdr)} else pVal2lfdr(out$p.value)
     }
    if("try-error" %in% class(datFit$lfdr)) {message(fxNa," PROBLEM with calulating lfdr ! ")}
    if("BY" %in% toupper(addResults)) {datFit$BY <- if(length(dim(out$p.value)) >1) {
      apply(datFit$p.value,2,stats::p.adjust,meth="BY")} else stats::p.adjust(out$p.value,meth="BY")
     }
    for(i in c("FDR","lfdr","BY")) {if(length(dim(out[[i]])) >1) rownames(out[[i]]) <- rownames(datMatr)}}
  out }
     
