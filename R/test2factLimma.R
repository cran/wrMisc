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
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return This function returns an object of class "MArrayLM" (from limma) containing/enriched by the testing results
#' @seealso \code{\link{makeMAList}}, single line testing \code{\link[limma]{lmFit}} and the \code{eBayes}-family of functions in package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}
#' @examples
#' set.seed(2014)
#' dat0 <- rnorm(30) + rep(c(10,15,19,20),c(9,8,7,6))
#' fa <- factor(rep(letters[1:4],c(9,8,7,6)))
#' dat2 <- data.frame(facA=rep(c("-","A","-","A"), c(9,8,7,6)),
#'   facB= rep(c("-","-","B","B"), c(9,8,7,6)), dat1=dat0, dat2=runif(30))
#' grpNa <- sub("-","",sub("\\.","", apply(dat2[,1:2], 1, paste, collapse="")))    
#' test2f <- test2factLimma(t(dat2[,3:4]), dat2$facA, dat2$facB, testS=FALSE)  
#' test2f 
#' # Now you can easily summarize results using topTable from limma
#' if(requireNamespace("limma", quietly=TRUE)) {
#'   library(limma)
#'   topTable(test2f, coef=1, n=5) 
#'   topTable(test2f, coef=2, n=5) } 
#' @export
test2factLimma <- function(datMatr, fac1, fac2, testSynerg=TRUE, testOrientation="=", addResults=c("lfdr","FDR","Mval","means"), addGenes=NULL, silent=FALSE, callFrom=NULL, debug=FALSE){
  fxNa <- .composeCallName(callFrom, newNa="test2factLimma")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  doTest <- TRUE
  if(!requireNamespace("limma", quietly=TRUE)) { doTest <- FALSE
    warning(fxNa,"You need to install package 'limma' first from Bioconductor")}
  if(doTest) {    
    msg1 <- " 'datMatr' should have the same number of cols as length of fac1 & fac2 !"
    if(ncol(datMatr) != length(fac1) | ncol(datMatr) != length(fac2)) stop(msg1)
    datDesign <- if(isTRUE(testSynerg)) try(stats::model.matrix(~ fac1 * fac2),silent=TRUE) else try(stats::model.matrix(~ fac1 + fac2), silent=TRUE)
    if(inherits(datDesign, "try-error")) { doTest <- FALSE; message(fxNa," Problem with model.matrix(), please check your factors !")
    } else if(debug) message(fxNa,"design matrix has ",nrow(datDesign)," rows and ",ncol(datDesign),"  cols")
  }
  if(doTest) {    
    datFit <- try(limma::lmFit(datMatr, design=datDesign), silent=TRUE)                ## Fitting linear models
    if(inherits(datFit, "try-error")) {warning(fxNa," PROBLEM with lmFit(); check if package 'limma' is correctly installed, it seems not to be working properly !?!")
      doTest <- FALSE
    } else if(debug) message(fxNa,"Sucessfully run lmFit()") }
    
  if(doTest) {    
    datFit <- limma::eBayes(datFit)                                  ## Adjusting using empirical Bayes
    altHyp <- "two.sided"                                             # default, change only if explicit sign recognized
    if(length(testOrientation) <1) testOrientation <- altHyp
    if(testOrientation %in% c("<","less","inf","lower")) altHyp <- "less"           
    if(testOrientation %in% c(">","greater","sup","upper")) altHyp <- "greater"
    chNAp <- colSums(!is.na(datFit$p.value))
    if(any(chNAp <1)) {message(fxNa," problem with redundant factors ?  Some cols of p.values are all NA !! (remove)")
      datFit$p.value <- datFit$p.value[,which(chNAp >0)]}  
    chFdr <- requireNamespace("fdrtool", quietly=TRUE)            # try(find.package("fdrtool"), silent=TRUE)
    if(length(addResults) >0) if("lfdr" %in% tolower(addResults) & !chFdr) {
      if(!silent) message(fxNa,"package 'fdrtool' not found ! Please install package fdrtool from CRAN for enabeling 'lfdr' estimations") 
      addResults <- addResults[which(!"lfdr" %in% tolower(addResults))] }    
    tx <- c("testing alternative hypothesis: true difference in means is "," than 0 (ie focus on "," results with A ",altHyp," than B)")
    if(debug) message(fxNa,"altHyp = ",altHyp)
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
        apply(out$p.value,2,stats::p.adjust,meth="BH")} else stats::p.adjust(out$p.value, meth="BH")
      if("lfdr" %in% tolower(addResults)) {
        chPa <- try(find.package("fdrtool"), silent=TRUE)
        if(inherits(chPa, "try-error")) message("package 'fdrtool' not found ! Please install first .. running so far without 'lfdr'") 
        addResults <- addResults[which(!tolower(addResults) %in% "lfdr")] }
      if("lfdr" %in% tolower(addResults)) {out$lfdr <- if(is.matrix(out$p.value)) {
        apply(out$p.value,2,pVal2lfdr)} else pVal2lfdr(out$p.value)
      }
      if(inherits(datFit$lfdr, "try-error")) {message(fxNa," PROBLEM with calulating lfdr ! ")
      } else if(debug) message(fxNa,"Sucessfully calculated lfdr ...")
      if("BY" %in% toupper(addResults)) {datFit$BY <- if(length(dim(out$p.value)) >1) {
        apply(datFit$p.value,2,stats::p.adjust,meth="BY")} else stats::p.adjust(out$p.value, meth="BY")
       }
      for(i in c("FDR","lfdr","BY")) {if(length(dim(out[[i]])) >1) rownames(out[[i]]) <- rownames(datMatr)}}
    out 
  } else if(debug) message(fxNa,"ATTENTION, no two-factorial was calculated, returning NULL") 
  }
       
