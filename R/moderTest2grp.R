#' Moderated pair-wise t-test from limma
#'
#' Runs moderated t-test from package 'limma' on each line of data.
#' Note: This function requires the package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} from bioconductor. 
#' The limma contrast-matrix has to be read by column, the lines in the contrast-matrix containing '+1' will be compared to the '-1' lines, eg grpA-grpB .
#' Local false discovery rates (lfdr) estimations will be made using the CRAN-package \href{https://CRAN.R-project.org/package=fdrtool}{fdrtool} (if available).
#'
#' @param dat matrix or data.frame with rows for multiple (independent) tests, use ONLY with 2 groups; assumed as log2-data
#' @param grp (factor) describes column-relationship of 'dat'   (1st factor is considered as reference -> orientation of M-values !!)
#' @param limmaOutput (logical) return full (or extended) MArrayLM-object from limma or 'FALSE' for only the (uncorrected) p.values
#' @param addResults (character) types of results to add besides basic limma-output, data are assumed to be log2 ! (eg "lfdr" using fdrtool-package, "FDR" or "BH" for BH-FDR, "BY" for BY-FDR, 
#'   "bonferroni" for Bonferroni-correction, "qValue" for lfdr by qvalue, "Mval", "means" or "nonMod" for non-moderated test and he equivaent all (other) multiple testing corrections chosen here)
#' @param testOrientation (character) for one-sided test (">","greater" or "<","less"), NOTE : 2nd grp is considered control/reference, '<' will identify grp1 < grp2
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns a limma-type object of class \code{MArrayLM}
#' @seealso \code{\link[limma]{lmFit}} and the \code{eBayes}-family of functions in package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}, \code{\link[stats]{p.adjust}}
#' @examples
#' set.seed(2017); t8 <- matrix(round(rnorm(1600,10,0.4),2), ncol=8,
#'   dimnames=list(paste("l",1:200),c("AA1","BB1","CC1","DD1","AA2","BB2","CC2","DD2")))
#' t8[3:6,1:2] <- t8[3:6,1:2]+3     # augment lines 3:6 for AA1&BB1
#' t8[5:8,5:6] <- t8[5:8,5:6]+3     # augment lines 5:8 for AA2&BB2 (c,d,g,h should be found)
#' t4 <- log2(t8[,1:4]/t8[,5:8])
#' ## Two-sided testing 
#' fit4 <- moderTest2grp(t4,gl(2,2))
#' # If you have limma installed we can now see further
#' if("list" %in% mode(fit4)) limma::topTable(fit4, coef=1, n=5)    # effect for 3,4,7,8
#' 
#' ## One-sided testing
#' fit4in <- moderTest2grp(t4,gl(2,2),testO="<")
#' # If you have limma installed we can now see further
#' if("list" %in% mode(fit4)) limma::topTable(fit4in, coef=1, n=5)
#' @export
moderTest2grp <- function(dat, grp, limmaOutput=TRUE, addResults=c("lfdr","FDR","Mval","means"), testOrientation="=", silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="moderTest2grp")
  runTest <- TRUE
  if(!isTRUE(silent)) silent <- FALSE
  if(!isFALSE(limmaOutput)) limmaOutput <- TRUE
  if(any(length(dat) <1, length(dim(dat)) !=2, dim(dat) < c(1,3))) stop("Invalid argument 'dat'; must be matrix or data.frame with min 1 lines and 3 columns")
  if(length(grp) != ncol(dat)) stop("Check parameters: Number of columns of 'dat' doesn't match to length of 'grp'")  
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(length(levels(grp)) <2) stop(" need at least 2 groups in argument 'grp'")
  if(length(levels(grp)) >2) {message(fxNa," capable only of treating 2 groups in argument 'grp' (ignore rest)")
    grp[which(grp %in% levels(grp)[-(1:2)])] <- NA }
    
  packages <- c("limma", "fdrtool", "qvalue")
  checkPkg <- function(pkg) requireNamespace(pkg, quietly=TRUE)
  checkPkgs <- sapply(packages, checkPkg)
  if(!checkPkgs[1]) {warning(fxNa,"Package 'limma' not found ! Please install first from Bioconductor"); runTest <- FALSE}
  if(runTest) {
    
    if(limmaOutput & length(addResults) >0) if("all" %in% addResults) addResults <- c("BH", "BY","lfdr","qValue","bonferroni","Mval","means","nonMod")
    if(!checkPkgs[2] & any("lfdr" %in% tolower(addResults))) {
      if(!silent) message(fxNa," package 'fdrtool' not found, omitting .. Please install from CRAN for enabeling 'lfdr'")
      addResults <- addResults[which(!tolower(addResults) %in% "lfdr")] }
    if(!checkPkgs[3] & any(c("qvalue","qval") %in% tolower(addResults))) {
      if(!silent) message(fxNa," package 'qvalue' not found, omitting .. Please install from Bioconductor for enabeling 'qValue'")
      addResults <- addResults[which(!tolower(addResults) %in% c("qvalue","qval"))] }
    ## treat non-unique row-names ?
    if(length(rownames(dat) >0)) if(length(unique(rownames(dat))) < nrow(dat)) {
      if(!silent) message(fxNa," detected ",nrow(dat) -length(unique(rownames(dat)))," non-unique rownames of 'dat' !")}
    altHyp <- "two.sided"                                             # default, change only if explicit sign recognized
    if(length(testOrientation) <1) testOrientation <- altHyp
    if(testOrientation %in% c("<","less","inf")) altHyp <- "less" 
    if(testOrientation %in% c(">","greater","sup")) altHyp <- "greater" 
    datDesign <- stats::model.matrix(~ grp)
    if(nrow(datDesign) < ncol(dat)) datDesign <- rbind(datDesign, matrix(rep(0,length(levels(grp))*(ncol(dat)-nrow(datDesign))), ncol=ncol(datDesign)))
    fit1 <- try(limma::eBayes(limma::lmFit(dat, design=datDesign)), silent=TRUE)                ## Fitting linear model
    if(inherits(fit1, "try-error")) { runTest <- FALSE
      warning(fxNa," UNABLE to run limma::lmFit() and/or limma::eBayes(); can't run this function !")}
  }
  if(runTest) {  
    fit1$means <- rowGrpMeans(dat, grp) 
    chNA <- colSums(is.na(fit1$p.value))
    if(any(chNA==nrow(dat))) fit1$p.value <- fit1$p.value[,-1*which(chNA==nrow(dat))]
    ## modif to adjust for different H0  (29mar18)
    tx <- c("testing alternative hypothesis: true difference in means is "," than 0 (ie focus on "," results with A ",altHyp," than B)")
    if(identical(altHyp,"greater")) {
      ch <- fit1$means[,1] > fit1$means[,2]
      if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
      if(any(ch)) fit1$p.value[which(ch),] <- fit1$p.value[which(ch),]/2
      if(any(!ch)) fit1$p.value[which(!ch),] <- 1- fit1$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
    }
    if(identical(altHyp,"less")){
      ## testing for Ho A > B
      ch <- fit1$means[,2] > fit1$means[,1]
      if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
      if(any(ch)) fit1$p.value[which(ch),] <- fit1$p.value[which(ch),]/2
      if(any(!ch)) fit1$p.value[which(!ch),] <- 1- fit1$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
    }
    if(!limmaOutput) out <- fit1$p.value[,2] else { out <- fit1
      ## further inspect & correct values of 'addResults' ?
      if("Mval" %in% addResults) out$Mval <- (out$means[,1] - out$means[,2]) 
      if(any(c("FDR","BH") %in% toupper(addResults))) out$FDR <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="BH")} else stats::p.adjust(out$p.value, meth="BH")
      if("BY" %in% toupper(addResults)) out$BY <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="BY")} else stats::p.adjust(out$p.value, meth="BY")
      if("lfdr" %in% tolower(addResults)) {out$lfdr <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, pVal2lfdr)} else pVal2lfdr(out$p.value) }    
      if(any(c("qval","qvalue") %in% tolower(addResults))) { out$qVal <- if(is.matrix(out$p.value)) {
        try(apply(out$p.value, 2, function(x) qvalue::qvalue(x,lfdr.out=TRUE)$lfdr), silent=TRUE)
          } else try(qvalue::qvalue(out$p.value,lfdr.out=TRUE)$lfdr, silent=TRUE) 
        if(inherits(out$qVal, "try-error")) {  
          if(!silent) message(fxNa," Problem with pi0 estimation, setting pi0=1 like BH-FDR")
          out$qVal <- if(is.matrix(out$p.value)) { apply(out$p.value, 2, function(x) qvalue::qvalue(x, pi0=1, lfdr.out=TRUE)$lfdr)
            } else qvalue::qvalue(out$p.value, pi0=1, lfdr.out=TRUE)$lfdr }
      }    
      if(any(c("bonferroni","bonf") %in% tolower(addResults))) out$bonf <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="bonferroni")} else stats::p.adjust(out$p.value, meth="bonferroni")
    }
    if("nonMod" %in% addResults) { gr1 <- which(grp==levels(grp)[1])
      gr2 <- which(grp==levels(grp)[2])
      altHyp <- testOrientation
      if(altHyp=="=") altHyp <- "two.sided" 
      out$nonMod.p <- apply(dat, 1, function(x) stats::t.test(x[gr1], x[gr2], alternative=altHyp)$p.value)
      if(any(c("FDR","BH") %in% toupper(addResults))) out$nonMod.FDR <- stats::p.adjust(out$nonMod.p, method="BH") 
      if("BY" %in% toupper(addResults)) out$nonMod.BY <- stats::p.adjust(out$nonMod.p, method="BY")
      if(any(c("lfdr") %in% tolower(addResults))) out$nonMod.lfdr <- try(pVal2lfdr(out$nonMod.p), silent=TRUE) 
      if(any(c("qval","qvalue") %in% tolower(addResults))) { 
        out$nonMod.qVal <- try(qvalue::qvalue(out$nonMod.p, lfdr.out=TRUE)$lfdr, silent=TRUE)
        if(inherits(out$nonMod.qVal, "try-error")) {
          if(!silent) message(fxNa," Problem with pi0 estimation (non-shrinked p-values) fro qValue, setting pi0=1 like BH-FDR")
          out$qVal <- if(is.matrix(out$p.value)) qvalue::qvalue(out$nonMod.p, pi0=1, lfdr.out=TRUE)$lfdr }}
      if(any(c("bonferroni","bonf") %in% tolower(addResults))) out$nonMod.bonf <- stats::p.adjust(out$nonMod.p, method="bonferroni")     
    }
    out }}
    
