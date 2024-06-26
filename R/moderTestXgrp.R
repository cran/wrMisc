#' Multiple moderated pair-wise t-tests from limma
#'
#' Runs all pair-wise combinations of moderated t-tests from package 'limma' on each line of data against 1st group from 'grp'.
#' Note: This function requires the package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} from bioconductor. 
#' The limma contrast-matrix has to be read by column, the lines in the contrast-matrix containing '+1' will be compared to the '-1' lines, eg grpA-grpB .
#'
#' @param dat matrix or data.frame with rows for multiple (independent) tests, use ONLY with 2 groups; assumed as log2-data !!!
#' @param grp (factor) describes column-relationship of 'dat'   (1st factor is considered as reference -> orientation of M-values !!)
#' @param limmaOutput (logical) return full (or extended) MArrayLM-object from limma or 'FAlSE' for only the (uncorrected) p.values
#' @param addResults (character) types of results to add besides basic limma-output, data are assumed to be log2 ! (eg "lfdr" using fdrtool-package, "FDR" or "BH" for BH-FDR, "BY" for BY-FDR, 
#'   "bonferroni" for Bonferroni-correction, "qValue" for lfdr by qvalue, "Mval", "means" or "nonMod" for non-moderated test and he equivaent all (other) multiple testing corrections chosen here)
#' @param testOrientation (character) for one-sided test (">","greater" or "<","less"), NOTE : 2nd grp is considered control/reference, '<' will identify grp1 < grp2
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns a limma-type MA-object (list)
#' @seealso \code{\link{moderTest2grp}} for single comparisons, \code{\link[limma]{lmFit}} and the \code{eBayes}-family of functions in package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}
#' @examples
#' grp <- factor(rep(LETTERS[c(3,1,4)],c(2,3,3)))
#' set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4),2), ncol=8,
#'   dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste(grp,c(1:2,1:3,1:3),sep="")))
#' t8[3:6,1:2] <- t8[3:6,1:2] +3                    # augment lines 3:6 (c-f) 
#' t8[5:8,c(1:2,6:8)] <- t8[5:8,c(1:2,6:8)] -1.5    # lower lines 
#' t8[6:7,3:5] <- t8[6:7,3:5] +2.2                  # augment lines 
#' ## expect to find C/A in c,d,g, (h)
#' ## expect to find C/D in c,d,e,f
#' ## expect to find A/D in f,g,(h) 
#' test8 <- moderTestXgrp(t8, grp) 
#' # If you have limma installed we can now see further
#' if("list" %in% mode(test8)) head(test8$p.value, n=8)
#' @export
moderTestXgrp <- function(dat, grp, limmaOutput=TRUE, addResults=c("lfdr","FDR","Mval","means"), testOrientation="=", silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="moderTestXgrp")
  runTest <- TRUE
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(!isFALSE(limmaOutput)) limmaOutput <- TRUE
  if(any(length(dat) <1, length(dim(dat)) !=2, dim(dat) < c(1,3))) { runTest <- FALSE
    warning(fxNa,"Invalid argument 'dat'; must be matrix or data.frame with min 1 lines and 3 columns") }
  if(runTest) {  
    if(length(grp) != ncol(dat)) { runTest <- FALSE
      warning(fxNa,"Check parameters: Number of columns of 'dat' doesn't match to length of 'grp'") }}  
  if(runTest) {  
    if(!is.factor(grp)) grp <- as.factor(grp)
    if(length(levels(grp)) <2) stop("Need at least 2 groups in argument 'grp'")
    ## check packae dependencies 
    packages <- c("limma", "fdrtool", "qvalue")
    checkPkg <- function(pkg) requireNamespace(pkg, quietly=TRUE)
    checkPkgs <- sapply(packages, checkPkg)
    if(!checkPkgs[1]) { runTest <- FALSE; message(fxNa,"NOTE: Package 'limma' not found ! Unable to run tests. Please install first from Bioconductor")} } 
  if(debug) {message(fxNa,"mTX1"); mTX1 <- list(dat=dat,grp=grp,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest)}
    
  if(runTest && requireNamespace("limma")) {
    if(limmaOutput && length(addResults) >0) if("all" %in% addResults) addResults <- c("BH", "BY","lfdr","qValue","bonferroni","Mval","means","nonMod")
    if(!checkPkgs[2] && any("lfdr" %in% tolower(addResults))) {
      if(!silent) message(fxNa,"Package 'fdrtool' not found, omitting .. Please install from CRAN for enabeling 'lfdr'")
      addResults <- addResults[-which(tolower(addResults) %in% "lfdr")] }
    if(!checkPkgs[3] && any(c("qvalue","qval") %in% tolower(addResults))) {
      if(!silent) message(fxNa,"Package 'qvalue' not found, omitting .. Please install from Bioconductor for enabeling 'qValue'")
      addResults <- addResults[-which(tolower(addResults) %in% c("qvalue","qval"))] }
    if(debug) {message(fxNa,"mTX2")}
    ## treat non-unique row-names ?

    if(length(rownames(dat) >0)) if(length(unique(rownames(dat))) < nrow(dat)) {
      if(!silent) message(fxNa,"Detected ",nrow(dat) -length(unique(rownames(dat)))," non-unique rownames of 'dat' !")}
    altHyp <- "two.sided"                                             # default, change only if explicit sign recognized
    if(length(testOrientation) <1) testOrientation <- altHyp
    if(testOrientation %in% c("<","less","inf")) altHyp <- "less"           
    if(testOrientation %in% c(">","greater","sup")) altHyp <- "greater"
    ## prepare modeling 
    datDesign <- stats::model.matrix(~ -1 + grp)                  # can't use directly, need contrasts !!
    colnames(datDesign) <- sub("^grp","", colnames(datDesign))
    comp <- triCoord(length(levels(grp)))
    rownames(comp) <- paste(levels(grp)[comp[,1]], levels(grp)[comp[,2]],sep="-")
    contr.matr <- matrix(0, nrow=length(levels(grp)), ncol=nrow(comp), dimnames=list(levels(grp), rownames(comp)))
    for(j in 1:nrow(comp)) contr.matr[comp[j,],j] <- c(1,-1)
    if(debug) {message(fxNa,"mTX3"); mTX3 <- list(dat=dat,grp=grp,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest,datDesign=datDesign,comp=comp,contr.matr=contr.matr )}
    ## see eg   https://support.bioconductor.org/p/57268/; https://www.biostars.org/p/157068/
    globFilt <- 1:nrow(dat)                                       # so far apply testing to all lines
    
    ## main
    fit0 <- try(limma::lmFit(dat[globFilt,], datDesign), silent=TRUE)          # testing part 1
    if(inherits(fit0, "try-error")) { runTest <- FALSE
       warning(fxNa," Problem running lmFit(), unable to run tests; check if package 'limma' is properly installed !")}
    if(debug) {message(fxNa,"mTX4")}       
  } else runTest <- FALSE

  if(runTest && requireNamespace("limma")) {
    fit1 <- limma::eBayes(limma::contrasts.fit(fit0, contrasts=contr.matr))  # variant to run all contrasts at same time
    compNa <- colnames(fit1$contrasts)
    if(is.null(compNa) && !silent) message(fxNa," Note: Could not find names of (multiple) comparisons !")
    fit1$means <- rowGrpMeans(dat, grp) 
    ## separate running of contrasts, like gxTools, need then to extract & combine all pValues
    chNA <- colSums(is.na(fit1$p.value))
    if(any(chNA==nrow(dat))) fit1$p.value <- fit1$p.value[,-1*which(chNA==nrow(dat))]
    ## modif to adjust for different H0  (29mar18)
    tx <- c("testing alternative hypothesis: true difference in means is "," than 0 (ie focus on "," results with A ",altHyp," than B)")
    if(identical(altHyp,"greater")){
      ch <- fit1$means[,1] > fit1$means[,2]
      if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
      if(any(ch)) fit1$p.value[which(ch),] <- fit1$p.value[which(ch),]/2
      if(any(!ch)) fit1$p.value[which(!ch),] <- 1- fit1$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
    }
    if(identical(altHyp,"less")){
      ch <- fit1$means[,2] > fit1$means[,1]
      if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
      if(any(ch)) fit1$p.value[which(ch),] <- fit1$p.value[which(ch),]/2
      if(any(!ch)) fit1$p.value[which(!ch),] <- 1- fit1$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
    }
    if(is.null(colnames(fit1$t))) colnames(fit1$t) <- compNa
    if(is.null(colnames(fit1$p.value))) colnames(fit1$p.value) <- compNa
    if(any(c("qval","qvalue") %in% tolower(addResults)) && !requireNamespace("qvalue") && !silent) message(fxNa,"NOTE: package 'qvalue' not installed from CRAN, can't calulate ...")

    ## add various multiple testing corrections
    if(!limmaOutput) out <- fit1$p.value[,2] else { out <- fit1
      ## further inspect & correct values of 'addResults' ?
      if("Mval" %in% addResults) out$Mval <- (out$means[,1] - out$means[,2]) 
      if(any(c("FDR","BH") %in% toupper(addResults))) out$FDR <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="BH")} else stats::p.adjust(out$p.value, meth="BH")
      if("BY" %in% toupper(addResults)) out$BY <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="BY")} else stats::p.adjust(out$p.value, meth="BY")
      if("lfdr" %in% tolower(addResults) && requireNamespace("fdrtool")) {out$lfdr <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, pVal2lfdr)} else pVal2lfdr(out$p.value) }    
      if(any(c("qval","qvalue") %in% tolower(addResults)) && requireNamespace("qvalue")) { out$qVal <- if(is.matrix(out$p.value)) {
        try(apply(out$p.value, 2, function(x) qvalue::qvalue(x,lfdr.out=TRUE)$lfdr), silent=TRUE)} else try(qvalue::qvalue(out$p.value,lfdr.out=TRUE)$lfdr, silent=TRUE) 
        if(inherits(out$qVal, "try-error")) { 
          if(!silent) message(fxNa," Problem with pi0 estimation, setting pi0=1 like BH-FDR")
          out$qVal <- if(is.matrix(out$p.value)) { apply(out$p.value, 2, function(x) qvalue::qvalue(x, pi0=1, lfdr.out=TRUE)$lfdr)
            } else qvalue::qvalue(out$p.value, pi0=1, lfdr.out=TRUE)$lfdr }
      }    
      if(any(c("bonferroni","bonf") %in% tolower(addResults))) out$bonf <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="bonferroni")} else stats::p.adjust(out$p.value, meth="bonferroni")
    }
    
    if("nonMod" %in% addResults) { leLev <- length(levels(grp))
      grX <- lapply(2:leLev, function(x) which(grp==levels(grp)[x]))
      grX[2:leLev] <- grX[1:(length(levels(grp)) -1)]
      grX[[1]] <- which(grp==levels(grp)[1])
      names(grX) <- levels(grp)
      altHyp <- testOrientation
      if(altHyp=="=") altHyp <- "two.sided"
      comp <- triCoord(leLev)
      out$nonMod.p <- apply(comp,1,function(y) apply(dat, 1, function(x) stats::t.test(x[which(grp==levels(grp)[y[1]])], x[which(grp==levels(grp)[y[2]])], alternative=altHyp)$p.value) )
      colnames(out$nonMod.p) <- compNa
      if(any(c("FDR","BH") %in% toupper(addResults))) {
        if(length(dim(out$nonMod.p))==2) { out$nonMod.FDR <- apply(out$nonMod.p, 2, stats::p.adjust, method="BH")
        colnames(out$nonMod.FDR)  <- compNa } else out$nonMod.FDR <- stats::p.adjust(out$nonMod.p, method="BH")
      }
      if(any("BY" %in% toupper(addResults))) {
        if(length(dim(out$nonMod.p))==2) { out$nonMod.BY <- apply(out$nonMod.p, 2, stats::p.adjust, method="BY")
        colnames(out$nonMod.BY)  <- compNa } else out$nonMod.BY <- stats::p.adjust(out$nonMod.p, method="BY")
      }
      if(any("lfdr" %in% tolower(addResults)) && requireNamespace("fdrtool")) {
        if(length(dim(out$nonMod.p))==2) { out$nonMod.lfdr <- apply(out$nonMod.p, 2, pVal2lfdr)
        colnames(out$nonMod.lfdr)  <- compNa } else out$nonMod.lfdr <- pVal2lfdr(out$nonMod.p)
      }
      if(any(c("qval","qvalue") %in% tolower(addResults)) && requireNamespace("qvalue")) {
        out$nonMod.qVal <- if(length(dim(out$nonMod.p))==2) try(apply(out$nonMod.p, 2, qvalue::qvalue), silent=TRUE) else try(qvalue::qvalue(out$nonMod.p), silent=TRUE)
        if(inherits(out$nonMod.qVal, "try-error")) { if(!silent) message(fxNa,"Problem with pi0 estimation (non-shrinked p-values) for qValue, setting pi0=1 like BH-FDR")
          out$nonMod.qVal <- if(length(dim(out$nonMod.p))==2) apply(out$nonMod.p, 2, qvalue::qvalue,pi0=1, lfdr.out=TRUE) else qvalue::qvalue(out$nonMod.p,pi0=1, lfdr.out=TRUE)
        }
        if(length(dim(out$nonMod.p))==2) colnames(out$nonMod.qVal)  <- compNa 
      }
      if(any(c("bonferroni","bonf") %in% tolower(addResults))) {
        if(length(dim(out$nonMod.p))==2) { out$nonMod.bonf <- apply(out$nonMod.p, 2, stats::p.adjust, method="bonferroni")
        colnames(out$nonMod.bonf)  <- compNa } else out$nonMod.lfdr <- stats::p.adjust(out$nonMod.p, method="bonferroni")
      }
    }
    out } }  
   
