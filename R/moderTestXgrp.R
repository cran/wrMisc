#' Multiple Moderated Pairwise t-tests From limma
#'
#' This function runs (selected or all) pairwise combinations of moderated t-tests from package 'limma' on each line of data against.
#' Note: This function requires the package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} from bioconductor. 
#'    The limma contrast-matrix has to be read by column, the lines in the contrast-matrix containing '+1' will be compared to the '-1' lines, eg grpA-grpB .
#' 
#' @details
#' When multiple pairwise comparisons will be run, first a global linear model will be estimated and the particular pairwise comparisons will then be performed using a contrast-matrix.
#' This process is described with the bioconductor package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} which is used underneith.
#' 
#' By default (\code{useComparison=NULL}), the first of all possible pairwise comparisons will be run.
#' If you would like to run all comparisons, plase set \code{useComparison="all"}.
#' Besides, it is also possible to custom choose which comparisons should be run (and which order of sample/reference) via the argument \code{useComparison} using
#' the character vector '--' as separator for the two groups of samples (referring to argument \code{grp}) to be compared.
#' This will be interpreted as 'sample--reference' (additional space around the separator, if present, will be removed), thus the second element will be used as reference. 
#' Furthermore, the argument \code{useComparison} may be a matrix of 2 columns (for sample and reference) where each line represents a pairwise comparison that should be run.  
#' 
#' As effort for compatibility to previous versions and compatibility to standard writing in limma the group-separator '-' in \code{useComparison} has limited support : 
#' This (single character) separator will be internally converted to '--' and in absence of '-' in argument \code{grp}, output will have the initial  (single character) separator '-'.
#' 
#' Concerning the separator used when reporting results for pairwise comparisons :
#' If argument \code{grp} contains any '-', comparisons of groups will always (!) get reported using '--' as separator to avoid any confusion.
#' In case \code{useComparison} is a matrix, the reported comparisons will have '-' if \code{grp} does not contain as well any '-', otherwise '--' will be used 
#' (to support compatibility with results from limma).
#' 
#' Please note, that in the output of this function the Benjamini-Hochberg (BH) adjusted p-values are called 'FDR' (see argument \code{addResults}).
#' 
#' the output caontains a list-element named \code{setup} summarizing the global setup as captured (names of groups of samples), including the names of the pairwise combinations that were tested.
#'
#' @param dat matrix or data.frame with rows for multiple (independent) tests, use ONLY with 2 groups; assumed as log2-data !!!
#' @param grp (factor) describes column-relationship of 'dat'   (1st factor is considered as reference -> orientation of M-values !!)
#' @param useComparison (numeric, character or matrix) optional way to indicate which pairwise comparisons should be performed :
#'      if \code{useComparison="all"} all pairwise comparisons possible will be performed,
#'      otherise pairwise comparisons may be chosen using indexes (the ith instance of all possible pairwise comparisons);
#'      a character vector citing the concatenated levels of  argument \code{grp}; may be used (in this case it may be necessary to specify the separator used in argument argument \code{sep} ;
#'      please note, it is important to check that a custom selected separator \code{sep} does not occur inside the groups from \code{grp});
#'      finally, the argument \code{useComparison} may be or as matrix of 2 columns where each row specifies a different comparison, 
#'      the matrix mat contain the names of the levels of argument \code{grp} or an index relative to the levels of argument \code{grp};
#'      In all cases where 2 groups are specified this will be read as 'sample-reference' - thus, the 2nd entity will be taken as reference;
#'      Of course this separator should not occur in 'grp' (if present it will be automatically repaced by '__' in 'grp' ).
#' @param limmaOutput (logical) return full (or extended) MArrayLM-object from limma or 'FAlSE' for only the (uncorrected) p.values
#' @param addResults (character) types of results to add besides basic limma-output, data are assumed to be log2 ! (eg "lfdr" using fdrtool-package, "FDR" or "BH" for BH-FDR, "BY" for BY-FDR, 
#'   "bonferroni" for Bonferroni-correction, "qValue" for lfdr by qvalue, "Mval", "means" or "nonMod" for non-moderated test and he equivaent all (other) multiple testing corrections chosen here)
#' @param testOrientation (character) for one-sided test (">","greater" or "<","less"), NOTE : 2nd grp is considered control/reference, '<' will identify grp1 < grp2
#' @param sep (\code{NULL]} or character of length=1) optional custom choice for separator for column-names of pairwise comparisons, otherwise determined as character(-set) not occurring in \code{grp}
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a limma-type MA-object (which can be handeled like a list), or when problems are encountered \code{NULL}
#' @seealso \code{\link{moderTest2grp}} for simplified single comparisons (with fewer options/flexibility), \code{\link[limma]{lmFit}} and the \code{eBayes}-family of functions in package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}
#' @examples
#' grp3 <- factor(rep(LETTERS[c(3,1,4)],c(2,3,3)))
#' set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4),2), ncol=8,
#'   dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste0(grp3, c(1:2,1:3,1:3))))
#' t8[3:6,1:2] <- t8[3:6,1:2] +3                    # augment lines 3:6 (c-f) 
#' t8[5:8,c(1:2,6:8)] <- t8[5:8,c(1:2,6:8)] -1.5    # lower lines 
#' t8[6:7,3:5] <- t8[6:7,3:5] +2.2                  # augment lines 
#' ## expect to find C/A in c,d,g, (h)
#' ## expect to find C/D in c,d,e,f
#' ## expect to find A/D in f,g,(h) 
#' test8 <- moderTestXgrp(t8, grp3) 
#' 
#' ## Custom choice of what to compare
#' test8b <- moderTestXgrp(t8, grp3, useComparison=c("D-A","C-A")) 
#' head(test8b$FDR)
#' head(test8b$Mval)
#' 
#' ## Custom choice as matrix
#' comp4 <- matrix(LETTERS[c(4,3,3,1)], ncol=2)
#' test4 <- moderTestXgrp(t8, grp3, useComparison=comp4)
#' 
#' ## Names of pairwise comparisons performed
#' test4$setup$concat
#'
#' ## One can also use functions from package limma to see more
#' library(limma)
#' topTable(test8b, n=5)
#' @export
moderTestXgrp <- function(dat, grp,  useComparison=NULL, limmaOutput=TRUE, addResults=c("lfdr","FDR","Mval","means"), testOrientation="=", sep=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="moderTestXgrp")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(!isFALSE(limmaOutput)) limmaOutput <- TRUE
  runTest <- TRUE
  pwIndex <- sep1 <- indX <- useComparisonNa <- NULL
  .extrSep2 <- function(comb , indiv) {     ## extract separator by striping indiv elements (iniv) from combined pairwise names (comb)  .. move to wrMisc ??
    fx2 <- function(x, y) sub(paste0(protectSpecChar(x),"$"),"", sub(paste0("^",protectSpecChar(x)),"", y))   # remove x from head and tail   (eg rm C from end)
    for(i in indiv) comb <- fx2(i, comb)
    unique(comb[which(nchar(comb) >0)])   # will/might return NULL when sep==""
  }
  
  if(any(length(dat) <1, length(dim(dat)) !=2, dim(dat) < c(1,3))) { runTest <- FALSE
    warning(fxNa,"Invalid argument 'dat'; must be matrix or data.frame with min 1 lines and 3 columns") }
  if(runTest) {  
    if(length(grp) != ncol(dat)) { runTest <- FALSE
      warning(fxNa,"Check parameters: Number of columns of 'dat' doesn't match to length of 'grp'") }}  
  if(runTest) {  
    if(!is.factor(grp)) grp <- as.factor(grp)
    if(length(levels(grp)) <2) stop("Need at least 2 groups in argument 'grp'")
    ## check packages dependencies 
    packages <- c("limma", "fdrtool", "qvalue")
    checkPkg <- function(pkg) requireNamespace(pkg, quietly=TRUE)
    checkPkgs <- sapply(packages, checkPkg)
    if(!checkPkgs[1]) { runTest <- FALSE; message(fxNa,"NOTE: Package 'limma' not found ! Unable to run tests. Please install first from Bioconductor")} } 
  if(debug) {message(fxNa,"mTX0"); mTX0 <- list(dat=dat,grp=grp,useComparison=useComparison,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest,sep=sep)}
    
  if(runTest && requireNamespace("limma")) {
    if(limmaOutput && length(addResults) >0) if("all" %in% addResults) addResults <- c("BH", "BY","lfdr","qValue","bonferroni","Mval","means","nonMod")
    if(!checkPkgs[2] && any("lfdr" %in% tolower(addResults))) {
      if(!silent) message(fxNa,"Package 'fdrtool' not found, omitting .. Please install from CRAN for enabeling 'lfdr'")
      addResults <- addResults[-which(tolower(addResults) %in% "lfdr")] }
    if(!checkPkgs[3] && any(c("qvalue","qval") %in% tolower(addResults))) {
      if(!silent) message(fxNa,"Package 'qvalue' not found, omitting .. Please install from Bioconductor for enabeling 'qValue'")
      addResults <- addResults[-which(tolower(addResults) %in% c("qvalue","qval"))] }
    if(debug) {message(fxNa,"mTX00")}
    ## treat non-unique row-names ?

  	if(length(rownames(dat)) > 0 && length(unique(rownames(dat))) < nrow(dat)) {
  	  if(!silent) message(fxNa, "Detected ", nrow(dat) - length(unique(rownames(dat))), " non-unique rownames of 'dat' !")
  	}
    altHyp <- "two.sided"                                             # default, change only if explicit sign recognized
    if(length(testOrientation) <1) testOrientation <- altHyp
    if(testOrientation %in% c("<","less","inf")) altHyp <- "less"           
    if(testOrientation %in% c(">","greater","sup")) altHyp <- "greater"
    
    ## prepare modeling - initial design-matrix 
    datDesign <- stats::model.matrix(~ -1 + grp)                  # can't use directly, need contrasts !!
    colnames(datDesign) <- sub("^grp","", colnames(datDesign))
    if(debug) {message(fxNa,"mTX0a"); mTX0a <- list(dat=dat,useComparison=useComparison,grp=grp,datDesign=datDesign,sep=sep)}    # ,compNames=compNames

    ## option 1: no useComparison (NULL) => do all pw combin as before
    ## option 2: useComparison as char vector (using '--' as sep, need to check if index)   
    ## option 3: useComparison as 2-dim (numeric) matrix (need to check if index, indexes should refer to unique(gr) and NOT to levels(gr) !), as from wrProteo

    ## note : sep .. as user-provided; sep1 .. finally used (en from automat determin)    
    if(length(sep) >1) {
      message(fxNa,"Expecting argument 'sep' to be of length=1, trimming to 1st (not NA and min 1 character long)")
      sep <- sep[which(!is.na(sep) & nchar(sep) >0)]
      if(length(sep) >1) sep <- sep[1]
    }
    ## check & convert useComparison
    # useComparison=NULL #  => 1st comparison # if(identical("all",useComparison)) useComparison <- NULL

    chComp <- convPairwiseSetup(useComparison, grp=grp, experSetup=NULL, sep=sep, silent=silent, debug=debug, callFrom=fxNa)
    pwIndex <- chComp$pwIndex
    chComp$grp <- grp
  	names(grp) <- colnames(dat)

    concatNa <- if(length(sep)==0) chComp$concat else { paste(chComp$pwGrpNa[,1], chComp$pwGrpNa[,2], sep=sep)	}
	
    pwGrpNa <- chComp$pwGrpNa
    sep1 <- chComp$sep
    grpNa <- chComp$grpNa
    ## now pwIndex (matrix of index for question-parts) is finished           
    if(debug) {message(fxNa,"mTX2a"); mTX2a <- list(dat=dat,grp=grp,useComparison=useComparison,sep=sep,sep1=sep1,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest,datDesign=datDesign,pwIndex=pwIndex,chComp=chComp,grpNa=grpNa, indX=indX) }   #concatNa=concatNa,
    convSingSep <- FALSE                  # initialize
     
    if(debug) {message(fxNa,"mTX3"); mTX3 <- list(dat=dat,grp=grp,useComparison=useComparison,sep1=sep1,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest,datDesign=datDesign,pwIndex=pwIndex,pwGrpNa=pwGrpNa,grpNa=grpNa )}
  }
    
  ## main testing
  if(runTest && requireNamespace("limma") && length(pwIndex) >0 && length(dat) >0 && all(!is.na(pwIndex))) {
    ## contrast matrix
    ## see eg   https://support.bioconductor.org/p/57268/; https://www.biostars.org/p/157068/
  	contr.matr <- matrix(0, nrow=length(levels(grp)), ncol=nrow(pwIndex), 
  		dimnames=list(levels(grp), rownames(pwIndex)))
  	for(j in 1:nrow(pwIndex)) contr.matr[pwIndex[j, ], j] <- c(1,-1)
  	if(debug) { message(fxNa, "mTX3") }
    globFilt <- 1:nrow(dat)                                       # so far apply testing to all lines
    
    ## main
    fit0 <- try(limma::lmFit(dat[globFilt,], datDesign), silent=TRUE)          # testing part 1
    if(inherits(fit0, "try-error")) { runTest <- FALSE
       warning(fxNa,"Problem running lmFit(), unable to run tests; check if package 'limma' is properly installed !")}
    if(debug) {message(fxNa,"mTX4"); mTX4 <- list(dat=dat,fit0=fit0,datDesign=datDesign,grp=grp,useComparison=useComparison,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest,pwIndex=pwIndex, contr.matr=contr.matr,pwGrpNa=pwGrpNa) }       
  } else runTest <- FALSE
  
  if(runTest && requireNamespace("limma")) {
    fit1 <- limma::eBayes(limma::contrasts.fit(fit0, contrasts=contr.matr))  # variant to run all contrasts at same time
    if(debug) {message(fxNa,"mTX4b"); mTX4b <- list(dat=dat,fit0=fit0,fit1=fit1,datDesign=datDesign,grp=grp,useComparison=useComparison,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest, contr.matr=contr.matr,pwIndex=pwIndex,pwGrpNa=pwGrpNa,grpNa=grpNa,sep1=sep1,altHyp=altHyp) } # concatNa=concatNa,      
    ## note rownames(fit1$contrasts) may not be sufficient (- in case of 'A-B', 'B-A')
    #concatNa <- paste(useComparisonNa[,1],useComparisonNa[,2], sep=sep1)
      #if(debug) message(fxNa," Note: Could not find names of (multiple) comparisons .. reconstructing") }
    
    fit1$means <- rowGrpMeans(dat, grp)  
    
    ## separate running of contrasts, like gxTools, need then to extract & combine all pValues
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
      ch <- fit1$means[,2] > fit1$means[,1]
      if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
      if(any(ch)) fit1$p.value[which(ch),] <- fit1$p.value[which(ch),]/2
      if(any(!ch)) fit1$p.value[which(!ch),] <- 1- fit1$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
    }
    if(is.null(colnames(fit1$t))) colnames(fit1$t) <- concatNa
    if(is.null(colnames(fit1$p.value))) colnames(fit1$p.value) <- concatNa
    if(any(c("qval","qvalue") %in% tolower(addResults)) && !requireNamespace("qvalue") && !silent) message(fxNa,"NOTE: Package 'qvalue' not installed from CRAN, can't calulate ...")
    if(debug) {message(fxNa,"mTX5"); mTX5 <- list(dat=dat,fit0=fit0,fit1=fit1,datDesign=datDesign,grp=grp,useComparison=useComparison,limmaOutput=limmaOutput,addResults=addResults,testOrientation=testOrientation,runTest=runTest,pwIndex=pwIndex, grpNa=grpNa,contr.matr=contr.matr,pwGrpNa=pwGrpNa)  }       

    ## add various multiple testing corrections
    if(!limmaOutput) out <- fit1$p.value[,2] else { out <- fit1
      ## further inspect & correct values of 'addResults' ?
      if("Mval" %in% addResults) { 
        out$Mval <- apply(pwGrpNa,1, function(x) fit1$means[,which(colnames(fit1$means)==x[1])] - fit1$means[,which(colnames(fit1$means)==x[2])]) }      
      if(any(c("FDR","BH") %in% toupper(addResults))) out$FDR <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="BH")} else stats::p.adjust(out$p.value, meth="BH")
      if("BY" %in% toupper(addResults)) out$BY <- if(is.matrix(out$p.value)) {
        apply(out$p.value, 2, stats::p.adjust, meth="BY")} else stats::p.adjust(out$p.value, meth="BY")
      if("lfdr" %in% tolower(addResults) && requireNamespace("fdrtool")) {out$lfdr <- if(is.matrix(out$p.value)) {
        suppressWarnings(apply(out$p.value, 2, pVal2lfdr))} else suppressWarnings(pVal2lfdr(out$p.value)) }    
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
      grX[[1]] <- which(grp ==levels(grp)[1])
      names(grX) <- levels(grp)
      altHyp <- testOrientation
      if(altHyp =="=") altHyp <- "two.sided"
      pwIndex <- triCoord(leLev)
      out$nonMod.p <- apply(pwIndex,1,function(y) apply(dat, 1, function(x) stats::t.test(x[which(grp==levels(grp)[y[1]])], x[which(grp==levels(grp)[y[2]])], alternative=altHyp)$p.value) )
      colnames(out$nonMod.p) <- concatNa
      if(any(c("FDR","BH") %in% toupper(addResults))) {
        if(length(dim(out$nonMod.p)) ==2) { out$nonMod.FDR <- apply(out$nonMod.p, 2, stats::p.adjust, method="BH")
        colnames(out$nonMod.FDR)  <- concatNa } else out$nonMod.FDR <- stats::p.adjust(out$nonMod.p, method="BH")
      }
      if(any("BY" %in% toupper(addResults))) {
        if(length(dim(out$nonMod.p)) ==2) { out$nonMod.BY <- apply(out$nonMod.p, 2, stats::p.adjust, method="BY")
        colnames(out$nonMod.BY)  <- concatNa } else out$nonMod.BY <- stats::p.adjust(out$nonMod.p, method="BY")
      }
      if(any("lfdr" %in% tolower(addResults)) && requireNamespace("fdrtool")) {
        if(length(dim(out$nonMod.p)) ==2) { out$nonMod.lfdr <- apply(out$nonMod.p, 2, pVal2lfdr)
        colnames(out$nonMod.lfdr)  <- concatNa } else out$nonMod.lfdr <- suppressWarnings(pVal2lfdr(out$nonMod.p))
      }
      if(any(c("qval","qvalue") %in% tolower(addResults)) && requireNamespace("qvalue")) {
        out$nonMod.qVal <- if(length(dim(out$nonMod.p))==2) try(apply(out$nonMod.p, 2, qvalue::qvalue), silent=TRUE) else try(qvalue::qvalue(out$nonMod.p), silent=TRUE)
        if(inherits(out$nonMod.qVal, "try-error")) { if(!silent) message(fxNa,"Problem with pi0 estimation (non-shrinked p-values) for qValue, setting pi0=1 like BH-FDR")
          out$nonMod.qVal <- if(length(dim(out$nonMod.p))==2) apply(out$nonMod.p, 2, qvalue::qvalue,pi0=1, lfdr.out=TRUE) else qvalue::qvalue(out$nonMod.p,pi0=1, lfdr.out=TRUE)
        }
        if(length(dim(out$nonMod.p)) ==2) colnames(out$nonMod.qVal)  <- concatNa 
      }
      if(any(c("bonferroni","bonf") %in% tolower(addResults))) {
        if(length(dim(out$nonMod.p)) ==2) { out$nonMod.bonf <- apply(out$nonMod.p, 2, stats::p.adjust, method="bonferroni")
        colnames(out$nonMod.bonf)  <- concatNa } else out$nonMod.lfdr <- stats::p.adjust(out$nonMod.p, method="bonferroni")
      }

    }

    out$setup <- chComp  #list(sep=sep1, grpNa=grpNa, pwGrpNa=pwGrpNa, pxIndex=pwIndex)             # document sep used
    if(debug) {message(fxNa,"mTX6"); mTX6 <- list(dat=dat,fit1=fit1,datDesign=datDesign,grp=grp,useComparison=useComparison,out=out,addResults=addResults,limmaOutput=limmaOutput) }       
    
    if(convSingSep) {
      ## reset comparison to initial '-'
      for(i in 1:length(out)) if(length(dim(out[[i]])) >0) { ch1 <- grepl("--", colnames(out[[i]])); if(any(ch1)) colnames(out[[i]])[which(ch1)] <- sub("--","-", colnames(out[[i]])[which(ch1)]) }
    }
    
    ## problem : colnames for Mval,FDR etc not correct
    
    out
  } }  
             
