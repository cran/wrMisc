#' Multiple moderated pair-wise t-tests from limma
#'
#' Runs all pair-wise combinations of moderated t-tests from package 'limma' on each line of data against 1st group from 'grp'.
#' Note: This function requires the package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} from bioconductor. 
#' The limma contrast-matrix has to be read by column, the lines in the contrast-matrix containing '+1' will be compared to the '-1' lines, eg grpA-grpB .
#'
#' @param dat matrix or data.frame with rows for multiple (independent) tests, use ONLY with 2 groups; assumed as log2-data !!!
#' @param grp (factor) describes column-relationship of 'dat'   (1st factor is considered as reference -> orientation of M-values !!)
#' @param limmaOutput (logical) return full (or extended) MArrayLM-object from limma or 'FAlSE' for only the (uncorrected) p.values
#' @param addResults (character) types of results to add besides basic limma-output (eg "lfdr" using fdrtool-package,"FDR" for BY-FDR,"Mval" (assumes that data are log2 !),"means" or "nonMod" for non-moderated test)
#' @param testOrientation (character) for one-sided test (">","greater" or "<","less"), NOTE : 2nd grp is considered control/reference, '<' will identify grp1 < grp2
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return limma-type MA-object (list)
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
#' test8 <- moderTestXgrp(t8,grp) 
#' head(test8$p.value,n=8)
#' @export
moderTestXgrp <- function(dat,grp,limmaOutput=TRUE,addResults=c("lfdr","FDR","Mval","means"),testOrientation="=",silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="moderTestXgrp")
  chPa <- try(find.package("limma"), silent=TRUE)
  if("try-error" %in% class(chPa)) stop("package 'limma' not found !") 
  chFdr <- try(find.package("fdrtool"), silent=TRUE)  
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(length(levels(grp)) <2) stop(" need at least 2 groups in argument 'grp'")
  ## treat non-unique row-names ?
  if(length(rownames(dat) >0)) if(length(unique(rownames(dat))) < nrow(dat)) {
    if(!silent) message(fxNa," detected ",nrow(dat)-length(unique(rownames(dat)))," non-unique rownames of 'dat' !")}
  altHyp <- "two.sided"                                             # default, change only if explicit sign recognized
  if(length(testOrientation) <1) testOrientation <- altHyp
  if(testOrientation %in% c("<","less","inf")) altHyp <- "less"           
  if(testOrientation %in% c(">","greater","sup")) altHyp <- "greater"
  if(length(addResults) >0) if("lfdr" %in% tolower(addResults) & "try-error" %in% class(chFdr)) {
    if(!silent) message(fxNa,"package 'fdrtool' not found ! Please install package fdrtool from CRAN for enabeling 'lfdr' estimations") 
    addResults <- addResults[which(!tolower(addResults) %in% "lfdr")] }  
  datDesign <- stats::model.matrix(~ -1 + grp)                  # can't use directly, need contrasts !!
  colnames(datDesign) <- sub("^grp","", colnames(datDesign))
  comp <- triCoord(length(levels(grp)))
  rownames(comp) <- paste(levels(grp)[comp[,1]], levels(grp)[comp[,2]],sep="-")
  contr.matr <- matrix(0, nrow=length(levels(grp)), ncol=nrow(comp), dimnames=list(levels(grp),rownames(comp)))
  for(j in 1:nrow(comp)) contr.matr[comp[j,],j] <- c(1,-1)
  ## see eg   https://support.bioconductor.org/p/57268/; https://www.biostars.org/p/157068/
  globFilt <- 1:nrow(dat)  # not used yet
  fit0 <- try(limma::lmFit(dat[globFilt,], datDesign))          # [sampleOrder,qcDat]
  if("try-error" %in% class(fit0)) message(fxNa," Problem running lmFit(),  check if package 'limma' is installed !?!")
  fit1 <- limma::eBayes(limma::contrasts.fit(fit0, contrasts=contr.matr))  # variant to run all contrasts at same time
  compNa <- colnames(fit1$contrasts)
  if(is.null(compNa) & !silent) message(fxNa," Note: Could not find names of (multiple) comparisons !")
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
  if(!limmaOutput) out <- fit1$p.value[,2] else { out <- fit1
    ## further inspect & correct values of 'addResults' ?
    if("Mval" %in% addResults) out$Mval <- (out$means[,1] - out$means[,2]) 
    if("FDR" %in% toupper(addResults)) out$FDR <- if(is.matrix(out$p.value)) {
      apply(out$p.value, 2, stats::p.adjust, meth="BY")} else stats::p.adjust(out$p.value, meth="BY")
    if("lfdr" %in% tolower(addResults)) {out$lfdr <- if(is.matrix(out$p.value)) {
      apply(out$p.value, 2, pVal2lfdr)} else pVal2lfdr(out$p.value)
      }    
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
    out$nonMod.FDR <- if(length(dim(out$nonMod.p))==2) apply(out$nonMod.p,2,stats::p.adjust, method="BY") else stats::p.adjust(out$nonMod.p, method="BY")
    out$nonMod.lfdr <- if(length(dim(out$nonMod.p))==2) apply(out$nonMod.p, 2, pVal2lfdr) else pVal2lfdr(out$nonMod.p)
    colnames(out$nonMod.p) <- colnames(out$nonMod.FDR)  <- colnames(out$nonMod.lfdr) <- compNa 
    }
  out }  
   
