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
#' @param addResults (character) types of results to add besides basic limma-output (eg "lfdr" using packege fdrtools-package,"FDR" for BY-FDR,"Mval" (assumes that data are log2 !),"means" or "nonMod" for non-moderated test)
#' @param testOrientation (character) for one-sided test (">","greater" or "<","less"), NOTE : 2nd grp is considered control/reference, '<' will identify grp1 < grp2
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return limma-type MA-object (list)
#' @seealso \code{\link[limma]{lmFit}} and the \code{eBayes}-family of functions in package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma}
#' @examples
#' set.seed(2017); t8 <- matrix(round(rnorm(1600,10,0.4),2),ncol=8,
#'   dimnames=list(paste("l",1:200),c("AA1","BB1","CC1","DD1","AA2","BB2","CC2","DD2")))
#' t8[3:6,1:2] <- t8[3:6,1:2]+3     # augment lines 3:6 for AA1&BB1
#' t8[5:8,5:6] <- t8[5:8,5:6]+3     # augment lines 5:8 for AA2&BB2 (c,d,g,h should be found)
#' t4 <- log2(t8[,1:4]/t8[,5:8])
#' fit4 <- moderTest2grp(t4,gl(2,2))
#' limma::topTable(fit4,coef=1,n=5)                      # effect for 3,4,7,8
#' fit4in <- moderTest2grp(t4,gl(2,2),testO="<")
#' limma::topTable(fit4in,coef=1,n=5)
#' @export
moderTest2grp <- function(dat,grp,limmaOutput=TRUE,addResults=c("lfdr","FDR","Mval","means"),testOrientation="=",silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="moderTest2grp")
  chPa <- try(find.package("limma"), silent=TRUE)
  if("try-error" %in% class(chPa)) stop("package 'limma' not found ! Please install from Bioconductor") 
  chFdr <- try(find.package("fdrtool"), silent=TRUE)
  if(!is.factor(grp)) grp <- as.factor(grp)
  if(length(levels(grp)) <2) stop(" need at least 2 groups in argument 'grp'")
  if(length(levels(grp)) >2) {message(fxNa," capable only of treating 2 groups in argument 'grp' (ignore rest)")
    grp[which(grp %in% levels(grp)[-(1:2)])] <- NA }
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
  datDesign <- stats::model.matrix(~ grp)
  if(nrow(datDesign) < ncol(dat)) datDesign <- rbind(datDesign,matrix(rep(0,length(levels(grp))*(ncol(dat)-nrow(datDesign))),ncol=ncol(datDesign)))
  fit1 <- limma::eBayes(limma::lmFit(dat, design=datDesign))                ## Fitting linear model
  fit1$means <- rowGrpMeans(dat,grp) 
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
    ## testing for Ho A > B
    ch <- fit1$means[,2] > fit1$means[,1]
    if(!silent) message(fxNa,tx[1],altHyp,tx[2],sum(ch),tx[3:5])
    if(any(ch)) fit1$p.value[which(ch),] <- fit1$p.value[which(ch),]/2
    if(any(!ch)) fit1$p.value[which(!ch),] <- 1- fit1$p.value[which(!ch),]/2        # !(A > B)  .. A <= B
  }
  if(!limmaOutput) out <- fit1$p.value[,2] else { out <- fit1
    ## further inspect & correct values of 'addResults' ?
    if("Mval" %in% addResults) out$Mval <- (out$means[,1] - out$means[,2]) 
    if("FDR" %in% toupper(addResults)) out$FDR <- if(is.matrix(out$p.value)) {
      apply(out$p.value,2,stats::p.adjust,method="BY")} else stats::p.adjust(out$p.value,method="BY")
    if("lfdr" %in% tolower(addResults)) out$lfdr <- if(is.matrix(out$p.value)) {
      apply(out$p.value,2,pVal2lfdr)} else pVal2lfdr(out$p.value)
    }
  if("nonMod" %in% addResults) {gr1 <- which(grp==levels(grp)[1])
    gr2 <- which(grp==levels(grp)[2])
    altHyp <- testOrientation
    if(altHyp=="=") altHyp <- "two.sided" 
    out$nonMod.p <- apply(dat,1,function(x) stats::t.test(x[gr1],x[gr2],alternative=altHyp)$p.value)
    out$nonMod.FDR <- stats::p.adjust(out$nonMod.p,method="BY")                                                           
    out$nonMod.lfdr <- pVal2lfdr(fit1$nonMod.p) } 
  out }
    
