#' Search Points Forming Lines At Given Slope
#'
#' \code{searchLinesAtGivenSlope} searchs among set of points (2-dim) those forming line(s) with user-defined slope ('coeff'),
#'  ie search optimal (slope-) offset parameter(s) for (regression) line(s) with given slope ('coef').
#'  Note: larger data-sets : segment residuals to 'coeff' & select most homogenous
#' 
#' Note: The package MASS is required when using as \code{lmCompare=TRUE}.
#' For larger data the function will try using the package \code{NbClust} (available from CRAN) if installed.
#' 
#' @param dat matrix or data.frame, main input
#' @param coeff (numeric) slope to consider
#' @param filtExtr (integer) lower & upper quantile values, remove points with extreme deviation to offset=0, (if single value: everything up to or after will be used)
#' @param minMaxDistThr (logical) optional minumum and maximum distance threshold
#' @param lmCompare (logical) add'l fitting of linear regression to best results, return offset AND slope based on lm fit
#' @param indexPoints (logical) return results as list with element 'index' specifying retained points
#' @param displHist (logical) display histogram of residues
#' @param displScat (logical) display (simple) scatter plot
#' @param bestCluByDistRat (logical) initial selection of decent clusters based on ratio overallDist/averNeighbDist (or by CV & cor)
#' @param neighbDiLim (numeric) additional threshold for (trimmed mean) neighbour-distance
#' @param silent (logical) suppress messages
#' @param debug (logical) for bug-tracking: more/enhanced messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso \code{\link[stats]{lm}}
#' @return This functions returns a matrix of line-characteristics  (or if indexPoints is \code{TRUE} then list (line-characteristics & index & lm-results)
#' @examples
#' set.seed(2016); ra1 <- runif(300)
#'  dat1 <- cbind(x=round(c(1:100+ra1[1:100]/5,4*ra1[1:50]),1),
#'   y=round(c(1:100+ra1[101:200]/5, 4*ra1[101:150]), 1))
#' (li1 <- searchLinesAtGivenSlope(dat1, coeff=1))
#' @export
searchLinesAtGivenSlope <- function(dat, coeff=1.5, filtExtr=c(0,1), minMaxDistThr=NULL, lmCompare=TRUE, indexPoints=TRUE,
  displHist=FALSE, displScat=FALSE, bestCluByDistRat=TRUE, neighbDiLim=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="searchLinesAtGivenSlope")
  opar <- list(mfrow=graphics::par("mfrow"), mfcol=graphics::par("mfcol"))
  on.exit(graphics::par(opar$mfrow))
  on.exit(graphics::par(opar$mfcol))
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(isTRUE(lmCompare)) { 
    if(!requireNamespace("MASS", quietly=TRUE)) warning(fxNa,"Package 'MASS' not found ! Please install first from CRAN \n   setting 'lmCompare' to FALSE")
    lmCompare <- FALSE } else lmCompare <- FALSE 
  minNPoints <- 8                            ## min no of points to be included in primary selection of groups/clusters
  cvThr <- c(0.06,0.09)                      ## threshold for 1st flitering of clustering results
  argNa <- deparse(substitute(dat))

  coeff <- convToNum(coeff, spaceRemove=TRUE, convert=c(NA,"sparseChar"), remove=NULL, euroStyle=TRUE, sciIncl=TRUE, callFrom=fxNa)
  if(length(coeff) <1) stop(" 'coeff' should be numeric of length 1") else coeff <- coeff[1]
  offS <- offS0 <- dat[,2] - coeff*dat[,1]
  if(identical(filtExtr, c(0,1))) filtExtr <- NULL
  if(is.numeric(filtExtr)) {                 ## remove extreme points (needed for all appoaches ??)
    if(length(filtExtr) !=2) filtExtr <- sort(c(filtExtr, if(filtExtr[1] <0.5) 1 else 0)[1:2])
    offSL <- stats::quantile(offS, filtExtr, na.rm=TRUE)           #
    filt1 <- which(offS > offSL[1] & offS < offSL[2])
  } else filt1 <- 1:nrow(dat)                # filt1 : index for which lines of 'dat' are actually condsidered ..
  ## exclude lines containing any NA
  if(sum(is.na(dat[,1:2]) >0)) { exclu <- which(rowSums(is.na(dat[,1:2])) >0)
    filt1 <- filt1[-1*which(filt1 %in% exclu)] }
  filt0 <- (1:nrow(dat))[-1*filt1]                         # opposite of filt1, ie index numbers of points(lines of dat) excluded
  ## main : segment - if data sufficiently large
  bestPart <- if(length(filt1) >60 ) { 
    .insp1dimByClustering(offS[filt1], automClu=TRUE, cluChar=TRUE, silent=silent, debug=debug, callFrom=fxNa)
  } else list(cluster=rep(1,length(filt1)), cluChar=matrix(c(length(filt1), stats::median(offS[filt1]),
    abs(stats::sd(offS[filt1]))/mean(offS[filt1])), nrow=1, dimnames=list(1,c("n","center","centerCV"))))
  if(!silent && length(filt1) >50) message(fxNa,"Data (",nrow(dat), " lines) segemented in ",nrow(bestPart$cluChar)," cluster(s)")
  ## note : cluster-names MUST be numeric-like !!  (no letters !)
  ## now add info from 2 dim data
  ## note : problem with using cor: small clusters have tendency for high cor ...
  ## choose base on 'refVa' (1-cor) x (centerCV)/ sqrt(n)   ... lower =better
  cluTab <- table(bestPart$cluster)
  disIni <- as.list(rep(NA,length(cluTab)))
  extrPtDis <- rep(NA,length(cluTab))
  for(j in as.numeric(names(cluTab))[which(cluTab > max(2,minNPoints))]) {
    cluLi <- which(bestPart$cluster==j)
    disIni[[j]] <- .neigbDis(dat[filt1[cluLi],1:2], asSum=FALSE)
    tmp <- dat[filt1[cluLi[order(rowMeans(dat[filt1[cluLi],1:2], na.rm=TRUE))]], 1:2]
    extrPtDis[j] <- sqrt(sum((tmp[1,] -tmp[length(cluLi)])^2 ))
  }
  if(debug) message(fxNa,"   ",sum(!is.na(extrPtDis))," out of ",length(extrPtDis)," distances calultated")
  xyCor <- as.numeric(unlist(by(dat[filt1,1:2], bestPart$cluster, function(x) stats::cor(x[,1],x[,2]) )))
  offSclu <- cbind(bestPart$cluChar, cor=xyCor)
  offSclu <- cbind(offSclu, refVa=(1-abs(offSclu[,"cor"]))*offSclu[,"centerCV"]/sqrt(offSclu[,"n"]))      ## <<== refVa
  offSclu <- cbind(offSclu, meanNeigbDi=sapply(disIni,mean,na.rm=TRUE,trim=0.2), maxSpread=extrPtDis)
  if(debug) message(fxNa,"   'offSclu' columns: ",paste(colnames(offSclu),collapse=" "))
  ## now refine best clusters (either by ratio overallSpread/meanNeighbDist, or by CV & cor)
  if(bestCluByDistRat && sum(cluTab >minNPoints) >1) {
    refRat <- mean(offSclu[,"meanNeigbDi"]/offSclu[,"maxSpread"], trim=0.2, na.rm=TRUE)
    refCluNo <- which(offSclu[,"meanNeigbDi"]/offSclu[,"maxSpread"] < refRat & offSclu[,"n"] >=minNPoints)
    if(length(refCluNo) <1) { message(fxNa,"Cannot find decent clusters with 'bestCluByDistRat' ") }
  } else  {
    refCluNo <- which(offSclu[,"refVa"] < stats::median(offSclu[,"refVa"],na.rm=TRUE) &&
      abs(offSclu[,"cor"]) > 0.95 && offSclu[,"centerCV"] < cvThr[1] & offSclu[,"n"] >=minNPoints)
    if(length(refCluNo) <1) { refCluNo <- which(offSclu[,"refVa"] < stats::median(offSclu[,"refVa"],na.rm=TRUE) &&
      abs(offSclu[,"cor"]) > 0.92 && offSclu[,"centerCV"] < cvThr[2] & offSclu[,"n"] >=minNPoints) }
  }
  if(debug) message(fxNa,"  select clusters ",if(length(refCluNo) >0) paste(refCluNo,collapse=" ") else "(none)"," for refining")
  if(length(refCluNo) >0) { for(j in refCluNo) {       
      newCluNo <- max(bestPart$cluster) +1
       ## this part may be further improved (.keepCenter1d may remove too much = split good clusters) 
      tmp <- .keepCenter1d(offS[filt1[which(bestPart$cluster==j)]], core="veryhigh", keepOnly=FALSE, displPlot=FALSE, silent=silent, callFrom=fxNa)   # want display of hist-refinement?
      if(!silent) message(fxNa,"Split clu no  ",j," in main (n=",length(tmp$keep),") and fragm clu ",
        newCluNo," (n=",length(tmp$drop),")")
      bestPart$cluster[which(bestPart$cluster==j)[tmp$drop]] <- newCluNo }
    if(debug) message("   now at ",newCluNo," groups/clusters")
    cluN <- bestPart$cluster
    offSclu <- cbind(n=table(cluN), center=tapply(offS[filt1], cluN, mean, na.rm=TRUE), centerSd=tapply(offS[filt1], cluN, stats::sd, na.rm=TRUE))
    ## set cor to 0 if cor can't get caluclated due to indentical values in one of the colums
    xyCor <- as.numeric(unlist(by(dat[filt1,1:2],cluN,function(x) {if(length(unique(x[,1])) >1 && length(unique(x[,2])) >1) stats::cor(x[,1],x[,2]) else 0} ))) 
    refVa <- (1 -abs(xyCor)) *offSclu[,"centerSd"]/ (offSclu[,"center"] *sqrt(offSclu[,"n"]))
    offSclu <- cbind(offSclu ,centerCV=abs(offSclu[,"centerSd"]/offSclu[,"center"]), cor=xyCor, refVa=refVa)
    if(debug) message(fxNa,"  updated xyCor,offSclu ...")
  }
  if(length(refCluNo) <1) {                  ## nothing found so far, rather choose big cluster with cor > 50%
    if(debug) message(fxNa,"  nothing found so far, rather choose big cluster with cor > 50%")
    tmp3 <- which(offSclu[,"n"] >3)
    if(nrow(offSclu) ==1){ refCluNo <- 1 } else {                     # single cluster (no choice)
    if(length(tmp3) <2) refCluNo <- which.max(offSclu[,"n"]) else {   # all clusters smaller <4, choose largest
      tmp <- offSclu[which(offSclu[,"n"] >3),]                        # or select amongst clusters >3
     if(length(dim(tmp)) >1){
        tmp2 <- which(abs(tmp[,"cor"]) >= stats::median(abs(tmp[,"cor"]), na.rm=TRUE))
        tmp <- log(tmp[tmp2,"n"])/20 + tmp[tmp2,"cor"] -10*tmp[tmp2,"centerCV"]       # compromise betw n,cor & centerCV
        refCluNo <- as.numeric(names(tmp2)[which.max(tmp2)])
      } else refCluNo <- which(offSclu[,"n"] >3)
    } }
    if(!silent) message(fxNa,"Cannot easily identify clusters for refining, use (large & decent cor) clu: ",refCluNo)
  }
  ## get characteristics (of best clusters)   (no need to refresh refCluNo, OK)
  ## add parameter for checking if points are compact : sort, sum of each dist.to.neighb
  neigbDist <- rep(NA, nrow(offSclu))
  for(j in which(offSclu[,"n"] >1)) {
    neigbDist[j] <- mean(.neigbDis(dat[filt1[which(bestPart$cluster==j)],1:2], asSum=FALSE), trim=0.1, na.rm=TRUE)}  # 10% each trimmed mean(dist)
  offSclu <- cbind(offSclu,neigbDist=neigbDist)
  ## return matrix 'offT' with line-characteristics (for selected/best) or list (line-characteristics & index)
  reportLi <- unique(c(refCluNo, which(offSclu[,"n"] > min(4,minNPoints))))             
  offT <- matrix(nrow=length(reportLi), ncol=10, dimnames=list(reportLi, c("n","medOffS","CVoffS","r","neigbDist",
    "CIlo","CIhi","CIov","grade","grade2")))
  for(j in reportLi){                
    sel <- which(bestPart$cluster==j)
    CI <- sort(stats::t.test(offS[filt1[sel]])$conf.int)
    offT[as.character(j),] <- c(n=length(sel), medOffS=stats::median(offS[filt1[sel]]),
      CVoffS=abs(stats::sd(offS[filt1[sel]])/mean(offS[filt1[sel]])), r=stats::cor(dat[filt1[sel],1],dat[filt1[sel],2]),
      neigbDist=.neigbDis(dat[filt1[which(bestPart$cluster==j)],1:2])/sum(bestPart$cluster==j),
      CIlo=CI[1], CIhi=CI[2], CIov=NA, grade=NA, grade2=NA) }
  offT[,"grade"] <- signif(log(offT[,"n"])/15 + offT[,"r"] -10*offT[,"CVoffS"],3)    # 'grade' .. consider n,r & CVoffS; higher..better
  if(debug) message(fxNa,"  offT created, columns ",paste(colnames(offT),collapse=" "))
  refCluSel <- which(refCluNo %in% rownames(offT)[which(offT[,"neigbDist"] <= neighbDiLim)])
  if(!is.null(neighbDiLim) && length(refCluSel) >0) refCluNo <- refCluNo[refCluSel]
  ## rank2 : rank of 'grade' among top-hits ('refCluNo'), here with 1 for highest=best 'grade'
  offT[match(refCluNo, rownames(offT)),"grade2"] <- (nrow(offT) +1 -rank(offT[,"grade"]))[match(refCluNo, rownames(offT))]
  if(indexPoints) {
    if(length(filt0) >0) bestPart$cluster[filt0] <- NA
    out <- list(offS=offT, clus=bestPart$cluster, index=lapply(refCluNo, function(x) which(bestPart$cluster==x)))
  } else out <- offT
  ## compare to lm fit                                            
  if(debug) message(fxNa,"  ..ready to refine ",length(refCluNo)," groups by lm (conserve = ",is.list(out),")")
  if(lmCompare && requireNamespace("MASS")) {for(i in 1:length(refCluNo)) {
    j <- refCluNo[i]
    dat2 <- as.data.frame(matrix(dat[filt1[which(bestPart$cluster==j)],1:2], ncol=2))
    colnames(dat2) <- c("slope","B")     # LETTERS[1:2]
    tryLm <- try(MASS::rlm(B ~ slope, data=dat2))
    if(debug) message("  ..i=",i," used ",c("MASS::rlm","stats::lm")[1 +as.numeric(inherits(tryLm, "try-error") )])
    if(inherits(tryLm, "try-error")) {
      if(!silent) message("Group ",refCluNo[i],":  problem making robust regression (from package MASS), trying regular regression instead")
      tryLm <- try(stats::lm(B ~ slope, data=dat2))
    }
    out$lm[[i]] <- tryLm                        # needed for lmFilter()
    if(i==1) out$lmSum <- matrix(NA, nrow=length(refCluNo), ncol=6, dimnames=list(refCluNo,
      c("(Intercept)","slope","pInterc","pSlope","residSE","Rsqu")))
    if(inherits(tryLm, "try-error")) message(fxNa,"Problem making regression on group",refCluNo[i],"") else if(is.list(out)) {
      tmp <- if(inherits(tryLm, "rlm")) {
        c(2*stats::pt(-abs(stats::coef(summary(tryLm))[,3]), df=length(stats::residuals(tryLm))-1), stats::cor(tryLm$qr$qr[,1], tryLm$qr$qr[,2])^2)
        } else c(stats::coef(summary(tryLm))[,4], summary(tryLm)$adj.r.squared)
      if(any(tmp[1:2] ==0, na.rm=TRUE)) tmp[which(tmp[1:2]==0)] <- 1e-320            # avoid p=0 ...
      out$lmSum[i,] <- c(stats::coef(tryLm), tmp[1:2], summary(tryLm)$sigma,tmp[3])
    } }
    names(out$lm) <- refCluNo }
  useColPa <- if(nrow(offT) <8) 2:nrow(offT) else grDevices::rainbow(1.2*nrow(offT))[1+(1:nrow(offT))]
  useCol <- useColPa[out$clus]
  ## plot histogram of residues
  if(displHist) {
    if(displScat) if(all(graphics::par()$mfrow <2)) {graphics::layout(matrix(1:2, ncol=2)) ; message(" adjusting layout for 2 images")}  
    cluBor <- matrix(unlist(by(offS,bestPart$cluster,range)), byrow=TRUE, ncol=2)
    his <- try(graphics::hist(offS, breaks="FD", main="hist of residuals to given slope"), silent=TRUE)
    if(inherits(his, "try-error")) message(fxNa,"UNABLE to plot histogram figure !") else {
      graphics::mtext(paste(nrow(cluBor),"(best) clusters shown as grey/color boxes according to mean neighbour-point distance"),cex=0.8)
      yPos <- c(-0.03*max(his$counts),-0.005*max(his$counts))
      tmp <- unique(as.numeric(cluBor))
      graphics::rect(min(cluBor[,1]), mean(yPos), max(cluBor[,2]), yPos[2],col=grDevices::grey(0.95), border=grDevices::grey(0.7))
      graphics::segments(tmp,mean(yPos), tmp, yPos[2],col=grDevices::grey(0.7))
      graphics::rect(cluBor[refCluNo,1], yPos[1], cluBor[refCluNo,2], yPos[2], col=useColPa,border=useColPa[refCluNo]) } 
    }
  ## plot of dat
  if(displScat) {
    if(debug) message(fxNa,"  ..displScat=",displScat)
    grpChar1 <- offT[,"neigbDist"]
    names(grpChar1) <- rownames(offT)   # names gets lost when just 1 line in offT
    tit <- paste("identification of linear groups with slope ",signif(coeff,3))
    if(!all(unique(bestPart$cluster) %in% rownames(offT))) {
      cluNa <- data.frame(clu=unique(bestPart$cluster), xx=NA, stringsAsFactors=FALSE)
      tmp <- data.frame(clu=rownames(offT), offT, stringsAsFactors=FALSE)
      tmp <- merge(tmp, cluNa, by="clu", all=TRUE)
      grpChar1 <- tmp[,"neigbDist"]
      names(grpChar1) <- tmp[,"clu"]
    } 
    if(debug) message(fxNa,"Class dat ",class(dat),"   class grpNo ",class(bestPart$cluster),"   class grpChar ",class(grpChar1),
      "   class grpHighl ",class(rownames(offT)),"   class datNa ",class(argNa))
    useClu <- which.max(offT[,"n"])
    useLi <- which(bestPart$cluster==useClu)
    if(debug) message(fxNa,"  len useLi",length(useLi),"   useCol ",useCol)
    chG <- try(graphics::plot(dat[,1], dat[,2], main=tit, pch=21, bg=useCol, xlab=colnames(dat)[1], ylab=colnames(dat)[2]), silent=TRUE)
    if(inherits(chG, "try-error"))  message(fxNa,"UNABLE to plot scatter figure !") else {
      graphics::legend("topleft",paste0("clu ",rownames(offT)," ,n=",offT[,1]), text.col=1, pch=21, col=1, pt.bg=useColPa,cex=0.8,xjust=0.5,yjust=0.5)}        # as points
  }
  out }

#' Calculate residues of (2-dim) linear model 'lMod'-prediction of/for 'dat'
#'
#' This function calculates residues of (2-dim) linear model 'lMod'-prediction of/for 'dat' (using 2nd col of 'useCol' )
#'  (indexing in 'dat', matrix or data.frame with min 2 cols), using 1st col of 'useCol' as 'x'.
#' It may be used for comparing/identifying data close to regression  (eg re-finding data on autoregression line in FT-ICR)
#' 
#' @param dat matrix or data.frame, main input
#' @param lMod linear model, only used to extract coefficients offset & slope
#' @param regTy (character) type of regression model
#' @param useCol (integer) columns to use
#' @return This function returns a numeric vector of residues (for each line of dat)
#' @seealso \code{\link{searchLinesAtGivenSlope}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' @export
.predRes <- function(dat, lMod, regTy="lin", useCol=1:2){
  ## calculate residues of (2-dim) linear model 'lMod'-prediction of/for 'dat' (using 2nd col of 'useCol' )
  ##  (indexing in 'dat', matrix or data.frame with min 2 cols), using 1st col of 'useCol' as 'x'
  ## used for comparing/identifying data close to regression  (eg re-finding data on autoregression line in FT-ICR)
  ## 'lMod' .. linear model, only used to extract coefficients offset & slope
  ##    used with column useCol[1] and then column useCol[2] subtracted
  ##  return numeric vector of residues (for each line of dat)
  dat <- dat[,useCol]
  lMod <- stats::coef(lMod)
  ## lin: y~ x +d+err; inv: y~ 1/x+d+err; res: y~ (x-y)+; nor: y~ x/y+; resY: y-x~ x+; norRes: y~ (x-y)/y+; norResY: (y-x)/x~ x+; sqNorRes: y ~ (x-y)/(y*y)+; sqNorResY: (y-x)/(x*x)~ x+
  switch(regTy,                         # lMod[2] ... slope !!
    lin = lMod[2]*dat[,1] +lMod[1] -dat[,2],                      
    inv = lMod[2]/dat[,1] +lMod[1] -dat[,2],                      
    res = lMod[2]*(dat[,1] -dat[,2]) +lMod[1] -dat[,2],           
    nor = lMod[2]*dat[,1]/dat[,2] +lMod[1] -dat[,2],              
    resY = lMod[2]*dat[,1] +lMod[1] -(dat[,2] -dat[,1]),          
    norY = lMod[2]*dat[,1] +lMod[1] -dat[,2]/dat[,1],             
    norRes = lMod[2]*(dat[,1] -dat[,2])/dat[,2] +lMod[1] -dat[,2],
    norResY = lMod[2]*dat[,1] +lMod[1] -(dat[,2] -dat[,1])/dat[,1],
    sqNorRes = lMod[2]*(dat[,1] -dat[,2])/dat[,2]^2 +lMod[1] -dat[,2],
    sqNorResY = lMod[2]*dat[,1] +lMod[1] -(dat[,2] -dat[,1])/dat[,1]^2,
    other=NULL)
  }

#' Compare 'dat' to confindence interval of linare model 'lMod' (eg from lm())
#'
#' This function allows to compare 'dat' to confindence interval of linare model 'lMod' (eg from lm())
#' 
#' @param dat matrix or data.frame, main input
#' @param lMod linear model, only used to extract coefficients offset & slope
#' @param level (numeric) alpha threshold for linear model
#' @return This function returns a logical vector for each value in 2nd col of 'dat' if INSIDE confid interval
#' @seealso \code{\link{searchLinesAtGivenSlope}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' @export
.checkLmConfInt <- function(dat, lMod, level=0.95){
  ## compare 'dat' to confindence interval of linare model 'lMod' (eg from lm())
  ## suppose (y-x)~x  (ie 2nd col estimated by 1st col) : formula(lMod)
  ## return logical vector for each value in 2nd col of 'dat' if INSIDE confid interval
  ##   could be improved by more steps betw various values of confint(lMod,level=level),
  ##   so far too stringent (nothing within confint range)
  ##   or use predict() and exploit pVal
  formPat <- sub("","", stats::formula(lMod)[3])                         # inspect formula if (y-x)~x
  chResForm <- stats::formula(lMod)[2] == paste0("(",formPat," - ",sub(" 1"," 2",formPat),")()")
  mod <- cbind(main=stats::coef(lMod)[2]*dat[,1] +stats::coef(lMod)[1],
    up1=stats::confint(lMod,level=level)[2,1]*dat[,1] +stats::confint(lMod,level=level)[1,1],
    up2=stats::confint(lMod,level=level)[2,1]*dat[,1] +stats::confint(lMod,level=level)[1,2],
    lo1=stats::confint(lMod,level=level)[2,2]*dat[,1] +stats::confint(lMod,level=level)[1,2],
    lo2=stats::confint(lMod,level=level)[2,2]*dat[,1] +stats::confint(lMod,level=level)[1,1])
  moRa <- t(apply(mod, 1, range))
  out <- if(chResForm) dat[,2] -dat[,1] >= moRa[,1] & dat[,2] -dat[,1] <= moRa[,2] else NULL
  out }

#' Segment (1-dim vector) 'dat' into clusters
#'
#' This function allows aegmenting (1-dim vector) 'dat' into clusters.
#' If 'automClu=TRUE ..' first try automatic clustering, if too few clusters, run km with length(dat)^0.3 clusters
#' This function requires the package NbClust to be installed.
#' 
#' @param dat matrix or data.frame, main input
#' @param automClu (logical) run atomatic clustering
#' @param cluChar (logical) to display cluster characteristics
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns clustering (class index) or (if 'cluChar'=TRUE) list with clustering and cluster-characteristics
#' @seealso \code{\link{searchLinesAtGivenSlope}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' @export
.insp1dimByClustering <- function(dat, automClu=TRUE, cluChar=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## Segment (1-dim vector) 'dat' into clusters
  ## if 'automClu=TRUE ..' first try automatic clustering, if too few clusters, run km with length(dat)^0.3 clusters
  ## 'cluChar' .. to display cluster characteristics
  ## return clustering (class index) or (if 'cluChar'=TRUE) list with clustering and cluster-characteristics
  ## require(NbClust)
  fxNa <- .composeCallName(callFrom, newNa=".insp1dimByClustering")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(!isFALSE(automClu)) automClu <- TRUE  
  if(!isFALSE(cluChar)) cluChar <- TRUE
  out <- NULL  
  if(automClu) { if(requireNamespace("NbClust", quietly=TRUE)) {
      cluAut <- try(NbClust::NbClust(dat, method="average", index="ccc"),silent=TRUE)     # cluster only best = smallest dist
      if(inherits(cluAut, "try-error")) { warning(fxNa,"Failed to run NbClust::NbClust(); ignoring  'automClu'")
        automClu <- FALSE }
    } else {if(!silent) message(fxNa,"NOTE: Package 'NbClust' not found ! Please install first from CRAN \n   setting 'automClu'=FALSE for using kmeans()")
    automClu <- FALSE }
  }       

  if(automClu) {    
    out <- cluAut$Best.partition
    if(length(table(out)) < ceiling(length(dat)^0.16)) {
      if(!silent) message(fxNa,"Automatic clustering proposed only ",length(table(out))," clusters -> impose ",ceiling(length(dat)^0.3))
      automClu <- FALSE } }   # insufficient number of clusters -> run kmeans
  if(!automClu) {
    nClu <- ceiling(length(dat)^0.4)
    kMe <- stats::kmeans(dat, centers=nClu, iter.max=9)
    out <- kMe$cluster }
  if(cluChar) {
    ctrClu <- tapply(dat, out, mean, na.rm=TRUE)
    sdClu <- tapply(dat, out, stats::sd, na.rm=TRUE)
    out <- list(cluster=out, cluChar=cbind(n=table(out), center=ctrClu, centerSd=sdClu, centerCV=abs(sdClu/ctrClu))) }
  out }


#' Refine/filter 'dat1' (1dim dataset, eg cluster) with aim of keeping center of data
#'
#' This function allows to refine/filter 'dat1' (1dim dataset, eg cluster) with aim of keeping center of data.
#' It is done based on most freq class of histogramm keep/filter data if 'core' (% of volume) are within 'core' (%) range 
#' 
#' @param dat1 simple numeric vector
#' @param core numeric vactor (betw 0 and 1) for fraction of data to keep; if null trimmedMean/max hist occurance will be used, limited within 30-70 perent; may also be 'high' or 'low' for forcing low (20-60percent) or high (75-99) percent of data to retain
#' @param keepOnly (logical) 
#' @param displPlot (logical) show plot of hist & boundaries
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns the index of values retained or if 'keepOnly' return list with 'keep' index and 'drop' index
#' @seealso \code{\link{searchLinesAtGivenSlope}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' @export
.keepCenter1d <- function(dat1, core=NULL, keepOnly=TRUE, displPlot=FALSE, silent=TRUE, debug=FALSE, callFrom=NULL) {
  ## refine/filter 'dat1' (1dim dataset, eg cluster) with aim of keeping center of data
  ## based on most freq class of histogramm keep/filter data if 'core' (% of volume) are within 'core' (%) range
  ##  or adopt to non-symmetric if most freq class is close to either end of distribution
  ## 'dat1  .. simple numeric vector
  ## 'core' .. numeric vactor (betw 0 and 1) for fraction of data to keep
  ##    if null trimmedMean/max hist occurance will be used, limited within 30-70%
  ##    may also be 'high' or 'low' for forcing low (20-60%) or high (75-99) % of data to retain
  ## 'displPlot' .. show plot of hist & boundaries
  ## not efficient if center very homogenous and very few values quite off !!
  ## return index of values retained or if 'keepOnly' return list with 'keep' index and 'drop' index
  fxNa <- .composeCallName(callFrom, newNa=".keepCenter1d")
  lim4cor <- c(0.3,0.7)
  if(identical(core, "high")) {
    lim4cor <- c(0.75,0.99)    # upper & lower limits for automatic determination of 'core' (ie % of values to keep)
    core <- NULL }
  if(identical(core, "low")) {
    lim4cor <- c(0.2,0.6)      # upper & lower limits for automatic determination of 'core' (ie % of values to keep)
    core <- NULL }
  dat1 <- signif(naOmit(dat1), 5)
  if(length(unique(dat1)) ==1)  {
    if(!silent) message(fxNa,"All (slightly rounded) values identical ! (keep all)")
    out <- 1:length(dat1)
    return(if(keepOnly) out else list(keep=out, drop=NULL))
  } else {
    his1 <- graphics::hist(dat1, breaks="FD", plot=FALSE)
    maxPo <- which(his1$counts==max(his1$counts, na.rm=TRUE))     # position of peak(s), ie mode based on hist
    maxVa <- if(length(maxPo)==1) his1$mids[maxPo] else sum(his1$mids[maxPo])/length(maxPo)
    if(length(maxPo) >1) {
      if(any(diff(maxPo)) >1) { if(!silent) message(fxNa," (multiple) non-neighbour peaks in hist")
         maxVa <- maxPo[round(stats::median(maxPo))] } }
    ## refine 'mode' in double resolution in +- 2 hist-classes range
    refWi <- 2.5*(his1$mids[2] -his1$mids[1])
    useBr <- seq(maxVa -refWi, maxVa +refWi, length.out=11)
    tab <- table(cut(dat1[dat1 > maxVa -refWi &  dat1 < maxVa +refWi], breaks=useBr))
    useP <- which(tab==max(tab))
    if(length(useP) >1) useP <- if(length(useP) %% 2 >0) stats::median(useP) else stats::median(useP[-1])
    maxVa <- ((useBr[-1] +useBr[-length(useBr)])/2)[useP]          # refined histogram 'mode'
    maxQu <- sum(dat1 <= maxVa)/length(dat1)                       # quantile of mode
    ## note: effort to find mode ('maxVa') using computeMode() from BBmisc was not at all satisfying ...
    ## note: fully tested against NAs
    ## automatic bandwidth ('core'=NULL) : based on ration max histogram class height / trimmed mean of heights
    if(is.null(core)) {
      core <- round(10*mean(his1$counts, trim=0.25)/his1$counts[which.max(his1$counts)], 3)   # sharp peak ->low ratio, small core fraction
      if(!silent) message(fxNa,"Proposed value for core ",core,"    trim mean ",
        signif(mean(his1$counts, trim=0.25), 3),"   max ",signif(his1$counts[which.max(his1$counts)], 3))
      core <- round(sort(c(lim4cor, core))[2],3) }          # keep proposed 'core' within limits
    .chSD <- function(x,y) {med <- stats::median(x,na.rm=TRUE); which(x > med -y*stats::sd(x,na.rm=TRUE) & x < med +y*stats::sd(x,na.rm=TRUE))}
    if(identical(core,"veryhigh")) {  
      core <- 0.9                         # must have some value for later use ..
      out <- .chSD(dat1, y=1)
      if(length(out) < length(dat1)*core) {out <- .chSD(dat1, y=1.5) }
      if(length(out) < length(dat1)*core) {out <- .chSD(dat1, y=2) }
      useQu <- c(sum(dat1 > min(dat1[out]) ,na.rm=TRUE), sum(dat1 > max(dat1[out]), na.rm=TRUE))/length(dat1)
      quaVa <- signif(stats::quantile(dat1, useQu), 4)
    } else {
      centrQu <- which(abs(dat1 -maxVa) ==min(abs(dat1 -maxVa)))
      useQu <- maxQu +c(-1,1)*core/2                           # boundaries of range around mode
      if(core/2 > min(maxQu,1 -maxQu)) {
        useQu <- if(useQu[1] <0) c(0,core) else c(1 -core,1)}
      quaVa <- signif(stats::quantile(dat1, useQu), 4)
      ## examine boders if assymetric (but not due to fact of touching range of data), if too assym bring most far closer
      offSe <- abs(quaVa -maxVa)
      offSe <- offSe > 1.3*sum(offSe)/length(offSe)
      if(any(offSe) && !(any(useQu %in% c(0,1)))) {
        quaVa <- .bringToCtr(quaVa, maxVa)
        useQu[which(offSe)] <- sum(dat1 >= quaVa[which(offSe)])/length(dat1)  }
      quaVa <- signif(quaVa,4)
      if(any(offSe) && !(any(useQu %in% c(0,1))) && !silent)  message(fxNa,
        "  ..new modified borders ",paste(quaVa,collapse=" "))  # result not ideal ?
      out <- which(dat1 >= quaVa[1] & dat1 <= quaVa[2])
    }
    if(length(out) > length(dat1)*core && !silent) message(fxNa,"Keep ",
      round(length(out) -length(dat1)*core)," more elements than ",round(100*core),"%")
    if(displPlot) {
      chG <- try(graphics::plot(his1, col=grDevices::grey(0.8),border=grDevices::grey(0.7)), silent=TRUE)
      if(inherits(chG, "try-error")) message(fxNa,"UNABLE to draw plot !") else {
        graphics::mtext(paste("n=",length(dat1)), cex=0.8)
        graphics::abline(v=sort(c(quaVa,maxVa)), lty=c(2,1,2), col=grDevices::grey(0.4))}}
    if(!keepOnly) out <- list(keep=out, drop=(1:length(dat1))[-out], limits=useQu)
    out }
    }
       
