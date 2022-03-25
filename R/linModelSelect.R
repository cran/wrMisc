#' Test multiple starting levels for linear regression model, select best and plot   
#'
#' The aim of this function is to select the data suiting set of levels of the main input data to construct a linear regression model. 
#' In real world measurements one may be confronted to the case of very low level analytes below the detection limit (LOD) and resulting read-outs fluctuate around around a common baseline (instead of \code{NA}). 
#' With such data it may be preferable to omit the read-outs for the lowest concentrations/levels of analytes if they are spread around a base-line value.
#' This function allows trying to omit all starting levels designed in \code{startLev}, then the resulting p-values for the linear regression slopes will be checked and the best p-value chosen. 
#' The input may also be a MArrayLM-type object from package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} or from \code{\link{moderTestXgrp}} or \code{\link{moderTest2grp}}.
#' In the graphical representation all points assocoated to levels omitted are shown in light green.
#' For the graphical display additional information can be used : If the  \code{dat} is list or MArrayLM-type object, 
#'  the list-elements $raw (according to argument \code{lisNa} will be used to display points initially given as NA ad imputed lateron in grey.
#' Logarithmic (ie log-linear) data can be treated by settting argument \code{logExpect=TRUE}. Then the levels will be taken as exponent of 2 for the regression, while the original values will be displayed in the figure. 
#'
#' @param rowNa (character, length=1) rowname for line to be extracted from \code{dat}
#' @param dat (matrix, list or MArrayLM-object from limma) main input of which columns should get re-ordered, may be output from \code{\link{moderTestXgrp}} or \code{\link{moderTest2grp}}.
#' @param expect (numeric of character) the expected levels; if character, constant unit-characters will be stripped away to extact the numeric content
#' @param logExpect (logical) toggle to \code{TRUE} if the main data are logarithmic but \code{expect} is linear
#' @param startLev (integer) specify all starting levels to test for omitting here (multiple start sites for modelling linear regression may be specified to finally pick the best model)
#' @param lisNa (character) in case \code{dat} is list or MArrayLM-type object, the list-elements with these names will be used as $raw (for indicating initial \code{NA}-values,
#'   $datImp (the main quantitation data to use) and $annot for displaying the corresponding value from the "Accession"-column.	
#' @param plotGraph (logical) display figure
#' @param tit (character) optional custom title
#' @param pch (integer) symbols to use n optional plot; 1st for regular values, 2nd for values not used in regression
#' @param cexLeg (numeric) size of text in legend
#' @param cexSub (numeric) text-size for line (as subtitle) giving regression details of best linear model)
#' @param xLab (character) custom x-axis label
#' @param yLab (character) custom y-axis label
#' @param cexXAxis (character) \code{cex}-type for size of text for x-axis labels
#' @param cexYAxis (character) \code{cex}-type for size of text for y-axis labels
#' @param xLabLas (integer) \code{las}-type orientation of x-axis labels (set to 2 for vertical axix-labels)
#' @param cexLab (numeric) \code{cex}-type for size of text in x & y axis labels (will be passed to \code{cex.lab} in \code{plot()})
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list with $coef (coefficients), $name (as/from input \code{rowNa}), $startLev the best starting level) 
#' @seealso \code{\link{moderTestXgrp}} for single comparisons, \code{\link[base]{order}}  
#' @examples
#' ## Construct data
#' li1 <- rep(c(4,3,3:6),each=3) + round(runif(18)/5,2)
#' names(li1) <- paste0(rep(letters[1:5], each=3), rep(1:3,6))
#' li2 <- rep(c(6,3:7), each=3) + round(runif(18)/5, 2)
#' dat2 <- rbind(P1=li1, P2=li2)
#' exp2 <- rep(c(11:16), each=3)
#' 
#' ## Check & plot for linear model 
#' linModelSelect("P2", dat2, expect=exp2)
#'
#' ## Log-Linear data
#' ## Suppose dat2 is result of measures in log2, but exp4 is not
#' exp4 <- rep(c(3,10,30,100,300,1000), each=3)
#' linModelSelect("P2", dat2, expect=exp4, logE=FALSE)    # bad
#' linModelSelect("P2", dat2, expect=exp4, logE=TRUE)
#' 
#' @export
linModelSelect <- function(rowNa, dat, expect, logExpect=FALSE, startLev=NULL, lisNa=c(raw="raw",annot="annot",datImp="datImp"), plotGraph=TRUE, 
  tit=NULL, pch=c(1,3), cexLeg=0.95, cexSub=0.85, xLab=NULL, yLab=NULL, cexXAxis=0.85, cexYAxis=0.9, xLabLas=1, cexLab=1.1, silent=FALSE, callFrom=NULL)  {
  ##  test for linear models with option to start form multiple later levels (ie omitting some of the early levels)
  fxNa <- .composeCallName(callFrom, newNa="linModelSelect")
  argNa <- c(deparse(substitute(rowNa)), deparse(substitute(dat)), deparse(substitute(expect)))
  quantCol <- c("blue","tan2","grey")       # figure: 1st for 'used in regression', 2nd for 'not-used', 3rd for 'NA/imputed'
  legLab <- c("used in regresssion","not used","NA/imputed,used","NA/imputed,not used")  # for figure legend ..
  annoColNa <- c("Accession","GeneName")
  doSelect <- TRUE
  if(length(rowNa) <1) {warning(fxNa," argument 'rowNa=",argNa[1],"' contains emty object, nothing to do !"); doSelect <- FALSE}
  if(length(dat) <1) {warning(fxNa," argument 'dat=",argNa[1],"' contains emty object, nothing to do !"); doSelect <- FALSE}
  if(length(expect) <1) {warning(fxNa," argument 'expect=",argNa[3],"' contains emty object, nothing to do !"); doSelect <- FALSE}
  if(doSelect) {  
    if(length(rowNa) >1) { message(fxNa," 'rowNa' should have length of 1 (but is ",length(rowNa),"), truncating ...")
      rowNa <- rowNa[1] }
    if(length(expect) <1) stop("Argument 'expect' seems to be empty (should be numeric for each level of dat)")
    hasAnn <- subPat <- FALSE
    if(length(startLev) >0) if(!is.numeric(startLev)) stop("Argument 'startLev' must be integer")
    if(is.list(dat)) {
      ## 'dat' is list
      if(lisNa[2] %in% names(lisNa) & lisNa[2] %in% names(dat)) if(annoColNa[2] %in% colnames(dat[[lisNa[2]]])) hasAnn <- TRUE
      chLi <- lisNa %in% names(dat)
      if(all(!chLi)) stop("None of the list-elements given in 'lisNa' were found in 'dat' !")
      if(any(!chLi)) message(fxNa," Trouble ahead : The elements ",pasteC(lisNa[which(!chLi)], quoteC="'")," not found !!")
      if(length(grep("^[[:digit:]]$", rowNa)) >0) {                     # is index
        linNo <- as.integer(rowNa)
        rowNa <- dat[[lisNa[2]]][rowNa,annoColNa[1]]
      } else {
        linNo <- if(hasAnn) which(dat[[lisNa[2]]][,annoColNa[1]] == rowNa) else which(rownames(dat[[lisNa[1]]]) ==rowNa)  
        if(length(linNo) >1) { if(!silent) message(fxNa," name specified in argument 'lisNa' not unique, using first")
          linNo <- linNo[1] } 
        }
      dat1 <- dat[[lisNa[3]]][linNo,]                       # get imputed data
    } else { 
      ## simple matrix data
      linNo <- which(rownames(dat) %in% rowNa)
      if(length(linNo) !=1 & !silent) message(fxNa," Note : ",length(linNo)," lines of 'dat' matched to '",rowNa,"' ! (can use only 1st)")
      if(length(linNo) >1) linNo <- linNo[1]
      dat1 <- dat[linNo,]}
      
    ## which starting levels to test
    startLev <- if(length(startLev) <1) 1:floor(length(unique(expect))/2) else as.integer(startLev)
    expect0 <- expect
    if(!is.numeric(expect)) {
      subPat <- "[[:alpha:]]*[[:punct:]]*[[:alpha:]]*"
      subPat <- paste0(c("^",""),subPat,c("","$"))   
      expect <- try(as.numeric( sub(subPat[2], "", sub(subPat[1], "", as.character(expect))))) } 
    if(inherits(expect, "try-error")) stop(fxNa," Problem extracting the numeric content of 'expect': ",pasteC(expect0,quoteC="'"))  
    if(!is.numeric(expect)) {
      expect <- as.numeric(as.factor(as.character(expect)))
      if(!silent) message(fxNa,"Note: 'expect' is not numeric, transforming to integers")
    }
  
    ## MAIN MODEL
    dat1 <- data.frame(conc=if(logExpect) log2(expect) else expect, quant=dat1, concL=expect)            # omics quantitation data is already log2
    lm0 <- lapply(startLev, function(x) { lmX <- try(stats::lm(quant ~ conc, data=dat1[which(expect >= sort(unique(expect))[x]),])); lmX })
    slopeAndP <- sapply(lm0, function(x)  z <- stats::coef(summary(x))[2,c("Estimate","Pr(>|t|)")])
    bestReg <- which.min(slopeAndP[2,])
    if(!silent) message(fxNa," best slope pVal starting at level no ",bestReg)
    
    ## PLOT
    if(plotGraph) {
      levExp <- sort(unique(expect))
      usedP <- expect >= levExp[bestReg] 
      useCol <- rep(quantCol[1], nrow(dat1))                    # initialize as used
      pch1 <- rep(pch[1], nrow(dat1))                           # initialize as used
      if(any(!usedP)) { useCol[which(!usedP)] <- quantCol[2]    # color used for not-used  
        pch1[which(!usedP)] <- pch[2] }                         # symbol used for not-used  
      
      chNa <- is.list(dat) & lisNa[1] %in% names(dat)
      if(any(chNa)) chNa <- is.na(dat[[lisNa[1]]][linNo,])
      hasNa <- any(chNa)
      legCol <- quantCol[if(hasNa) c(1:3,3) else 1:2]
      legPch <- pch[if(hasNa) c(1,2,1,2) else 1:2]
      if(hasNa) {useCol[which(chNa)] <- quantCol[3]} else {legLab <- legLab[1:2]}
      if(length(tit) <1) {tit <- if(hasAnn) dat[[lisNa[2]]][linNo, annoColNa[2]] else paste0(rowNa," (from ",argNa[2],")")}
      if(length(yLab) <1) yLab <- "measured"
      if(length(xLab) <1) xLab <- "expected"
      if(logExpect) xLab <- paste0(xLab," (log-scale)")
      ## main plot
      graphics::plot(quant ~ conc, data=dat1, col=useCol, main=tit,ylab=yLab, xlab=xLab, las=1, pch=pch1, cex.lab=cexLab, col.axis="white", cex.axis=0.3,tck=0)   
      graphics::mtext(paste0("best regr starting at level ",bestReg," (ie ",sort(unique(expect))[bestReg],"),  slope=",signif(slopeAndP[1,bestReg],3),
        ",  p=",signif(slopeAndP[2,bestReg],2)), cex=cexSub, col=quantCol[1])
      ## use crt= for rotating text on bottom ? ... crt works only for text()
      graphics::mtext(at=(unique(dat1[,1])), signif(if(logExpect) 2^(unique(dat1[,1])) else unique(dat1[,1]),3), side=1, las=xLabLas,col=1,cex=cexXAxis)    # set las to vertical ?
      graphics::abline(lm0[[bestReg]], lty=2, col=quantCol[1])
      graphics::axis(side=2, las=1, col=1, tick=TRUE, cex.axis=cexYAxis)                                         # left axis
      ptBg <- quantCol
      chPch <- pch %in% c(21:25)
      if(any(chPch)) { quantCol[which(chPch)] <- grDevices::rgb(0.2,0.2,0.2,0.4) }    
      chPa <- requireNamespace("wrGraph", quietly=TRUE)
      if(!chPa) { message(fxNa,": package 'wrGraph' not installed for searching optimal placement of legend")
        legLoc <- "bottomright"
      } else legLoc <- try(wrGraph::checkForLegLoc(dat1, sampleGrp=legLab, showLegend=FALSE)$loc, silent=TRUE)
      if(inherits(legLoc, "try-error")) { legLoc <- "bottomright"
         message(fxNa,"Did not succeed in determining optimal legend location")}
      tmp <- try(graphics::legend(legLoc, legLab, pch=legPch, col=legCol, text.col=legCol, pt.bg=ptBg, cex=cexLeg, xjust=0.5,yjust=0.5), silent=TRUE)        # as points
      if("try-error" %in% class(tmp)) message(fxNa,"NOTE : Unable to add legend !  ",as.character(tmp))
    }
    list(coef=stats::coef(summary(lm0[[bestReg]])), name=rowNa, startLev=bestReg) }}
    
