#' Selecte best levels and plot linear regression model  
#'
#' The aim of this function is to select the data suiting est to a linear regression model. 
#' In real world measurements one may be confronted to the case of very low level analytes below the detection limit (LOD) and resulting read-outs fluctuate around around a common baseline (instead of \code{NA}). 
#' With such data it may be preferable to omit the read-outs for the lowest concentrations/levels of analytes if they are spread around a base-line value.
#' This function allows trying to omit all starting levels designed in \code{startLev}, then the resulting p-values for the linear regression slopes will be checked and the best p-value chosen. 
#' The input may also be a MArrayLM-type object from package \href{https://bioconductor.org/packages/release/bioc/html/limma.html}{limma} or from \code{\link{moderTestXgrp}} or \code{\link{moderTest2grp}}.
#' In the graphical representation all points assocoated to levels omitted are shown in light green.
#' For the graphical display additional information can be used : If the  \code{dat} is list or MArrayLM-type object, the list-elements $raw (according to argument \code{liNa} will be used to display points initially given as NA ad imputed lateron in grey.
#' Logarithmic (ie log-linear) data can be treated by settting argument \code{logExpect=TRUE}. Then the levels will be taken as exponent of 2 for the regression, while the original values will be displayed in the figure. 
#'
#' @param protNa (character, length=1) rowname for line to be extracted from \code{dat}
#' @param dat (matrix, list or MArrayLM-object from limma) main input of which columns should get re-ordered, may be output from \code{\link{moderTestXgrp}} or \code{\link{moderTest2grp}}.
#' @param expect (numeric of character) the expected levels; if character, constant unit-characters will be stripped away to extact the numeric content
#' @param logExpect (logical) toggle to \code{TRUE} if the main data are logarithmic
#' @param startLev (integer) specify all starting levels to test for omitting here 
#' @param liNa (character) in case \code{cat} is list or MArrayLM-type object, the list-elements with these names will be used as $raw (for indicating initial \code{NA}-values,
#'   $datImp (the main quantitation data to use) and $annot for displaying the corresponding value from the "Accession"-column.	
#' @param plotGraph (logical) display figure
#' @param silent (logical) suppress messages
#' @param yLab (character) custom y-axis label
#' @param cexLeg (character) size of text in legend
#' @param cexSub (character) size of subtitle (giving regression details of best linear model)
#' @param cexYAxis (character) size of text for y-axis labels
#' @param cexXAxis (character) size of text for x-axis labels
#' @param cexLab (character)  text in x & y axis legend (will be passed to \code{cex.lab} in \code{plot()})
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return list with $coef (coefficients), $name (as/from input \code{protNa}), $startLev the best starting level) 
#' @seealso \code{\link{moderTestXgrp}} for single comparisons, \code{\link[base]{order}}  
#' @examples
#' ## Construct data
#' li1 <- rep(c(4,3,3:6),each=3) + round(runif(18)/5,2)
#' names(li1) <- paste0(rep(letters[1:5], each=3), rep(1:3,6))
#' li2 <- rep(c(6,3:7), each=3) + round(runif(18)/5, 2)
#' dat2 <- rbind(P1=li1, P2=li2)
#' exp2 <- rep(c(11:16), each=3)
#' exp4 <- rep(c(3,10,30,100,300,1000), each=3)
#' 
#' ## Check & plot for linear model 
#' linModelSelect("P1", dat2, expect=exp2)
#' linModelSelect("P2", dat2, expect=exp2)
#'
#' ## Log-Linear data
#' dat4 <- rbind(P1=2^li1, P2=2^li2)
#' linModelSelect("P1", dat4, expect=exp2, log=TRUE)
#' 
#' linModelSelect("P1", dat4, expect=log2(exp2), log=FALSE)
#' 
#' linModelSelect("P1", dat4, expect=exp4, log=TRUE)
#' @export
linModelSelect <- function(protNa, dat, expect, logExpect=FALSE, startLev=NULL, liNa=c(raw="raw",annot="annot",datImp="datImp"), plotGraph=TRUE, 
  yLab=NULL, cexLeg=0.95, cexSub=0.85, cexYAxis=0.9, cexXAxis=0.85, cexLab=1.1, silent=FALSE, callFrom=NULL)  {
  ##  test for linear models with option to start form multiple later levels (ie omitting some of the early levels)
  ## protNa (character,length=1) name to be extracted from 'dat'
  ## dat (matrix or list) if list, the first 4 elements from 'liNa' will be used for accessing information
  ## expect ()
  ## startLev (integer) sometimes the lowest level measures are not yet in the linear range; multiple start sites for modelling linear regression may be specified to finally pick the best model
  ## liNa (character, min length=4, should have names) if 'dat' is list, this contains the names of the elements that will be used for accessing information
  ## plotGraph (logical)
  ## cexLeg (numeric) size of text in legend
  ## cexSub (numeric) size of text in sutitle/line giving regression details
  ## cexYAxis (numeric) size of text in y axis
  ## cexXAxis (numeric) size of text in x axis
  ## cexLab (numeric) size of text in x & y axis legend 
   
  fxNa <- .composeCallName(callFrom, newNa="linModelSelect")
  quantCol <- c("grey","green","blue")       # 1st for NA/imputed, 2nd for not-used, 3rd for used for regression
  pch <- 1
  if(length(protNa) >1) { message(fxNa," 'protNa' should have length of 1 (but is ",length(protNa),"), truncating ...")
    protNa <- protNa[1] }
  if(length(expect) <1) stop("Argument 'expect' seems to be empty (should be numeric for each level of dat)")
  hasAnn <- subPat <- FALSE
  if(length(startLev) >0) if(!is.numeric(startLev)) stop("Argument 'startLev' must be integer")
  if(is.list(dat)) {
    ## 'dat' is list
    if("annot" %in% names(liNa) & liNa["annot"] %in% names(dat)) if("ProteinName" %in% colnames(dat[["annot"]])) hasAnn <- TRUE
    chLi <- liNa %in% names(dat)
    if(all(!chLi)) stop("None of the list-elements given in 'liNa' were found in 'dat' !")
    if(any(!chLi)) message(fxNa," Trouble ahead : The elements ",wrMisc::pasteC(liNa[which(!chLi)], quoteC="'")," not found !!")
    if(length(grep("^[[:digit:]]$", protNa)) >0) {                     # is index
      linNo <- as.integer(protNa)
      protNa <- dat[[liNa["annot"]]][protNa,"Accession"]
    } else {
      linNo <- if(hasAnn) which(dat[[liNa["annot"]]][,"Accession"] == protNa) else which(rownames(dat[[liNa[1]]]) ==protNa)  # dat[[liNa["annot"]]]
      if(length(linNo) >1) { if(!silent) message(fxNa," name specified in argument 'liNa' not unique, using first")
      linNo <- linNo[1] } 
      }
    dat1 <- dat[[liNa["datImp"]]][linNo,]  
  } else { 
    ## simple matrix data
    linNo <- which(rownames(dat) %in% protNa)
    if(length(linNo) !=1 & !silent) message(fxNa," Note : ",length(linNo)," lines of 'dat' matched to '",protNa,"' ! (can use only 1st)")
    if(length(linNo) >1) linNo <- linNo[1]
    dat1 <- dat[linNo,]}
    
  ## which starting levels to test
  startLev <- if(length(startLev) <1) 1:floor(length(unique(expect))/2) else as.integer(startLev)
  expect0 <- expect
  if(!is.numeric(expect)) {
    subPat <- "[[:alpha:]]*[[:punct:]]*[[:alpha:]]*"
    subPat <- paste0(c("^",""), subPat,c("","$"))   
    expect <- try(as.numeric( sub(subPat[2],"",sub(subPat[1],"",as.character(expect))))) }  
  if("try-error" %in% class(expect)) stop(fxNa," Problem extacting the numeric content of 'expect': ",wrMisc::pasteC(expect0,quoteC="'"))
  if(!is.numeric(expect)) {
    expect <- as.numeric(as.factor(as.character(expect)))
    if(!silent) message(fxNa,"Note: 'expect' is not numeric, transforming to integers")
  }

  ## MAIN MODEL
  dat1 <- data.frame(conc=if(logExpect) log2(expect) else expect, quant=dat1, concL=expect)            # omics quantitation data is already log2
  lm0 <- lapply(startLev, function(x) { lmX <- stats::lm(quant ~ conc, data=dat1[which(expect >= sort(unique(expect))[x]),]); lmX })
  slopeAndP <- sapply(lm0, function(x)  z <- stats::coef(summary(x))[2,c("Estimate","Pr(>|t|)")])

  bestReg <- which.min(slopeAndP[2,])
  if(!silent) message(fxNa," best slope pVal starting at level no ",bestReg)
  ## plot
  if(plotGraph) {
    levExp <- sort(unique(expect))
    plotLi2 <- which(expect >= levExp[bestReg])
    useCol <- rep(quantCol[2], nrow(dat1))         # initialize as not-used
    useCol[plotLi2] <- quantCol[3]                 # assign used for regression 
    if(is.list(dat) & "raw" %in% names(dat)) {     
      chNa <- is.na(dat[[liNa["raw"]]][linNo,])
      if(any(chNa)) useCol[which(chNa)] <- quantCol[1]
    }
    tiPN <- if(hasAnn) dat[[liNa["annot"]]][linNo,"ProteinName"] else NA
    tit <- if(is.na(tiPN)) protNa else paste(protNa," , ",tiPN)
    if(length(yLab) <1) yLab <- "measured"
    yLab <- paste0(if(length(yLab) <1) "measured" else yLab[1], if(logExpect) " (log2)")
    
    graphics::plot(quant ~ conc, data=dat1, col=useCol, main=tit,ylab=yLab, xlab="expected concentration", las=1, cex.lab=cexLab, col.axis="white", cex.axis=0.3,tck=0)   
    graphics::mtext(paste0("best regr starting at level ",bestReg," (ie ",sort(unique(expect))[bestReg],"),  slope=",signif(slopeAndP[1,bestReg],3),
      ",  p.slo=",signif(slopeAndP[2,bestReg],2)),cex=cexSub, col=quantCol[3])
    ## use crt= for rotating text on bottom ? ... crt works only for text()
    graphics::mtext(at=(unique(dat1[,1])), signif(if(logExpect) 2^(unique(dat1[,1])) else unique(dat1[,1]),3), side=1, las=1,col=1,cex=cexXAxis)    # set las to vertical ?
    graphics::abline(lm0[[bestReg]], lty=2, col=quantCol[3])
    graphics::axis(side=2, las=1, col=1, tick=TRUE, cex.axis=cexYAxis)                                         # left axis
    legLab <- c("NA/imputed","not-used","used in regresssion")
    if(!hasAnn) {legLab <- legLab[-1]; quantCol <- quantCol[-1]}
    ptBg <- quantCol
    chPch <- pch %in% c(21:25)
    if(any(chPch)) { quantCol[which(chPch)] <- grDevices::rgb(0.2,0.2,0.2,0.4) }    
    chPa <- requireNamespace("wrGraph", quietly=TRUE)
    if(!chPa) { message(fxNa,": package 'wrGraph' not installed for searching optimal placement of legend")
      legLoc <- "bottomright"
    } else legLoc <- wrGraph::checkForLegLoc(dat1, sampleGrp=legLab, showLegend=FALSE)$loc 
    graphics::legend(legLoc, legLab, text.col=1, pch=pch, col=quantCol, pt.bg=ptBg, cex=cexLeg, xjust=0.5,yjust=0.5)        # as points
  }
  list(coef=stats::coef(summary(lm0[[bestReg]])), name=protNa, startLev=bestReg) }
  
