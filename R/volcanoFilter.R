#' Filter Statistical Testing Results in Volcano-Plot Like Fashion
#'   
#' This function allows filtering statistical testing results combined with M-values (log2 fold-change) and other filtering criteria.
#' All suitable values can be extracted from objects created from package wrProteo (and limma) may get used automatically.
#' The outcome can be used for Volcano-plots or exporting tables.
#'
#' @details  
#' This function offers support for combined filtering along a significance-threshold (flexible choice of type of multiple testing correction) 
#' combined with M-values (log2 fold-change) and other filtering results (already present in objects from package wrProteo).
#' Even though basic filtering can be performed using separated vectors (see example below), the main advantage relies on automatically recognizing 
#' the structure of results from \code{testRobustToNAimputation()} from package \code{wrProteo}.
#'  
#' In case of using with separate vectors the original p-values may be provided without or with multiple-testing corrected values (argument \code{FdrList}. 
#' If no  multiple-testing correctio is provided it will be calculated using the Benjamini-Hochberg formula from \code{\link[stats]{p.adjust}}. 
#'   
#' Note : M-value data is assumed to be log2-transformed ratios !
#' 
#' @param Mvalue (numeric, list or of class 'MArrayLM') vector of M-values (log2 fold-change) or output from \code{testRobustToNAimputation()} from package \code{wrProteo}
#' @param pValue (numeric) statistical testing results, only used in case argument \code{Mvalue} is only a numeric vector 
#' @param useComp (integer or character) in case argument \code{Mvalue} contains output from \code{testRobustToNAimputation()} this argument allows choosing a given pair-wise comparison
#' @param filtFin (logical or matrix) optional vector to provide/include results from other additional filtering (columns should maych columns of  \code{pValue} if multiple questions addressed in data);
#'   if argument \code{Mvalue} is list or class 'MArrayLM' this argument will be only used if respective information not present from \code{Mvalue}
#' @param FCthrs (numeric) threshold for M-values (as log2(fold-change)), defaults to 1
#' @param FdrList (vector, matrix or data.frame) additional statistical testing results for multiple testing correction, used for apply \code{FdrThr};
#'   if argument \code{Mvalue} is list or class 'MArrayLM' this argument will be only used if respective information not present from \code{Mvalue}
#' @param FdrThrs (numeric) threshold for filtering \code{FdrList}, defaults to 0.05
#' @param FdrType (character) choose which multiple correction testing shoulbe used for filtering (only if argument \code{Mvalue} is list from wrProteo or class 'MArrayLM' )
#' @param signifOnly (logical) decide if only filtered data or complete table should be returned
#' @param annotColumn (character) additional elements from $annot to extract too if  \code{Mvalue} is list (from wrProteo) or class 'MArrayLM') 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @seealso \code{testRobustToNAimputation} and \code{extractTestingResults} from package \code{wrProteo}, or \code{filter_volcano} from package \code{genefilter}, \code{\link[stats]{p.adjust}}
#' @return This function returns a matrix (or in some instances a data.frame) combining (at least) the columns 'FDRvalue', 'Mvalue', 'pValue', 'filtFin' and 'retain'
#' @examples
#' set.seed(2025); means1 <- matrix(rnorm(2*95) +seq(1,3, length.out=2*95), 
#'   ncol=2, byrow=TRUE, dimnames=list(NULL, LETTERS[3:2]))
#' means1 <- rbind(means1, cbind((3:7)/2, c(6:8, -2:-3)))
#' rownames(means1) <- paste0("li_",sprintf(paste0("%03d"), 1:100))
#' Mval0 <- means1[,1] - means1[,2]
#' Mval1 <- matrix(means1[,1] - means1[,2], ncol=1, dimnames=list(rownames(means1), "C-B"))
#' set.seed(2026); pVal0 <- c(runif(95, max=0.9), (4:8)^(-4:-8))
#' FdrVal1 <- matrix(p.adjust(pVal0, method="BH"), ncol=1, dimnames=list(rownames(means1), "C-B"))
#' pVal1 <- matrix(pVal0, ncol=1, dimnames=list(rownames(means1), "C-B")) 
#' 
#' ## with separate entries ...
#' filt1 <- volcanoFilter(Mvalue=Mval1, pValue=pVal1)        # separate values
#' tail(signif(filt1, 3))
#' 
#' ## separate entries, retrieve all data for Volcano-Plot
#' filt2 <- volcanoFilter(Mvalue=Mval1, pValue=pVal1, signifOnly=FALSE)        # separate values
#' tail(signif(filt2, 3))
#' plot(filt2[,2], -1*log10(filt2[,3]), col=c("grey","red")[1+filt2[,5]], pch=16, main="Volcano-Plot")
#' 
#' ## simulate basic/minimal object from wrProteo
#' Mobj3 <- list(means=means1, BH=FdrVal1, p.value=pVal1)
#' filt3 <- volcanoFilter(Mobj3)
#' tail(signif(filt3, 3))
#' @export
volcanoFilter <- function(Mvalue, pValue=NULL, useComp=1, filtFin=NULL, FCthrs=NULL, FdrList=NULL, FdrThrs=NULL, FdrType=NULL, signifOnly=TRUE, 
  annotColumn=c("SpecType","GeneName","EntryName","Accession","Species","Contam"), silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## centralized filtering function for export & Volcano-plot 
  fxNa <- .composeCallName(callFrom, newNa="volcanoFilter")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  datOK <- length(Mvalue) !=0
  ## set defaults
  if(length(FCthrs) !=1 || !is.finite(FCthrs)) FCthrs <- 1        # ie lin scale=2
  if(length(FdrThrs) !=1  || !is.finite(FdrThrs)) FdrThrs <- 0.05
  
  ## check/compose input to merge1
      #load("C:\\Users\\wraff\\Documents\\260526-xx_dataTe2.RData") 
      #Mvalue <- dataTe; useComp=1; FCthrs=1.5; FdrList=NULL; FdrThrs=0.05; FdrType="BH"; pwSep="  /  "; filtFin=NULL
      #silent=FALSE; debug=TRUE; fxNa=".."

  if(datOK) {
    Mval <- FDRval <- pVal <- out <- outNames <- addColNa <- NULL               # initialize
    if(debug) { message(fxNa,"vFi1"); vFi1 <- list(Mvalue=Mvalue,pValue=pValue,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,FdrList=FdrList,fxNa=fxNa )}
    
    if(("MArrayLM" %in% class(Mvalue) || "list" %in% class(Mvalue)) && "means" %in% names(Mvalue)) {
        ## 1) extract setup from MArrayLM
        ## 1.1)  check useComp
        MArrayLMobj <- TRUE
        setup <- if(length(Mvalue$setup) !=0 && length(Mvalue$setup$pwGrpNa) !=0) Mvalue$setup else try(getPairwiseSetup(Mvalue, sep=NULL, silent=silent, debug=debug, callFrom=fxNa), silent=TRUE)
        if(inherits(setup, "try-error")) stop(fxNa,"Unabale to establish experimental setup")
        if(debug) { message(fxNa,"vFi1b"); vFi1b <- list(Mvalue=Mvalue,pValue=pValue,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,setup=setup,fxNa=fxNa )}

        if(length(setup$index) !=0 && length(setup$pwIndex)==0) names(setup)[which(names(setup) =="index")] <- "pwIndex"  # check/correct  name (not needed at >= wrMisc-2.1.0)
        #if(inherits()) {datOK <- FALSE}
        chUseComp <- length(useComp)==1 
        if(chUseComp) { if(is.numeric(useComp)) {        # treat as integer
            useComp <- as.integer(useComp)
            chUseComp <- useComp >0 && useComp <= nrow(setup$pwGrpNa) 
          } else {
            chUseComp <- useComp %in% rownames(setup$pwGrpNa)                # need to know/use right pwSep to allow match (risky ?)
            if(chUseComp) useComp <- which(rownames(setup$pwGrpNa) == useComp) else {             # transform text useComp to index
              warning(fxNa,"Incomplete dataset or invalid 'useComp' (abort..)"); useComp <- 0; datOK <- FALSE }
          }
          useCompNa <- setup$pwGrpNa[useComp,]                            # vector with separate names (needed for extracting from $means)
        } else { warning(fxNa,"Incomplete dataset or invalid 'useComp' (abort..)"); datOK <- FALSE }
        if(debug) {message(fxNa,"vFi2"); vFi2 <- list(Mvalue=Mvalue,pValue=pValue,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,FdrThrs=FdrThrs,FdrType=FdrType,setup=setup,datOK=datOK,fxNa=fxNa)}  #pcol=pcol,

        ## 1.2)  locate p.value
        pcol <- naOmit(match(c("p.value","pvalue","pval","p"), tolower(names(Mvalue))))
        if(length(pcol)==0) {datOK <- FALSE; warning(fxNa,"No list-element lookiing like p.values found ! (bad input ?)")} else {
          if(length(pcol) >1) { if(!silent) message(fxNa,"Confusing object, multiple columns look like p.value, using 1st (",names(Mvalue)[pcol[1]],")")
            pcol <- pcol[1]
          }
        }

        ## correct names for FDR
        if(datOK) {
          if(length(FdrType) >1) FdrType <- FdrType[which(FdrType %in% names(Mvalue))][1]
          if(length(FdrType) ==0 || is.na(FdrType)) { FdrType <- "BH"; if(!silent) message(fxNa,"Setting 'FdrType' to default 'BH'")}
          if(any("FDR" %in% FdrType, na.rm=TRUE)) FdrType <- "BH"     # assume FDR referrs to BH

          chFDR <- FdrType %in% names(Mvalue) 
          if(!chFDR) { if("FDR" %in% names(Mvalue)) names(Mvalue)[which(names(Mvalue)=="FDR")] <- "BH"    # also possibly adjust Mvalue -(needed if from wrMisc::moderTest2grp)
            chFDR <- FdrType %in% names(Mvalue) }  # update
          if(!chFDR)  {message(fxNa,"NOTE : Invalid entry: Unable to locate ",FdrType," in 'Mvalue', creating BH-FDR")
            Mvalue$BH <- apply(Mvalue[[pcol]], 2, stats::p.adjust, method="BH")
            FdrType <- "BH"}
        }       
        
        ## 1.3)  special case of single comp : different colnames => adjust
        if(datOK && ncol(Mvalue[[pcol]])==2 && colnames(Mvalue[[pcol]])[1]=="(Intercept)") {
          Mvalue[[pcol]] <- Mvalue[[pcol]][,2, drop=FALSE]
          Mvalue[[FdrType]]  <- Mvalue[[FdrType]][,2, drop=FALSE]   # FdrType already reduce to single
        }
        ## check useComp
        if(datOK) {
          if(length(useComp) ==0 || any(is.na(useComp))) {useComp <- 1
            warning(fxNa,"Argument 'useComp' invalid, setting to default useComp=1 ")}
          if(length(useComp) >1) {useComp <- useComp[1] 
            if(!silent) message(fxNa,"Argument 'useComp' must be of length=1, trimming ")}
          chFDR2 <- ncol(Mvalue[[FdrType]]) >= useComp
          if(!chFDR2) {warning(fxNa,"Argument 'useComp' designs too high number ! Setting to default useComp=1 ")
            useComp <- 1 }
        }


        if(datOK) {
          FDRval <- Mvalue[[FdrType]][,useComp] 
          ## use pValue from complex object (if available, only if not check if custom provided)      
          if(length(pcol) >0) { names(pcol) <- c("p.value","pvalue","pval","p")[pcol]             
            if(all(dim(Mvalue[[pcol]]) >= c(length(FDRval),useComp) )) pVal <- Mvalue[[pcol]][,useComp] else {
              if(length(pValue)==FDRval) pValue
            } }         
        }
        if(debug) {message(fxNa,"vFi3"); vFi3 <- list(Mvalue=Mvalue,pValue=pValue,pVal=pVal,useComp=useComp,useCompNa=useCompNa,filtFin=filtFin,FCthrs=FCthrs,FdrThrs=FdrThrs,setup=setup,datOK=datOK,fxNa=fxNa)}

        ## 1.4)  check means  & extract Mval
        chMeans <- length(Mvalue$means) !=0 && nrow(Mvalue$means) >2 && ncol(Mvalue$means) >= max(setup$pwIndex[useComp,])
        if(datOK && chMeans) Mval <- Mvalue$means[,useCompNa[1]] - Mvalue$means[,useCompNa[2]] else {datOK <- FALSE}
        if(length(Mval) != length(FDRval)) {datOK <- FALSE; warning(fxNa,"Lengths of extracted FDR-values and M-values does NOTG match (abort..)")}
        if(debug) {message(fxNa,"vFi4"); vFi4 <- list(Mvalue=Mvalue,pValue=pValue,Mval=Mval,pVal=pVal,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,datOK=datOK,fxNa=fxNa)}

        ## 1.5)  check finFilt  & extract 
        chFilt <- length(Mvalue$filter) !=0 && identical(dim(Mvalue$filter), dim(Mvalue[[FdrType]]))
        if(datOK) filtFin <- if(chFilt) Mvalue$filter[,useComp] else if(length(filtFin)==length(FDRval)) filtFin
        if(length(filtFin) != length(FDRval)) { 
          if(length(filtFin)==0 && debug) message(fxNa,"Invalid 'filtFin', setting to default") 
          filtFin <- rep(TRUE, length(FDRval)) }

        ## 2) combine for output
        if(datOK) out <- cbind(FDRvalue=FDRval, Mvalue=Mval, pValue=pVal, filtFin=filtFin, retain= FDRval <= FdrThrs & abs(Mval) >= FCthrs & filtFin )   
        if(debug) {message(fxNa,"vFi5"); vFi5 <- list(out=out,FDRval=FDRval,Mvalue=Mvalue,pValue=pValue,Mval=Mval,pVal=pVal,useComp=useComp,useCompNa=useCompNa,filtFin=filtFin,FdrThrs=FdrThrs,FCthrs=FCthrs,annotColumn=annotColumn,datOK=datOK,fxNa=fxNa)}
        
        ## what strategy (order of cols in output ??), when to add full names (eg from rownames(Mvalue$setup$pwGrpNa))

        ## 2.1) add optional annotation
        if(length(annotColumn) !=0 && length(Mvalue$annot) !=0) {         # add more columns
          chCol <- naOmit(match(annotColumn, colnames(Mvalue$annot)))  
          if(length(chCol) !=0) {
            out <- cbind(as.data.frame(out), Mvalue$annot[,chCol])
            addColNa <- colnames(Mvalue$annot)[chCol] }
        } else addColNa <- NULL   

  
        ## 2.z) special filtering for columns ('slim' ...)



    } else {
        
      ## 3) assume data as separate vectors
      MArrayLMobj <- FALSE
      stopifnot(length(Mvalue) !=0, is.numeric(Mvalue), (length(Mvalue)==length(pValue) || length(Mvalue)==length(FdrList)))

      ## check names
      fxNames <- function(x) {if(length(x) >0) {if(length(dim(x))==2) rownames(x) else names(x)} else NULL}
      names1 <- list(Mvalue=fxNames(Mvalue), pValue=fxNames(pValue), FdrList=fxNames(FdrList), filtFin=fxNames(filtFin))
      
      naLe <- sapply(names1, length)
      if(any(naLe >0)) { names1 <- names1[which(naLe >0)]
        if(length(names1) >1) {
          chDiff <- rep(NA, length(names1) -1)
          for(i in 2:length(names1)) chDiff[i-1] <- !identical(names1[[1]], names1[[i]])
          {message(fxNa,"vFi6c") ; vFi6c <- list(names1=names1,chDiff=chDiff,Mvalue=Mvalue,pValue=pValue,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,FdrList=FdrList,FdrThrs=FdrThrs,filtFin=filtFin,fxNa=fxNa )}
          if(any(chDiff)) warning(fxNa,"Check order of input : Names of '",names(names1)[1],"' don't match to names of ",pasteC(names(names1)[1+ which(chDiff)], quoteC="'"))}
          ## idea: possibly try to adjust if only different order ?  (but whom to use as ref is some have no names ?)
        outNames <- names1[[1]]  
      }
      if(debug) { message(fxNa,"vFi6"); vFi6 <- list(Mvalue=Mvalue,pValue=pValue,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,FdrList=FdrList,FdrThrs=FdrThrs,filtFin=filtFin,fxNa=fxNa )}
      
      if(length(FdrList)==length(Mvalue) && is.numeric(FdrList)) FDRvalue <- FdrList else {
        if(is.numeric(pValue)) FDRvalue <- stats::p.adjust(pValue, method="BH")
      }

      if(length(filtFin)==length(Mvalue)) { filtFin <- try(as.character(filtFin)); 
        if(inherits(filtFin, "try-error")) filtFin <- NULL }
      if(length(filtFin) !=length(Mvalue)) filtFin <- rep(TRUE, length(FDRvalue))
      
      if(debug) { message(fxNa,"vFi7"); vFi7 <- list(Mvalue=Mvalue,pValue=pValue,useComp=useComp,filtFin=filtFin,FCthrs=FCthrs,FDRvalue=FDRvalue,FdrList=FdrList,FdrThrs=FdrThrs,filtFin=filtFin,fxNa=fxNa )}

      retainDat <- FDRvalue <= FdrThrs & abs(Mvalue) >= FCthrs & filtFin 

      out <- data.frame(FDRvalue=as.numeric(FDRvalue), Mvalue=as.numeric(Mvalue), pValue=as.numeric(pValue), filtFin=as.logical(filtFin), retain=as.logical(retainDat))
      rownames(out) <- outNames 

    }  

    
    ## 4) Apply filtering & thresholds
    if(!isFALSE(signifOnly)) out <- out[which(if(is.logical(out[,"retain"])) out[,"retain"] else out[,"retain"]==1),, drop=FALSE]
    out
  }  
}
   

