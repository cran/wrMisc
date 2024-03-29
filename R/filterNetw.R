#' Filter nodes & edges for extracting networks
#'  
#' This function allows extracting and filtering network-data based on fixed threshold (\code{limInt}) and add sandwich-nodes (nodes inter-connecting initial nodes) out of node-based queries.
#' 
#' 
#' @param lst (list, composed of multiple matrix or data.frames ) main input (each list-element should have same number of columns)
#' @param filtCol (integer, length=1) which column of \code{lst} should be usd to filter using thresholds \code{limInt} and \code{sandwLim}
#' @param limInt (numeric, length=1) filter main edge-scores according to \code{filterAsInf}
#' @param sandwLim (numeric, length=1) filter sandwich connection edge-scores accodring to \code{filterAsInf} 
#' @param filterAsInf (logical) filter as 'inferior or equal' or 'superior or equal'
#' @param outFormat (character) may be 'matrix' for tabular output, 'all' as list with matrix and list of node-names
#' @param remOrphans (logical) remove networks consisting only of 2 connected edges
#' @param remRevPairs (logical) remove duplicate edges due to reverse massping (eg A - B and B - A); NOTE : use only when edges don't have orientation !   
#' @param elemNa (character) used only for messages
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a matrix or data.frame
#' @seealso  in \code{\link[base]{cbind}}
#' @examples
#' 
#' lst2 <- list('121'=data.frame(ID=as.character(c(141,221,228,229,449)),11:15), 
#' 	 '131'=data.frame(ID=as.character(c(228,331,332,333,339)),11:15), 
#'   '141'=data.frame(ID=as.character(c(121,151,229,339,441,442,449)),c(11:17)), 
#'   '151'=data.frame(ID=as.character(c(449,141,551,552)),11:14),
#'   '161'=data.frame(ID=as.character(171),11), '171'=data.frame(ID=as.character(161),11),
#'   '181'=data.frame(ID=as.character(881:882),11:12) )
#'
#' lst2 <- list('121'=data.frame(ID=as.character(c(141,221,228,229,449)),11:15, 21:25), 
#' 	 '131'=data.frame(ID=as.character(c(228,331,332,333,339)),11:15, 21:25), 
#'   '141'=data.frame(ID=as.character(c(121,151,229,339,441,442,449)), c(11:17), 21:27), 
#'   '151'=data.frame(ID=as.character(c(449,141,551,552)), 11:14, 21:24),
#'   '161'=data.frame(ID=as.character(171), 11,21), '171'=data.frame(ID=as.character(161), 11,21),
#'   '181'=data.frame(ID=as.character(881:882), 11:12,21:22) )
#' 
#' (te1 <- filterNetw(lst2, limInt=90, remOrphans=FALSE))
#' (te2 <- filterNetw(lst2, limInt=90, remOrphans=TRUE))
#' 
#' (te3 <- filterNetw(lst2, limInt=13, remOrphans=FALSE))
#' (te4 <- filterNetw(lst2, limInt=13, remOrphans=TRUE))
#' 
#' 
#' @export
filterNetw <- function(lst, filtCol=3, limInt=5000, sandwLim=5000, filterAsInf=TRUE, outFormat="matrix", remOrphans=TRUE, remRevPairs=TRUE, elemNa="genes", silent=FALSE, callFrom=NULL, debug=FALSE) {
  ##
  fxNa <- .composeCallName(callFrom, newNa="filterNetw")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(any(sapply(c("mat","matr","matrix","all"), identical, outFormat))) {asMatrix <- TRUE} else {asMatrix <- FALSE}
  if(length(names(lst)) <1) stop("Invalid format of 'lst' : has NO names")

  ## check input list for duplicate queries
  lstNa <- names(lst)
  chDu <- duplicated(lstNa, fromLast=FALSE)
  if(any(chDu)) { lst <- lst[-1*which(chDu)]
    if(!silent) message(fxNa,"Entry has ",sum(chDu)," duplicate entry-names, removing duplicates")
    lstNa <- names(lst) }                       # update
  useID <- list(query=lstNa)
  if(debug) {message(fxNa,"fiNe1 passed ini check")}
  ## check format
  ch2dim <- sapply(lapply(lst, dim), length) ==2
  ## remove any non-matrix or data.frame
  if(any(!ch2dim)) { 
    if(all(!ch2dim)) {lst <- list(); chDim <- NULL; warning(fxNa,"Invalid input, should be list of matrixes or data.frames !!")
    } else { lst <- lst(which(ch2dim))
      chDim <- lapply(lst, dim)            # update
      if(!silent) message(fxNa,"Note: ",sum(!ch2dim)," out of ",length(ch2dim)," list-elements are not matrix or data.frame, removing")
   } } else chDim <- lapply(lst, dim)
  colNa <- colnames(lst[[1]])    
  if(debug) {message(fxNa,"fiNe2,  passed remove any non-matrix or data.frame")}
  
  ## check if add'l column for quality/intensity data available
  chQ <- sapply(lst, function(x) ncol(x) >1)
  ## check filtCol argument  (assume format of lst as checked)
  msg <- "Invalid entry for 'filtCol': should be integer (of length=1) to designate which column to use or column-name"
  if(length(filtCol) <1) {if(ncol(lst[[1]]) >1) {filtCol <- 2; message(fxNa,msg,"; setting to 2")} else stop(msg," -too short")}
  if(length(filtCol) >1) {filtCol <- filtCol[1]; message(fxNa,msg," -use 1st")}
  if(!is.numeric(filtCol)) {filtCol <- which(colnames(lst[[1]])==filtCol)
     if(length(filtCol) <1) {filtCol <- 2; message(fxNa, msg,"; setting to 2")}   
  } else { if(filtCol > ncol(lst[[1]]) | filtCol <2) {
    filtCol <- 2; message(fxNa, msg,"; setting to 2")} }

  ## check input format if filtering can be applied (reset 'limInt' to NULL if can't be used) 
  if(length(limInt)==1 && is.numeric(limInt) && !any(is.na(limInt))) {           # valid threshod for filtering
    chDimQ <- all(sapply(chDim, function(x) x[2]) >1) 
    if(!chDimQ) { limInt <- sandwLim <- NULL
      if(!silent) message(fxNa,"Data have no second column with numeric data for filtering, ignoring 'limInt' and 'sandwLim'")     
  } } else limInt <- NULL 
  if(debug) message(fxNa,"fiNe3,  check input format if filtering can be applied")
  ## check if reverse-(direct)mapping has same score - only if .filterNetw is not launched (.filterNetw does this check, too)
  if(remRevPairs && !asMatrix && all(chQ)) {    # run only if no conversion to matrix, otherwise do this check after lrbind()
    sepCha <- "__"
    chRe <- cbind(pri=rep(names(lst), sapply(lst,nrow)), sec=lrbind(lst) )
    chR2 <- paste(chRe[,2], chRe[,1], sep=sepCha) %in% paste(chRe[,1], chRe[,2], sep=sepCha) 
    if(any(chR2)) { if(!all(chR2)) chRe <- chRe[which(chR2),]     # data with any reverse mapping only
      tmp <- apply(as.matrix(chRe[,1:2]), 1, function(x) paste(sort(x), sep=sepCha))
      tmp <- paste(tmp[1,], tmp[2,], sep=sepCha)
      if(debug) {message(fxNa,"fiNe4,  opional check if reverse-(direct)mapping has same score")}
      tmp2 <- by(as.matrix(chRe), tmp, function(x) length(unique(x[,3])) ==1)
      if(any(!tmp2)) warning(fxNa,"Reverse value for ",sum(!tmp2)," pairs incoherent !! (",
        pasteC(sub(sepCha,"& ", names(tmp2)[utils::head(which(!tmp2))])),")")
  } }
  ## filter all for thresh limInt, subseq filter sandwLim, establish list of all connected nodes
  if(all(chQ) && length(limInt)==1 && is.numeric(limInt) && !any(is.na(limInt))) {           # valid threshod for filtering
    lst <- lapply(lst, function(x) x[which(if(!isFALSE(filterAsInf)) x[,filtCol] <= limInt else x[,filtCol] >= limInt),])
    chDi <- sapply(lst, function(x) length(dim(x)) <2)
    if(any(chDi)) lst[which(chDi)] <- lapply(lst[which(chDi)], function(x) matrix(x, nrow=1, dimnames=list(NULL,colNa)))  # reset to matrix-type if lost at filtering  
    chN <- sapply(lst, length)
    if(all(chN <1)) warning(fxNa,"NOTHING remaining after filtering for 'limInt'=",limInt," !!")
    if(debug) {message(fxNa,"fiNe5,  filter all for thresh limInt")}
    ##  now check for possible sandwLim filter
    if(any(sapply(c("def","default","auto"), identical, sandwLim))) sandwLim <- limInt
    if(length(sandwLim)!=1 | !is.numeric(sandwLim) | any(is.na(sandwLim))) sandwLim <- NULL    
    if(debug) { message(fxNa,"fiNe6,  check for possible sandwLim filter")}
  } else sandwLim <- NULL
  ## extract 2nd nodes, apply sandwLim (if applicable) 
  lst <- lapply(lst, function(x) if(length(sandwLim) <1 || ncol(x) <2) x[which(x[,1] %in% useID$query),] else x[which(x[,1] %in% useID$query | 
      if(!isFALSE(filterAsInf)) x[,filtCol] <= sandwLim else x[,filtCol] >= sandwLim ),])
  chDi <- sapply(lst, function(x) length(x) >0 && length(dim(x)) <2)    # in case of list of matrixes, some may become simple vectors due to indexing
  if(any(chDi)) lst[which(chDi)] <- lapply(lst[which(chDi)], function(x) matrix(x, nrow=1, dimnames=list(NULL,colNa)))  # reset to matrix-type if lost at filtering  
      
  all2nd <- unlist(lapply(lst, function(x) x[,1]), use.names=FALSE)  # extr 2nd node IDs (after add'l filtering)       
  if(debug) {message(fxNa,"fiNe7,  extracting 2nd nodes, apply sandwLim")}
  ## remove empty (due to filtering)
  chLe <- sapply(lst, length) >0
  if(any(!chLe)) { lst <- lst[which(chLe)]
    if(all(!chLe)) { if(!silent) message(fxNa,"NOTHING remaining after filtering !!")
    } else if(!silent) message(fxNa,"Removed ",sum(!chLe)," out of ",length(chLe)," queries without data passing filtering")
    lstNa <- names(lst)                     # update
    useID <- list(query=lstNa)              # update    
  }  

  ## look for Sandwich nodes/genes  
  useID$sandwID <- NULL
  ## a sandwich node/gene has too aoocur at min 2x in query-results (and may not be part of init query)
  chDup <- duplicated(all2nd, fromLast=FALSE) & !(all2nd %in% lstNa)    # needed to be defined as sandwich
  useID$sandwID <- if(any(chDup)) unique(all2nd[which(chDup)]) else NULL  
  if(debug) {message(fxNa,"fiNe8,  sandwich nodes/genes")}; fiNe8 <- list(lst=lst,all2nd=all2nd,useID=useID,chDup=chDup,chLe=chLe,filtCol=filtCol, limInt=limInt,chQ=chQ)

  ## filter to keep connected network only (and reduce all to IDs & filtCol)
  useID2 <- if(length(limInt) ==1) unique(unlist(useID)) else useID$query
  lst <- lapply(lst, function(x) {y <- which(x[,1] %in% useID2); if(length(y) >0) {if(length(y) >1) x[y,c(1,filtCol)] else { 
    z <- data.frame(x[y,1], x[y,filtCol]); colnames(z) <- colnames(x)[c(1,filtCol)]; z} }})  
  chLe <- sapply(lst, length)
  if(any(chLe <1)) { lst <- lst[which(chLe >0)]
    if(!silent) message(fxNa,"",sum(chLe <1)," element(s) had no data remaining after filtering ..." )  
  }
  if(debug) {message(fxNa,"fiNe9,  filter to keep connected network only")}

  if(length(lst) <1) message(fxNa,"NOTE: NOTHING remaining after filtering for connected networks ") else {
    if(!isFALSE(asMatrix)) {
      ## convert to matrix, remove duplicates
      lst <- .filterNetw(lst, remOrphans=remOrphans, reverseCheck=remRevPairs, filtCol=filtCol, callFrom=fxNa, silent=silent, debug=debug)
      isSandw <- lst[,2] %in% useID$sandwID
      lst <- cbind(lst, toSandw=if(is.data.frame(lst)) isSandw else as.numeric(isSandw))
      if(!isFALSE(remRevPairs)) {
        te1x <- apply(as.matrix(lst[,c(1,filtCol)]), 1, sort)      ## need to sort le/ri !!
        lst <- lst[which(!duplicated(paste(te1x[1,], te1x[2,]), fromLast=FALSE)),]      # corrected for duplicate pairs
      }
    } else {
      ## prepare output as list
      lst <- lapply(lst, function(x) {ch1 <- x[,1] %in% unlist(useID)
        if(sum(ch1) >0) { if(sum(ch1) >0) x[which(ch1),] else matrix(x[which(ch1),], nrow=1, dimnames=list(NULL, colnames(x)))} else NULL })
      if(!silent) message(fxNa,"Network of ",sum(useID$query %in% lstNa)," (init) ",elemNa," plus ",
        sum(useID$sandwID %in% lstNa)," sandw-",elemNa )              #  
    }
  }
  if(any(sapply(c("all"), identical, outFormat))) lst <- list(useID=useID, netw=lst) 
  lst }

 
#' Filter nodes & edges for extracting networks (main)
#'  
#' This function allows extracting and filtering network-data based on fixed threshold (\code{limInt}) and add sandwich-nodes (nodes inter-connecting initial nodes) out of node-based queries.
#' 
#' 
#' @param lst (list, composed of multiple matrix or data.frames ) main input (each list-element should have same number of columns)
#' @param remOrphans (logical) remove networks consisting only of 2 connected edges
#' @param reverseCheck (logical) 
#' @param filtCol (integer, length=1) which column of \code{lst} should be usd to filter using thresholds \code{limInt} and \code{sandwLim}
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a matrix or data.frame
#' @seealso \code{\link{filterNetw}} and other CRAN package dedeicated to networks
#' @examples
#' ab <- 1:10  
#' @export  
.filterNetw <- function(lst, remOrphans=TRUE, reverseCheck=TRUE, filtCol=2, callFrom=NULL, silent=FALSE, debug=FALSE) {
  ##
  fxNa <- .composeCallName(callFrom, newNa=".filterNetw")
  sepCha <- "__"           # characters used in paste when fusing 2 nodes
  lstLe <- sapply(lst, nrow)
  names(lstLe) <- names(lst)
  ## transform list to matrix/data.frame
  lst <- lrbind(lst, silent=TRUE)
  if(any(dim(lst) <1) && length(dim(lst)) <2) stop("Invalid input format ! Bizzare, lrbind() did NOT manage to produce a matrix or data.frame")
  colnames(lst)[1] <- "Node2"
  if(ncol(lst) >1) colnames(lst)[2] <- "edgeScore" 
  
  lst <- data.frame(Node1=rep(names(lstLe), lstLe), lst)
  if(debug) {message(fxNa,".fiNe.1 transform list to matrix/data.frame")}
  ## check for (reverse-)pairs
  chR2 <- paste(lst[,filtCol], lst[,1], sep=sepCha) %in% paste(lst[,1], lst[,filtCol], sep=sepCha) 
  if(any(chR2)) { 
    chRe <- if(!all(chR2)) lst[which(chR2),] else lst      # data with any reverse mapping only
    tmp <- apply(as.matrix(chRe[,1:2]), 1, function(x) paste(sort(x), sep=sepCha))     #  pairs of sorted nodes
    tmp <- paste(tmp[1,], tmp[2,], sep=sepCha)
    names(tmp) <- rownames(chRe)
    ## check for consistent score with reverse pairs
    if(reverseCheck && ncol(lst) >2) {
      tmp2 <- by(as.matrix(chRe), tmp, function(x) length(unique(x[,3])) ==1)
      if(any(!tmp2)) warning(fxNa,"Reverse value for ",sum(!tmp2)," pairs incoherent !! (",
        pasteC(sub(sepCha,"& ", names(tmp2)[utils::head(which(!tmp2))])),")")
    }
    if(debug) {message(fxNa,".fiNe.2 check for consistent score with reverse pairs")}
    ## remove duplicates (ie remove 2nd occurance due to reverse pairs)
    dupP <- duplicated(tmp, fromLast=FALSE)  
    if(any(dupP)) {lst <- lst[-1*as.integer(names(tmp)[which(dupP)]),]
      if(!silent) message(fxNa,"Removing ",sum(dupP)," (reverse) redundant mappings")}
  }
  if(debug) {message(fxNa,".fiNe.3 remove duplicates")} 
  ## 
  lstN <- c(as.character(lst[,1]), as.character(lst[,2]))
  if(remOrphans) {
    ## need number of connections to decide on low-connected
    chNo <- duplicated(lstN, fromLast=TRUE)
    if(any(chNo)) { 
      chNo2 <- chNo | duplicated(lstN, fromLast=FALSE)
      if(debug) {message(fxNa,".fiNe.4 number of connections to decide on low-connected")}
      if(!all(chNo2)) { hiNo <- unique(lstN[which(chNo2)] )
        ## which pairs of connections to keep (any node connected to a hiNo ?)
        chLi <- rowSums(matrix(lstN %in% hiNo, ncol=2)) >0
        if(all(!chLi) & !silent) warning(fxNa,"NONE of the nodes has any further connections, argument 'remOrphans' removes everything")
        lst <- lst[which(chLi),]
        if(debug) {message(fxNa,".fiNe.5 which pairs of connections to keep")}
        } }
  }      
  ##
  rownames(lst) <- 1:nrow(lst)
  lstN <- unique(c(as.character(lst[,1]), as.character(lst[,2])))      
  lst
} 
     
