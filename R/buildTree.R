#' Connect edges to from tree and extract all possible branches 
#'
#' It is assumed that multiple fragments from a common ancestor bay be charcterized by the their start- and end-sites by integer values.  
#' For example, If 'abcdefg' is the ancestor, the fragments 'bcd' (from position 2 to 4) to  and 'efg' may then be assembled. 
#' To do so, all fragments must be presented as matix specifying all start- and end-sites (and fragment-names).  
#' \code{buildTree} searchs contiguous fragments from columns 'posCo' (start/end) from 'disDat' to build tree & extract path information starting with line 'startFr'.
#' Made for telling if dissociated fragments contribute to long assemblies.
#' This function uses various functions of package \href{https://CRAN.R-project.org/package=data.tree}{data.tree} which must be installed, too.
#'
#' @param disDat (matrix or data.frame) integer values with 1st column, ie start site of fragment, 2nd column as end of fragments, rownames as unique IDs (node-names)
#' @param startFr (integer) index for 1st node (typically =1 if 'disDat' sorted by "beg"), should point to a terminal node for consective growing of branches
#' @param posCo (character) colnames specifying the begin & start sites in 'disDat', if NULL 1st & 2nd col will be used
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function returns a list with $paths (branches as matrix with columns 'sumLen' & 'n'), $usedNodes (character vector of all names used to build tree) and $tree (object from data.tree)
#' @seealso package \href{https://CRAN.R-project.org/package=data.tree}{data.tree} original function used \code{\link[data.tree]{Node}}; in this package : for exploiting edge/tree related issues \code{\link{simpleFragFig}}, \code{\link{countSameStartEnd}} and \code{\link{contribToContigPerFrag}}, 
#' @examples
#' frag2 <- cbind(beg=c(2,3,7,13,13,15,7,9,7,3,7,5,7,3),end=c(6,12,8,18,20,20,19,12,12,4,12,7,12,4)) 
#' rownames(frag2) <- c("A","E","B","C","D","F","H","G","I", "J","K","L","M","N")
#' buildTree(frag2)
#' countSameStartEnd(frag2)
#' @export
buildTree <- function(disDat, startFr=NULL, posCo=c("beg","end"), silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="buildTree")
  if(!isTRUE(silent)) silent <- FALSE
  datOK <- TRUE
  if(length(disDat) <1) datOK <- FALSE
  if(length(dim(disDat)) <2) disDat <- matrix(disDat, nrow=1, dimnames=list("1",names(disDat))) # assume as single entry
  rowNa <- rownames(disDat)
  if(is.null(rowNa)) rowNa <- 1:nrow(disDat)
  chCol <- match(colnames(disDat), posCo)
  disDat <- if(sum(is.na(chCol[1:2])) <1) {
    cbind(beg=as.integer(disDat[,chCol[1]]), end=as.numeric(disDat[,chCol[2]]))
  } else disDat <- as.matrix(disDat[,1:2])
  rownames(disDat) <- rowNa
  chSlash <- grep("/", rownames(disDat))
  if(length(chSlash) >0) message(fxNa,"TROUBLE ahead, names of nodes should NOT contain '/' !!")
  disDat <- cbind(disDat[,1:2], le=disDat[,2] -disDat[,1] +1)            # add col for length # if(ncol(disDat)==2)   
  if(!requireNamespace("data.tree", quietly=TRUE)) {
    warning(fxNa,"Package 'data.tree' missing ! Please install from CRAN first .. returning NULL)")
    datOK <- FALSE
  } else {
  	## check if package is functioning  
    setX <- try(data.tree::Node$new("_Root_"))        # virtual node as generic root,  need to avoid reserved names (see NODE_RESERVED_NAMES_CONST)
    if(inherits(setX, "try-error")) { datOK <- FALSE
      warning(fxNa,"Problem running package data-tree : Can't even create a new generic node")}
  }   
  if(datOK) {  
  	## main  
    ## check for dupl
    chDup <- duplicated(paste(disDat[,1],disDat[,2],sep="_"), fromLast=FALSE)
    names(chDup) <- rownames(disDat)
    if(any(chDup)) {
      chDu2 <- duplicated(paste(disDat[,1],disDat[,2],sep="_"), fromLast=TRUE)
      hasDu <- chDu2 & !chDup                    # the originals (of dupl) to keep
      names(hasDu) <- rownames(disDat)
      ## remove duplicated/redundant
      dupDat <- lapply(which(hasDu), function(x) {z <- paste(disDat[,2],disDat[,1],sep="_"); which(z %in% z[x])})  # index of replicated elements (line-no)
      if(!silent) message(fxNa,": ", sapply(dupDat, function(x) {y <- names(chDup)[x]; paste("  ",y[1]," duplicated by ",paste(y[-1],collapse=" "),"\n  ")}))
      disDat <- disDat[which(!chDup),]           # cleaned main data
    } else dupDat <- NULL
    ## check possible start sites
    nodeWPrev <- sort(unique(names(which(rep(disDat[,1], nrow(disDat)) == rep(disDat[,2] +1, each=nrow(disDat))))))
    rootBaseNa <- if(length(nodeWPrev) >0) rownames(disDat)[which(!rownames(disDat) %in% nodeWPrev)] else rownames(disDat)
    rootBase <- which(rownames(disDat) %in% rootBaseNa)
    names(rootBase) <- rootBaseNa
    ## check startFr
    startFr <- if(is.null(startFr)) rootBase[1] else try(as.integer(startFr))
    if(inherits(startFr, "try-error")) stop(fxNa,": 'startFr' should be NULL or integer (of length 1) !")
    if(!startFr %in% rootBase) { if(!silent) message(fxNa,": choice of 'startFr' is not close to root, resetting to ",rootBase[1]," ('",names(rootBase)[1],"')")
      startFr <- rootBase[1] } 
    names(startFr) <- rownames(disDat)[startFr]
    tm1 <- disDat[,1] == disDat[startFr,2] +1              # startFr has following
    tm1 <- list(lo=tm1, it=0 +tm1, preN=rep(NA,nrow(disDat)), disDat=disDat, iter=1, start=startFr)     
    ## grow 1st branch
    if(any(tm1$lo)) tm1 <- .growTree(tm1,setX) else {
      x1 <- setX$AddChild(names(startFr), len=disDat[startFr,3])
      tm1$it[startFr] <- 1
      }
    ## look for other starting points (ie nodes not yet used)
    chSup <- rownames(disDat) %in% rootBaseNa[-which(rootBaseNa == names(startFr))]   
    names(chSup) <- rownames(disDat)
    if(any(chSup)) { 
      while(any(chSup)) {
        j <- which(chSup)[1]
        tm3 <- disDat[,1]== disDat[j,2] +1
        tm3 <- list(lo=tm3, it=0 +tm3, preN=rep(NA,nrow(disDat)), disDat=disDat, iter=1, start=j)
        tm3 <- .growTree(tm3,setX)
        chSup[j] <- FALSE }
    } else tm3 <- NULL     
    ## now extract n and cumulated length fo fragments
    setX$sumLen <- setX$len                              # initialize variable for summed length
    traversal <- data.tree::Traverse(setX, filterFun=data.tree::isNotRoot)
    data.tree::Do(traversal, function(node) node$sumLen <- node$parent$sumLen + node$len)
    setX$n <- 1                                          # initialize variable for path length
    data.tree::Do(traversal, function(node) node$n <- node$parent$n + 1)
    out <- data.tree::ToDataFrameTable(setX, "pathString","sumLen","n")
    rownames(out) <- sub("^_Root_\\/","",out[,1])
    out$n <- out$n-1  
    out <- as.matrix(out[,-1])
    if(length(dupDat) >0) {
      ## need list indicating which node(s) is/are duplicate of which node : dupDat
      replOut <- function(x,chDup,out) {   # fetch original of eliminated duplicated elements, reconstruct output with adjusted rownames
        se <- function(v) paste0("/",v,"/")
        y <- grep(se(names(chDup)[x[1]]), se(rownames(out))) 
        z <- se(rownames(out)[y])
        w <- sub("^/","",sub("/$","",sapply(names(chDup)[x[-1]], function(w) sub(se(names(chDup)[x[1]]), se(w),z))))
        matrix(rep(t(out[y,]), length(x)-1), ncol=ncol(out), dimnames=list(w,colnames(out)), byrow=TRUE) }
       replOut(dupDat[[1]], chDup, out)
      supMat <- lapply(dupDat, replOut, chDup, out)
      out2 <- matrix(unlist(sapply(supMat, as.integer)), ncol=ncol(out), byrow=FALSE)
      dimnames(out2) <- list(unlist(sapply(supMat, rownames)), colnames(out))
      out <- rbind(out, out2)  
    }
    list(paths=out, usedNodes=sort(names(chSup)[which(!chSup)]), tree=setX ) 
  } }

  #' @export
  .growTree <- function(tm, setX, addToObj=NULL) {
    ## grow tree 'setX' based on 'tm' 
    ## 'tm' .. list ($disDat .. matrix with integer start & end sites for fragments; $lo (logical) which fragments may be grown; $start (integer) index for which line of $disDat to start; $it numeric version of $lo; $preN for previous tree objects towards root; $iter for iterator (starting at 1))
    ## 'setX' .. data.tree object (main obj from root)
    ## 'addToObj' .. data.tree object (branch on which to add new branches/nodes)
    #addToObj <- if(tm$iter==1 & length(addToObj) >0) addToObj else paste0("b",tm$it[j],"_",j)
    newNodeNa <- paste0("b",0,"_",tm$disDat[tm$start,1])
    namesX <- deparse(substitute(setX))      #deparse(substitute(tm)))            # name of tree-object (typically 'setX'  )
    assign(newNodeNa, get(namesX)$AddChild(rownames(tm$disDat)[tm$start], len=tm$disDat[tm$start,3]))      # add new 1st level branch to '_Root_'
    if(any(tm$lo)) tm$preN[which(tm$lo)] <- newNodeNa
    while(any(tm$lo)) {        # need to grow further ..
      tm$iter <- tm$iter +1
      j <- which(tm$lo)[1]
      addToObj <- if(tm$iter==2) newNodeNa else tm$preN[j]    
      assign(paste0("b",tm$it[j],"_",j), get(addToObj)$AddChild(rownames(tm$disDat)[j],len=tm$disDat[j,3])) 
      tm$lo[j] <- FALSE                                          # this one is done ...
      tm0 <- tm$disDat[,1]== tm$disDat[j,2] +1                    # test for potential children
      if(any(tm0)) {z <- which(tm0);                        
        tm$lo[z] <- TRUE                                        # set to-do status for children
        tm$it[z] <- tm$it[j]+1                                  # tree-level 
        tm$preN[z] <- paste0("b",tm$it[j],"_",j)          # report (prev)name of node
        reOrd <- c(z,which(!tm0))                               # need to change order to treat children next (for treatig correctly branched trees)
        tm$lo <- tm$lo[reOrd]
        tm$it <- tm$it[reOrd]
        tm$preN <- tm$preN[reOrd]
        tm$disDat <- tm$disDat[reOrd,] }    
    }
    tm }
   
