#' Combine Vectors From List And Return Basic Count Statistics
#'
#' The aim of this function is to choose a fixed number (\code{nCombin}) of list-elments from \code{lst} and count the number of common values/words.
#' Furthermore, one can define levels to fine-tune the types of combinations to examine.
#' In case multiple combinations for a given level are possible, some basic summary statistics are provided, too.
#'
#' @details
#' Note of caution :
#' With very long lists and/or high numbers of repeats of given levels, however, the computational effort incerases very much (like it does when using \code{table}).
#' Thus, when exploring all different combinations of large data-sets may easily result in queries consuming many ressources (RAM and processing time) !
#' It is recommended to start testing with test smaller sub-groups.
#'
#' The main idea of this function is to count frequency of terms when combining different drawings.
#' For example, you ask students from different cities which are their preferred hobbies, they may have different preference depending on the city ( defined by \code{lev}).
#' Now, if you want to make groups of 3 students, possibly with one from each city (A ,B and C), you want to count (/estimate) the frequency of different combinations possible.
#' Thus, using this function all combinations of the students from city A with the students from city B and C will be made when counting the number of common hobbies (by \code{nCombin} students).
#' Then, all counting results will be summarized to the average count for the various categories (which hobbies were seen once, twice or 3 times...),
#' sem (standard error of the mean) and CI (95% confidence interval), as well as sd.
#'
#' Of course, the number of potential combinations may quickly get very large. Using the argument \code{remDouble=TRUE} you can limit the search to
#' either finding all students giving the same answer plus all student giving different answers.
#' In this case, when a given level appears multiple times, all possible combinations using one of the respective entries will be be made with the other levels.
#'
#'
#' @param lst (list of character or integer vectors) main input
#' @param lev (character) define groups of \code{lst}
#' @param nCombin (integer) number of list-elements to combine from \code{lst}
#' @param remDouble (logical) remove intra-duplicates (defaults to \code{TRUE})
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns an array with 3 dimensions : i)  ii) the combinations of \code{nCombin} list-elements,
#'   iii) the number of counts (n), sem (standard error of the mean), CI (confidence interval) and sd
#' @seealso  \code{\link[base]{table}},  \code{\link[wrMisc]{replicateStructure}}
#' @examples
#' ## all list-elements are considered equal
#' tm1 <- list(a1=LETTERS[1:17], a2=LETTERS[3:19], a3=LETTERS[6:20], a4=LETTERS[8:22])
#' combineAsN(tm1, lev=gl(1,4))[,1,]
#'
#' ## different levels/groups in list-elements
#' tm4 <- list(a1=LETTERS[1:15], a2=LETTERS[3:16], a3=LETTERS[6:17], a4=LETTERS[8:19],
#'   b1=LETTERS[5:19], b2=LETTERS[7:20], b3=LETTERS[11:24], b4=LETTERS[13:25], c1=LETTERS[17:26],
#'   d1=LETTERS[4:12], d2=LETTERS[5:11], d3=LETTERS[6:12], e1=LETTERS[7:10])
#' te4 <- combineAsN(tm4, nCombin=4, lev=substr(names(tm4),1,1))
#' str(te4)
#' te4[,,1]
#'
#' @export
combineAsN <- function(lst, lev=NULL, nCombin=3, remDouble=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## count combinatorics
  ## 'remDouble' .. idea to remove mixed auto&hetero-combinations (but leave auto.only and hetero.only combinations)
  ## combine 'nCombin' combinations of the (same level of) vectors in list 'lst' and search how may instances of the elements are found/counted on average, as total ('any'), single, double, triple or min2
  ## Example : There are 4 independent drawings of a variable number of letters from the alphabet.
  ## Note double-repeats may be quite frequent and are eliminated by default, triple- (and higher) repeats are kept.
  ## if multiple ways of making sets of 'nCombin' vetors exist, summary statistics will be added in 3rd dimension of resulting array
  fxNa <- .composeCallName(callFrom, newNa="combineAsN")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  reqPa <- c("utils","wrMisc")
  chPa <- sapply(reqPa, requireNamespace, quietly=TRUE)
  if(any(!chPa)) stop("package(s) '",paste(reqPa[which(!chPa)], collapse="','"),"' not found ! Please install first from CRAN")
  ## sanity checks
  if(length(lev) <1) { if(length(names(lst)) >0) { lev <- substr(names(lst),1,1)
      if(!silent) message(fxNa,"Note : Argument 'lev' is missing; trying to recuperate as 1st character of names of 'lst' :  ",pasteC(utils::head(lev,7),quoteC="'"), if(length(lev) >7) "...")
    } else stop("Argument 'lev' (for levels of list-elements) MUST be provided when 'lst' has no names !")}

  if(length(lst) != length(lev)) stop("Length of 'lst' and 'lev' must match !   Was ",length(lst)," and ",length(lev)," !")
  colNaSep <- c("_","-","\\.","\\+","__","--","\\+\\+")        # separators to test/for choice when combining
  inclMono <- TRUE            # complementary to remDouble (realy useful ?)

  if(length(nCombin) <1) { nCombin <- length(levels(as.factor(lev)))
    if(debug) message(fxNa,"Setting nCombin to ",nCombin)
  } else if(length(nCombin) >1) { nCombin <- nCombin[1]
    if(!silent) message(fxNa,"'nCombin' should be of length=1 (using only 1st)")}
  if(any(is.na(nCombin))) { nCombin <- length(levels(as.factor(lev)))
    if(!silent) message(fxNa,"'nCombin' has NA, resetting to default")}

  if(any(sapply(c("all","def"), identical, nCombin))) nCombin <- length(levels(as.factor(lev)))
  if(length(lst) <2 || nCombin <2) { datOK <- FALSE; if(!silent) message(fxNa,"Nothing to do")
  } else datOK <- TRUE

  if(datOK & nCombin ==2 & remDouble) { remDouble <- FALSE
    if(debug) message(fxNa,"Setting 'nCombin=2' AND 'remDouble=TRUE' makes no sense (nothing will match), running as 'remDouble=FALSE' ")}

  if(datOK) {
    ## check levels
    levNo <- try(suppressWarnings(as.numeric(unique(lev))), silent=TRUE)          # level-indexes
    if(inherits(levNo, "try-error") || any(is.na(levNo))) levNo <- unique(suppressWarnings(as.numeric(as.factor(lev))))
    if(debug) message(fxNa," levNo (",class(levNo),")  = ",pasteC(levNo))
    if(nCombin > length(lev) -1) { nCombin <- length(lev) -1
      if(!silent) message(fxNa," Argument 'nCombin' is too high for given data, re-setting to ",nCombin)
    }
    outDiNa <- list( c("sing","doub","trip","min2","any"), "x", c("n","sem","CI","sd"))
  }
    
  if(datOK) {
    if(nCombin ==length(lev)) {
      out <- table(table(unlist(lst)))[c("1","2","3")]
      out <- array(c(out, sum(out[c(1:2)]), sum(out), rep(NA,5*3)), dim=c(5,1,4), dimnames=outDiNa)
    } else {
      ## MAIN, prepare matrix of combin
      ## Setup combinations to try
      if(isTRUE(remDouble) && length(unique(levNo)) >1) { nCombin <- min(nCombin, length(levels(as.factor(lev))), na.rm=TRUE)
        if(!silent) message(fxNa," argument 'nCombin' combined to 'remDouble'=TRUE  is too high for given data, re-setting to ",nCombin)}
      combOfN <- utils::combn((1:length(lst)), nCombin)              # the combinations (all) ..
      ## prepare separator for names of combinations
      chSep <- sapply(colNaSep, function(x) length(grep(x, levNo)) >0)
      sep <- if(any(!chSep)) colNaSep[which(!chSep)[1]] else "_"
      colnames(combOfN) <- apply(combOfN, 2, function(x) paste(sort(lev[x]), collapse=sep))
      if(debug) {message(fxNa,"cA3a")}
      ## FILTER
      ## idea : remove all multi-intra exect all-intra
      if(remDouble) {           # remove mixed repeats (double-same + other) but keep all-same (or higher n-mers)
        chD <- sapply( strsplit(colnames(combOfN), sep), function(x) length(unique(x)))
        chD <- chD %in% c(nCombin, if(inclMono) 1 else NULL)
        if(any(!chD)) { combOfN <- if(sum(chD) >1) combOfN[,which(chD)] else matrix(combOfN[,which(chD)], ncol=1, dimnames=list(NULL, colnames(combOfN)[which(chD)]))
          if(debug) {message(fxNa,"Removing ",sum(!chD)," poly-repeats but not auto-repeats (out of ",ncol(combOfN),
            ") combinations since argument 'remDouble=",remDouble,"'  cA3b")}
        }
      }
      if(debug) { message(fxNa,"cA3c")}

      ## main part, extract elements and count frequencies
      com3c <- apply(combOfN, 2, function(x) sortByNRepeated(lst[x], silent=silent, debug=debug, callFrom=fxNa))   # make list of number of times found: 1x, 2x, 3x ...  takes a few sec
      forceN <- 3                                     # ?? for extracting (no more than) c("sing","doub","trip")
      if(debug) {message(fxNa,"cA3d")}
      maxLev <- max(sapply(com3c, length))
      tmp <- matrix(0, ncol=length(com3c), nrow=max(forceN, maxLev), dimnames=list(1:max(maxLev, forceN), names(com3c)))
      for(i in 1:length(com3c)) {tm2 <- if(is.matrix(com3c[[i]])) rep(nrow(com3c[[i]]), ncol(com3c[[i]])) else sapply(com3c[[i]], length)
        tmp[if(length(names(tm2)) >0) as.integer(names(tm2)) else 1:length(tm2), i] <- tm2  }
      rownames(tmp) <- c("sing","doub","trip", if(nrow(tmp) > 3) paste0("_",c(4:nrow(tmp)),"x"))
      ## adhere to previous structure (sing, doub, min2, any & rest), ie adjust number of indv x-counts to forceN
      comCou2 <- rbind(tmp[1:forceN,], min2=colSums(tmp[1:2,]), any=colSums(tmp))
      dupNa <- duplicated(colnames(comCou2), fromLast=FALSE)
      if(debug) {message(fxNa,"cA3e  Found ",sum(dupNa)," 'redundant' level-combinations  out of ",length(dupNa))}

      ## which contain repeated combinations of groups: calculate sd & derivatives, organize as array
      if(any(dupNa)) {
        grTy <- unique(colnames(comCou2))
        sumMatr <- function(x) { avX <- rowMeans(x); sdX <- rowSds(x); c(nAv=avX, sem=sdX/sqrt(ncol(x)), CI=apply(x, 1, confInt), sd=sdX)}   # seems OK

        comCou3 <- sapply(grTy, function(x) { y <- comCou2[,which(colnames(comCou2) %in% x)]; if(length(dim(y)) >1) sumMatr(y) else c(y, rep(NA,length(y)*3))} )
        if(length(dim(comCou3)) <2) comCou3 <- matrix(comCou3, ncol=1, dimnames=list(NULL, colnames(comCou2)))
        allNa <- colnames(comCou3)
        faRep <- findRepeated(colnames(comCou3), nonRepeated=TRUE)     # correct object (note: all colnames are already different)
        out <- array(NA, dim=c(nrow(comCou2), length(faRep$rep) +length(faRep$nonrep), 4), dimnames=list(rownames(comCou2), allNa,c("n","sem","CI","sd")))
          #isMult <- match(names(faRep$rep), allNa)   # needed ?
        if(debug) {message(fxNa,"cA3g")}
        for(i in 1:(dim(out)[3])) out[,,i] <- comCou3[(i-1)*nrow(out) +(1:nrow(out)),]
      } else {
        out <- array(c(comCou2, rep(NA, nrow(comCou2)*ncol(comCou2)*3)), dim=c(nrow(comCou2),ncol(comCou2),4),
          dimnames=list(rownames(comCou2), colnames(comCou2),c("n","sem","CI","sd")))
      }
    }
  }
  out }
    
