#' Value Matching With Option For Concatenated Terms
#'
#' This is a _match()_-like function allowing to serach among concatenated terms/IDs, additional options to remove text pattern like terminal lowercase extesion are available.
#' The function returns a named vector indicating the positions of (first) matches similar to \code{\link[base]{match}}.
#'
#' @details
#' The main motivation to create this function was to be able to treat concatenated entries and to look if \code{any} of the concatenated values match to 'x'.
#' This function offers additional options for trimming values before running the main comparison.
#'
#' Of course, the concatenation strategy must be known and only a single concatenation separator (which may be multiple characters long) may be used for both \code{x} and \code{match}.
#' Thus result will only indicate that at least one of the concatenated terms had a match, but not which one.
#' Finally, both vectors \code{x} and \code{table} may contain concatenated terms.
#' In this case this function will require much more computational ressources due to the increased combinatorics when comparing larger vectors.
#'
#' Please note, that in case of multiple to multiple matches, only the first hit gets reported.
#'
#' The argument \code{globalPat="digitExtension"} allows eg reducing 'A1234-4' to 'A1234'.
#'
#' @param x (vector) the values to be matched
#' @param table (vector) the values to be matched against (ie reference)
#' @param sep (character) separator character in case concatenation of entries is tested
#' @param sepPattern (character or \code{NULL}) optional custom  pattern for splitting concatenations of \code{x}) and \code{table}) (in case \code{NULL}) is not sufficient)
#' @param globalPat (character) pattern for additional trimming of serach-terms. If \code{globalPat="digitExtension"} all terminal digits will not be considered when matching
#' @param nomatch (vector) similar to \code{\link[base]{match}} the value to be returned in the case when no match is found
#' @param incomparables (vector) similar to \code{\link[base]{match}}, a vector of values that cannot be matched. Any value in x matching a value in this vector is assigned the nomatch value.
#' @param extensPat (logical) similar to \code{\link[base]{match}} the value to be returned in the case when no match is found
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a character vector with verified path and file-name(s), returns \code{NULL} if nothing
#' @seealso \code{\link[base]{match}} (for two simple vectors without concatenated terms), \code{\link[base]{grep}}
#' @examples
#' tab1 <- c("AA","BB-5","CCab","FF")
#' tab2 <- c("AA","WW,Vde,BB-5,E","CCab","FF,Uef")
#' x1 <- c("ZZ","YY","AA","BB-2","DD","CCdef","Dxy")            # modif of single ID (no concat)
#' concatMatch(x1, tab2)
#' x2 <- c("ZZ,Z","YY,Y","AA,Z,Y","BB-2","DD","X,CCdef","Dxy")  # conatenated in 'x'
#' concatMatch(x2, tab2)
#' tab1 <- c("AA","BB-5","CCab","FF")              # no conatenated in 'table'
#' concatMatch(x2, tab1)                          # simple case of no concat anywhere
#' concatMatch(x1, tab1)
#' @export
concatMatch <- function(x, table, sep=",", sepPattern=NULL, globalPat="digitExtension", nomatch=NA_integer_, incomparables=NULL, extensPat=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## move to wrMisc ?
  ## idea : find where x (eg fasta) matche(s) IN table (MQresult, has concat)
  ## both \code{x} and \code{table} may contain concatenated IDs
  ## match-like function including each of concatenated terms/IDss, add'l option to remove add'l pattern like terminal lowercase extension
  ## strategy A : few concatenations : run match() on 1st set of table, 2nd set etc
  ## strategy B : many concat & short x : grep each x in table (as heading, sep+x+sep, as term)
  ##  globalPat (character, 'digitExtension') regular expression for searching eg reduce 'A1234-4' to 'A1234'
  ##  extensPat (charcter) \code{TRUE} for removing terminal lower case eg ('AB1234ups')
  ## need to find if any of concatenated 'x' in 'table'
  ## prepare
  fxNa <- wrMisc::.composeCallName(callFrom, newNa="concatMatch")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  xIni <- x; tableIni <- table           # initialize backup
  chDux <- duplicated(x)
  if(length(x) <1) stop("Invalid entry, 'x' must be at least of length=1")
  if(length(table) <1) stop("Invalid entry, 'table' must be at least of length=1")
  if(!any(c("numeric", "character", "logical") %in% class(x)))  stop("Invalid entry, 'x' must be numeric, character or logical vector")
  if(!any(c("numeric", "character", "logical") %in% class(table)))  stop("Invalid entry, 'table' must be numeric, character or logical vector")
  if(any(chDux) && !silent) message(fxNa,"Note, some entries of 'x' are not unique !")
  chDut <- duplicated(table)
  if(any(chDut) && !silent) message(fxNa,"Note, some entries of 'table' are not unique !")
  if(debug) {message(fxNa,"cMa1"); cMa1 <- list(x=x,table=table, sep=sep,sepPattern=sepPattern,globalPat=globalPat,extensPat=extensPat)}
  ## rm term sep
  chSep <- grep(paste0(sep,"$"), x)
  if(length(chSep) >0) {x <- sub(paste0(sep,"$"), "", x)}
  chSep <- grep(paste0(sep,"$"), table)
  if(length(chSep) >0)  {table <- sub(paste0(sep,"$"), "", table)}
  ## however - possibility that x or table become non-unique due to manipulation !!
  if(length(sepPattern) <1) sepPattern <- sep
  ## remove terinal extensions (eg AB1234ups)
  if(length(extensPat) ==1) {
    if(isTRUE(extensPat)) {     # remove terminal lower case eg ('AB1234ups')
      chPatXt <- grep("[A-Z0-9][[:lower:]]+$", x)                   # tailing pattern
      chPatXc <- grep(paste0("[A-Z0-9][[:lower:]]+",sep,"."), x)    # center pattern
      chPatTt <- grep("[A-Z0-9][[:lower:]]+$", table)
      chPatTc <- grep(paste0("[A-Z0-9][[:lower:]]+",sep,"."), table)
      if(length(chPatXt) >0) x[chPatXt] <- sub("[[:lower:]]+$","", x[chPatXt])
      if(length(chPatXc) >0) x[chPatXc] <- sub(paste0("[[:lower:]]+",sep), sep, x[chPatXc])
      if(length(chPatTt) >0) table[chPatTt] <- sub("[[:lower:]]+$","", table[chPatTt])
      if(length(chPatTc) >0) table[chPatTc] <- sub(paste0("[[:lower:]]+",sep), sep, table[chPatTc])
    } else {
      chExt <- grep(extensPat, x)
      if(length(chExt) >0) x <- sub(extensPat, "", x)}
    chDux <- duplicated(x)
    if(any(chDux) && !silent) message(fxNa,"Note, some entries of 'x' are not unique ! (after cleaning by 'extensPat')")
    chDut<- duplicated(table)
    if(any(chDut) && !silent) message(fxNa,"Note, some entries of 'table' are not unique ! (after cleaning by 'extensPat')")
  }
  ## global pattern modif
  if(length(globalPat) >0) {
    ## however - possibility that x or table become non-unique due to manipulation !!
    if(identical(globalPat, "digitExtension")) {                          ## remove all after '-' eg ('P1234-5')
      chPatXt <- grep("[A-Z0-9]\\-[[:digit:]]{1,4}$", x)                   # tailing pattern
      chPatXc <- grep(paste0("[A-Z0-9]\\-[[:digit:]]{1,4}",sep,"."), x)    # center pattern
      chPatTt <- grep("[A-Z0-9]\\-[[:digit:]]{1,4}$", table)
      chPatTc <- grep(paste0("[A-Z0-9]\\-[[:digit:]]{1,4}",sep,"."), table)
      if(length(chPatXt) >0) x[chPatXt] <- sub("\\-[[:digit:]]{1,4}$","", x[chPatXt])
      if(length(chPatXc) >0) x[chPatXc] <- sub(paste0("\\-[[:digit:]]{1,4}",sep), sep, x[chPatXc])
      if(length(chPatTt) >0) table[chPatTt] <- sub("\\-[[:digit:]]{1,4}$","", table[chPatTt])
      if(length(chPatTc) >0) table[chPatTc] <- sub(paste0("\\-[[:digit:]]{1,4}",sep), sep, table[chPatTc])
    } else {
      chPat <- grep(globalPat, x)
      if(length(chPat) >0) x[chPat] <- sub(globalPat, sep, x[chPat])
      chPat <- grep(globalPat, table)
      if(length(chPat) >0) table[chPat] <- sub(globalPat, sep, table[chPat])}
    chDux <- duplicated(x)
    if(any(chDux) && !silent) message(fxNa,"Note, some entries of 'x' are not unique ! (after cleaning by 'globalPat')")
    chDut<- duplicated(table)
    if(any(chDut) && !silent) message(fxNa,"Note, some entries of 'table' are not unique ! (after cleaning by 'globalPat')")
  }

  if(debug) { message(fxNa,"cMa2"); cMa2 <- list(x=x,table=table, sep=sep,sepPattern=sepPattern,globalPat=globalPat,extensPat=extensPat,chSep=chSep)}

  ## MAIN :
  ## start with regular match
  out <- match(x, table, nomatch=nomatch, incomparables=incomparables)
  x1 <- x; table1 <- table
  names(out) <- x
  chMa <- is.na(out)

  if(any(chMa)) {
    ## not all items of 'x' found in 'table' ==> try complementing missing
    ##  now work only on x not prevusouly found !
    if(debug) {message(fxNa,sum(chMa)," ID/term of 'x' not found so far : ", wrMisc::pasteC(utils::head(x[which(chMa)], quoteC="'")))}
    ## check for separators
    chSepT <- grep(sepPattern, table)             # which 'table' are concatented => useful for splitting
    chSepX <- grep(sepPattern, x[which(chMa)])    # don't use truncation (strat A) when x has concatenation
    if(debug) { message(fxNa,"cMa3"); cMa3 <- list(x=x,table=table,out=out,chMa=chMa,chSepT=chSepT,chSepX=chSepX, sep=sep,sepPattern=sepPattern,globalPat=globalPat,extensPat=extensPat,chSep=chSep)}
    if(length(chSepT) >0 || length(chSepX) >0) {
      if(debug) message(fxNa,"Found ",length(chSepT)," cases of conatenated IDs in 'table' -> split;  ",length(chSepX)," cases of conatenated 'x'")
      ## has separators
      ## now devide strat A and B
      nCharX <- nchar(x)
      nSepX <- (nCharX - nchar(gsub(sepPattern,"", x))) /nchar(sep)
      nCharTab <- nchar(table)
      nSepTab <- (nCharTab - nchar(gsub(sepPattern,"", table))) /nchar(sep)       # number of concatenated terms for each 'table'
      if(length(chSepX) <1 && max(nSepTab, na.rm=TRUE) / length(x) < 0.5 && max(nSepTab, na.rm=TRUE) < 15) {
        ## strategy A :
        ##   few concatenations : run match() on 1st set of table, 2nd set etc
        if(debug) message(fxNa,"Choose strategy A : few concatenations : run match() on 1st set of table, 2nd set etc")
        if(debug) { message(fxNa,"cMa4"); cMa4 <- list(x=x,table=table,out=out,chSepT=chSepT,chMa=chMa,chSepT=chSepT, sep=sep,sepPattern=sepPattern,globalPat=globalPat,extensPat=extensPat,chSep=chSep)}
        ##
        for(i in 0:max(nSepTab, na.rm=TRUE)) {
          if(length(chSepT) >0 && any(is.na(out))) {                   # yes, need to go further and split
            if(i >0) {chSepT <- grep(sepPattern, table)                # update (which 'table' are concatented => use for splitting)
               table <- substring(table, nchar(tabT) +2) }             # removve 1st element from left (already treated)
            tabT <- sub(paste0(sep,".+"), "", table[chSepT])           # truncate from right, ie leave only 1st
            outNa <- which(is.na(out))
            maT <- match(x[outNa], tabT, nomatch=nomatch, incomparables=incomparables)
              names(maT) <- x[which(is.na(out))]
            chNa <- !is.na(maT)
            if(any(chNa)) { out[outNa[which(chNa)]] <- chSepT[maT[which(chNa)]]
              if(debug) message(fxNa,"Found ",sum(chNa)," add'l matches in i=",i+1) }
          }
        }
      } else {
        ## strategy B :
        ##    many concat & short x : grep each 'x' in table (as heading, sep+x+sep, as term) ==> may be slow when 'x' is long
        ## also works if 'x' has concatenations
        if(debug) message(fxNa," ++ Strategy B : use grep for terminal 'x' or 'x' combined with 'sep'")
        .sepGrep <- function(xx, sep, table) grep(paste0("^",xx,"$|","^",xx,sep,"|",sep,xx,sep,"|",sep,xx,"$"), table)    # terminal & separator-grep
        if(length(chSepX) <1) {
          out2 <- lapply(x[which(chMa)], .sepGrep, sep, table)
          out2 <- sapply(out2, function(y) if(length(y) >0) y[1] else NA)
        } else {
          ## double concatenated (x & table) => strsplit x for grep of each !
          if(debug) { message(fxNa,"cMa5"); cMa5 <- list(x=x,table=table,out=out,chSepT=chSepT,chMa=chMa,chSepT=chSepT, sep=sep,sepPattern=sepPattern,globalPat=globalPat,extensPat=extensPat,chSep=chSep)}
          spl <- strsplit(x[which(chMa)], sep)
          naOm3 <- function(zz, sep, table) {zz <- unlist(lapply(zz, .sepGrep, sep, table)); isNa <- is.na(zz); if(any(!isNa)) zz[which(!isNa)[1]] else NA }  # return 1st non-NA or NA from sepGrep search
          out2 <- sapply(spl, naOm3, sep, table)
        }
        chLe <- sapply(out2, length)
        if(any(chLe) >1) {
          if(!silent) message(fxNa,"Found ",sum(chLe >1)," cased of multiple matching after split & grep, using only first") }
        out[which(chMa)] <- unlist(out2)
      }
    } else {if(debug) message(fxNa,"Remain with match-approach (no concatenations found)")
    }
  } else if(debug) message(fxNa,"All initial 'x' were found in 'table'")
out }
