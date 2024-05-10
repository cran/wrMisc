#' Combine replicates from list to matrix
#'
#' Suppose multiple measures (like multiple chanels) are taken for subjects and these measures are organized as groups in a list, 
#' like muliple parameters (= channels) or types of measurements (typically many paramters are recorded when screeinig compounds in microtiter plates).
#' Within one parameter/channel all replicate-data from separate list-entries ('lst') will get combined according to names of list-elements.
#' The function will trim any redundant text in names of list-elements, try to isolate separator (may vary among replicate-groups, but should be 1 character long).
#' eg names "hct116 1.1.xlsx" & "hct116 1.2.xlsx" will be combined as replicates, "hct116 2.1.xlsx" will be considered as new group.
#' 
#' @param lst (list)  list of arrays (typically multi-parameter measures of micortiterplate data)
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return list of arrays now with same dimension of arrays (but shorter, since replicate-arrays were combined)
#' @seealso \code{\link{extr1chan}}, \code{\link{organizeAsListOfRepl}}
#' @examples
#' lst2 <- list(aa_1x=matrix(1:12,nrow=4,byrow=TRUE),ab_2x=matrix(24:13,nrow=4,byrow=TRUE))
#' combineReplFromListToMatr(lst2)
#' @export
combineReplFromListToMatr <- function(lst, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="combineReplFromListToMatr")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  varNa <- .trimRight(.trimLeft(names(lst)))
  varSep <- gsub("[[:alnum:]]","",varNa)
  if(any(nchar(varSep) <1)) stop(fxNa,"Manually EDIT file-names, can't find separator between group/plate-name and replicate-number !")
  if(any(nchar(varSep) >1)) message(fxNa,"So far designed for single separator character; use gregexpr & unlist",
    "\ Here :",paste(varSep,collapse=" "))
  varSep[which(varSep== ".")] <- "\\."
  varNa2 <- apply(cbind(varNa, varSep), 1, function(x) unlist(strsplit(x[1], x[2])) )
  arrTy <- table(varNa2[1,])[rank(unique(varNa2[1,]))]
  out <- list()
  iniFi <- 0
  for(i in 1:length(arrTy)) {
    check <- sapply(lst[iniFi +(1:arrTy[i])], length)
    if(length(unique(check)) !=1) {
      message(fxNa,"Plate-type ",i," :  Trouble ahead : Dimensions of plates not constant ! files ",paste(names(lst)[iniFi+(1:arrTy[i])],collapse=" "))
    } else {
      out[[i]] <- as.matrix(sapply(lst[iniFi +(1:arrTy[i])], as.numeric))
      dimNa <- dimnames(lst[[1 +iniFi]])
      dimnames(out[[i]]) <- list(paste(dimNa[[1]], rep(sub("^X","",dimNa[[2]]), each=length(dimNa[[1]])), sep=""), varNa2[2, iniFi +(1:arrTy[i])] )
      iniFi <- iniFi + arrTy[i]
   } }
   names(out) <- unique(varNa2[1,])
   out }
   
