#' Contingenty tables for fit of ranking 
#'
#' Count the number of instances where the corresponding columns of 'dat' have a value matching the group number as specified by 'grp'.
#' Counting will be performed/repeated independently for each line of 'dat'.
#' Returns array  (1st dim is rows of dat, 2nd is unique(grp), 3rd dim is ok/bad), these results may be tested using eg \code{\link[stats]{fisher.test}}.
#' This function was made for prearing to test the ranking of multiple features (lines in 'mat') including replicates (levels of 'grp'). 
#' 
#' @param dat (matrix or data.frame of integer values) ranking of multiple features (lines), equal ranks may occur   
#' @param grp (integer) expected ranking
#' @return array (1st dim is rows of dat, 2nd is unique(grp), 3rd dim is ok/bad)
#' @seealso \code{\link[stats]{lm}}
#' @examples
#' # Let's create a matrix with ranks (equal ranks do occur)
#' ma0 <- matrix(rep(1:3,each=6), ncol=6, dimnames=list(
#'   c("li1","li2","ref"), letters[1:6]))
#' ma0[1,6] <- 1                       # create item not matching correctly
#' ma0[2,] <- c(3:1,2,1,3)             # create items not matching correctly
#' gr0 <- gl(3,2)                      # the expected ranking (as duplicates)
#' (count0 <- rankToContigTab(ma0,gr0))
#' cTab <- t(apply(count0, c(1,3) ,sum))
#' # Now we can compare the ranking of line1 to ref ...
#' fisher.test(cTab[,c(3,1)])          # test li1 against ref
#' fisher.test(cTab[,c(3,2)])          # test li2 against ref
#' @export
rankToContigTab <- function(dat,grp) {
  ## It is supposed that the columns of dat can be cut in multiple groups according to (sorted) grp
  ## Then the number of instances where the corresponding columns of dat have a value matching the group number is counted
  ## returns array  (1st dim is rows of dat, 2nd is unique(grp), 3rd dim is ok/bad)
  grUnq <- unique(grp)
  chSo <- sort(grUnq)==grUnq 
  if(length(dim(dat)) <2) dat <- matrix(as.numeric(dat),nrow=1,dimnames=list(NULL,names(dat)))
  if(any(!chSo)) { message("need to re-sort groups !")
    newOrd <- order(grp)
    oldRa <- rank(grp)          # for setting back in old order
    dat <- dat[,newOrd]
    grp <- grp[newOrd]
    grUnq <- unique(grp)  
  } else newOrd <- NULL
  lims <- sapply(grUnq,function(x) range(which(grp==x)))
  out <- array(dim=c(nrow(dat),length(grUnq),2),dimnames=list(rownames(dat),grUnq,c("ok","bad")))
  for(i in 1:length(grUnq)) { z <- dat[,lims[1,i]:lims[2,i]]
    out[,i,] <- if(nrow(z) >1) cbind(ok=rowSums(z ==i,na.rm=TRUE),bad=rowSums(z !=i,na.rm=TRUE)) else c(ok=sum(z ==i,na.rm=TRUE),bad=sum(z !=i,na.rm=TRUE)) }
  out }  
  
