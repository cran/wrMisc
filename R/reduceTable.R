#' Reduce table by aggregating smaller groups 
#'
#' \code{reduceTable} treats/reduces results from \code{\link[base]{table}} to 'nGrp' groups, 
#' optional indiv resolution of 'separFirst' (numeric or NULL). 
#' Mainly made for reducing the number of classes for betters plots with \code{\link[graphics]{pie}}
#'
#' @param tab output of \code{\link[base]{table}}
#' @param separFirst (integer or NULL) optinal separartion of n 'separFirst' groups (value <2 or NULL 
#'  will priviledge more uniform size of groups, higher values will cause small inital and larger tailing groups) 
#' @param nGrp (integer) number of groups expected
#' @return numeric vector with number of counts and class-borders as names (like table).
#' @seealso \code{\link[base]{table}}
#' @examples
#' set.seed(2018); dat <- sample(11:60,200,repl=TRUE)
#' pie(table(dat))
#' pie(reduceTable(table(dat),sep=NULL))
#' pie(reduceTable(table(dat),sep=NULL),init.angle=90,clockwise=TRUE,col=rainbow(20)[1:15],cex=0.8) 
#' @export
reduceTable <- function(tab,separFirst=4,nGrp=15){
   if(!"table" %in% class(tab)) message(" Note: Expect class 'table' for argument 'tab' for proper functioning")
   cuSu <- cumsum(tab)
   if(length(separFirst) >0) {ch <- grep("^[[:digit:]]$",separFirst[1])
     separFirst <- if(length(ch)==1) as.numeric(separFirst[1]) else NULL}
   if(length(separFirst) >0) {
      nGrp <- nGrp -separFirst +1
      if(nGrp <1) cat("correcting nGrp -> 1 \n") else cat(" nGrp=",nGrp,"\n")
      if(nGrp <1) nGrp <- 1 
      cu <- as.numeric(cut(cuSu,nGrp))+separFirst-1
      cu[1:separFirst] <- 1:separFirst
   } else cu <- as.numeric(cut(cuSu,nGrp))
   ## idea : modify to to priviledge round numbers (of names(tab))
   out <- tapply(tab,cu,sum)
   names(out) <- tapply(as.numeric(names(tab)),cu,function(x) if(length(x) >1) paste(range(x),collapse="-") else x)
   out }
      
