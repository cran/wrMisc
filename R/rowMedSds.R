#' sd of median for each row by bootstrap
#'
#' \code{rowMedSds} determines the stand error (sd) of the median for each row by bootstraping each row of 'dat'.
#' Note: requires package \href{https://CRAN.R-project.org/package=boot}{boot}
#'
#' @param dat (numeric) matix, main input 
#' @param nBoot (integer) number if iterations for bootstrap
#' @return (numeric) vector with estimated standard errors
#' @seealso \code{\link[boot]{boot}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)), ncol=10)
#' rowMedSds(dat1) ; plot(rowSds(dat1), rowMedSds(dat1))
#' @export
rowMedSds <- function(dat, nBoot=99) {
  msg <- "'dat' should be matrix or data.frame with "
  if(is.null(ncol(dat))) stop(msg,"multiple columns !") else if(ncol(dat) < 2) stop(msg,"at least 2 columns !")
  if(!requireNamespace("boot", quietly=TRUE)) {
    warning("package 'boot' not found ! Please install first from CRAN")   
  } else {  
    median.fun <- function(dat,indices) stats::median(dat[indices],na.rm=TRUE)
    out <- try(apply(dat,1,function(x) stats::sd(boot::boot(data=x, statistic=median.fun, R=nBoot)$t)))
    if("try-error" %in% class(out)) stop(" Did not succeed in running boot()")
    out }}
   
