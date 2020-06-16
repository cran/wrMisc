#' Standard error of median for each column by bootstrap
#'
#' Determine standard error (sd) of median by bootstraping for multiple sets of data (rows in input matrix 'dat').
#' Note: Uses the package \href{https://CRAN.R-project.org/package=boot}{boot}.
#' @param dat (numeric) matix 
#' @param nBoot  (integer) number if iterations
#' @return (numeric) vector with estimated standard errors
#' @seealso \code{\link[boot]{boot}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
#' colMedSds(dat1) 
#' @export
colMedSds <- function(dat,nBoot=99){
  msg <- "'dat' should be matrix or data.frame with "
  if(is.null(ncol(dat))) stop(msg,"multiple columns !") else if(ncol(dat) < 2) stop(msg,"at least 2 columns !")
  chPa <- try(find.package("boot"),silent=TRUE)
  if("try-error" %in% class(chPa)) stop("package 'boot' not found") 
  median.fun <- function(dat,indices) stats::median(dat[indices],na.rm=TRUE)
  out <- apply(dat,2,function(x) stats::sd(boot::boot(data=x, statistic=median.fun, R=nBoot)$t))
  out }
 
