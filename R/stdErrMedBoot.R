#' Standard eror of median by boot-strap
#'
#' \code{stdErrMedBoot} estimate standard eror of median by boot-strap approach. 
#' Note: requires package \href{https://CRAN.R-project.org/package=boot}{boot}
#'
#' @param x (numeric) vector to estimate median and it's standard error
#' @param nBoot (integer) number for iterations
#' @return This function returns a (numeric) vector with estimated standard error
#' @seealso \code{\link[boot]{boot}}
#' @examples
#' set.seed(2014); ra1 <- c(rnorm(9,2,1),runif(8,1,2))
#' rat1 <- ratioAllComb(ra1[1:9],ra1[10:17])
#' median(rat1); stdErrMedBoot(rat1)
#' @export
stdErrMedBoot <- function(x, nBoot=99) {  
  ## uses package boot
  chPa <- requireNamespace("boot", quietly=TRUE)
  if(!chPa) { warning("package 'boot' not found ! Please install first from CRAN")
    NULL 
  } else {
    median.fun <- function(dat, indices) stats::median(dat[indices], na.rm=TRUE)
    out <- try(stats::sd(boot::boot(data=x, statistic=median.fun, R=nBoot)$t))
    if(inherits(out, "try-error")) stop("Unable to run bootstrap, check format of data and argument 'nBoot'")
    out }}
 
