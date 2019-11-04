#' Check for similar values in series
#'
#' \code{checkSimValueInSer} checks all values of 'x' for similar values outside/within (relative) range of 'ppm' (ie ambiguous within given range).
#' Return logical vector : FALSE for each entry of 'x' if value inside of ppm range to neighbour 
#' @param x numeric vector
#' @param ppm (numeric) ppm-range for considering as similar
#' @param sortX (logical) allows speeding up function when set to FALSE, for large data that are already sorted
#' @return logical vector : FALSE for each entry of 'x' if value inside of ppm range to neighbour
#' @seealso similar with more options \code{\link{withinRefRange}}
#' @examples
#' va1 <- c(4:7,7,7,7,7,8:10)+(1:11)/28600; checkSimValueInSer(va1)
#' cbind(va=va1,simil=checkSimValueInSer(va1))
#' @export
checkSimValueInSer <- function(x,ppm=5,sortX=TRUE) {
  nNA <- sum(is.na(x))
  if(nNA >0) message("  ",nNA," NA values")
  so <- if(sortX) sort(x) else x
  di <- diff(so)
  ch <- which(di > di[c(1,1:(length(di)-1))])
  if(length(ch) >0) di[ch] <- di[ch-1]                # adust by looking at diff in other direction
  out <- di > so[-length(so)]*ppm*1e-6
  out <- c(out,out[length(out)])
  if(sortX) !out[order(so)] else !out }

