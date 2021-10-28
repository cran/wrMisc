#' Distance of categorical data (Jaccard,Rand and adjusted Rand index) 
#'
#' \code{randIndFx} calculates distance of categorical data (as Rand Index, Adjusted Rand Index or Jaccard Index). 
#' Note: uses/requires package \href{https://CRAN.R-project.org/package=flexclust}{flexclust}
#' Methods so far available (via flexclust): "ARI" .. adjusted Rand Index, "RI" .. Rand index, "J" .. Jaccard, "FM" .. Fowlkes-Mallows.
#' @param ma (matrix) main input for distance calulation
#' @param method (character) name of distance method (eg "ARI","RI","J","FM")
#' @param adjSense (logical) allows introducing correlation/anticorrelation (interprete neg distance results as anti)
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return distance matrix
#' @seealso \code{comPart} in \code{\link[flexclust]{randIndex}}
#' @examples
#' set.seed(2016); tab2 <- matrix(sample(1:2, size=42, replace=TRUE), ncol=7)
#' if(requireNamespace("flexclust")) { flexclust::comPart(tab2[1,],tab2[2,])
#'   flexclust::comPart(tab2[1,],tab2[3,])
#'   flexclust::comPart(tab2[1,],tab2[4,]) }
#' ## via randIndFx():
#'   randIndFx(tab2, adjSense=FALSE)
#'   cor(t(tab2))
#'   randIndFx(tab2, adjSense=TRUE)
#' @export
randIndFx <- function(ma, method="ARI", adjSense=TRUE, silent=FALSE, callFrom=NULL){
  ## calculate distance for categorical data (using Rand Index, Adjusted Rand Index or Jaccard Index)
  ## method : "ARI" .. adjusted Rand Index, "RI" .. Rand index, "J" .. Jaccard, "FM" .. Fowlkes-Mallows
  ## 'adjSense' allows introducing corretaltion/anticorrelation (interprete neg distance results as anti)
  ## uses package flexclust
  ## wr 29jan15, cor 23mar16
  ## require(flexclust)
  fxNa <- .composeCallName(callFrom, newNa="randIndFx")
  if(!isTRUE(silent)) silent <- FALSE
  if(!requireNamespace("flexclust", quietly = TRUE)) {
    warning("Package 'flexclust' not found ! Please install first from CRAN")  
  } else { 
    if(!is.matrix(ma) & !silent) message(fxNa," Caution : data-frames with factors may cause problems !!")
    if(is.logical(ma)) stop(fxNa,"Expecting matrix with integer values")
    maCo <- matrix(1:nrow(ma), ncol=nrow(ma), nrow=nrow(ma))
    maCo <- cbind(x=maCo[upper.tri(maCo)], y=t(maCo)[upper.tri(maCo)])
    maCo <- maCo[order(maCo[,1], maCo[,2]),]                                 # need proper order for upper.tri
    di <- try(apply(maCo, 1, function(x) flexclust::comPart(ma[x[1],],ma[x[2],],type=method)), silent=TRUE)
    if(any(class(di) == "try-error")) message(fxNa,"Problem running flexclust::comPart ! (package might not be installed) class ",class(di)," mode ",mode(di)," ",di)
    out <- matrix(NA, nrow=nrow(ma), ncol=nrow(ma), dimnames=list(rownames(ma),colnames(ma)))
    out[upper.tri(out)] <- rev(di)                    # not used any more by as.dist()
    out[lower.tri(out)] <- di
    if(adjSense){
      out <- .scaleXY(out, minim=0, maxim=max(out,na.rm=TRUE))                 # re-scale from 0 to max
      mi <- min(out, na.rm=TRUE)
      maX <- max(out, na.rm=TRUE)
      if(mi <0) out <- (out -mi)*maX/(maX -mi)
      orient <- try(stats::cor(t(ma)) >0)
      if("try-error" %in% class(orient)) { out <- NA
        warning(fxNa," PROBLEM with calulating cor(), returning 0 ")
      } else out <- as.matrix(out)*(-1 +2*orient) }
    stats::as.dist(out) }}
   
