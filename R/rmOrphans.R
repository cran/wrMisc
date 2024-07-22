#' Remove or Reassign Orphan Indexes
#'
#' This function allows detecting terminal orphans of a vector of (cluster-) indexes and removing (ie marking as \code{NA})
#'   or re-assigning them to the neighbour class towrds the center.
#'
#' @details
#' All input of \code{ind} is supposed to be interger values as (cluster-) indexes.
#' This function will look if the lowest and/or highest (cluster-) indexes appear at very low frequency so that they may be considered orphans. 
#' The argument \code{minN} assigns the threshold of when the frquency of terminal values may be considered as 'orphan',
#' either as absolute threshold or if less than 1 as ratio (0.1 => 10%tile of length of \code{ind}) .
#' 
#' The argument \code{side} may be 'both', 'b', 'upper', 'u', 'lower' or 'l', to decide if lower and/or upper end indexes should be treated.
#' 
#' @param ind (integer) main input of (cluster-) indexes
#' @param minN (numeric, length=1) the min frequency to consider as orphans, if less than 1 it will be interpreted as ratio compared to length of \code{index}
#' @param reassign (logical) if \code{TRUE} orphan indexes will be replaced by neighbour class indexes (towrds the center)
#' @param side (character) may be 'both', 'b', 'upper', 'u', 'lower' or 'l' to decide if lower and/or upper end indexes should be treated.
#' @param callFrom (character) allows easier tracking of messages produced
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @return This function returns an integer vector of adjusted indexes
#' @seealso \code{\link[base]{table}}
#' @examples
#'  x=c(3:1,3:4,4:6,5:3); rmOrphans(x)
#' rmOrphans(x, minN=0.2) 
#' ## reassign orphans to neighbour center class
#' cbind(x,  x=x, def=rmOrphans(x, reassign=TRUE), 
#'   minN=rmOrphans(x, minN=0.2, reassign=TRUE) )
#' @export
rmOrphans <- function(ind, minN=1, reassign=FALSE, side="both", silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## reassign/remove orphans (groups with too few members) from both ends of vector if indexes 'ind'
  ## minN (numeric) if 0 > minN > 1 will be interpreded as ratio (0.1 => 10%tile of length of ind) otherwise as interger threshold for min number of values in terminal groups
  fxNa <- .composeCallName(callFrom, newNa="rmOrphans")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(ind) >0 && is.character(ind)) ind <- as.integer(factor(ind))
  if(length(ind) <1 || all(is.na(ind))) {
    if(!silent) message(fxNa,"Nothing to do ..")
  } else {
    minN <- if(length(minN) <1 || !is.numeric(minN)) 1 else {if(minN >0 && minN <1) round(length(ind)*minN) else as.integer(minN)}
    tab <- table(ind)
    begOrph <- cumsum(tab <= minN) ==1:length(tab)
    if(any(begOrph) && is.character(side) && any(c("both","lower","b","l") %in% side)) { 
      rmVal <- names(begOrph)[which(begOrph)]       
      ind[which(ind %in% rmVal)] <- if(length(reassign)==1 && isTRUE(reassign)) sum(begOrph) +1 else NA
      tab <- table(ind) }
    endOrph <- cumsum(rev(tab) <= minN) ==1:length(tab)
    if(any(endOrph) && is.character(side) && any(c("both","upper","b","u") %in% side)) { rmVal <- names(endOrph)[which(endOrph)]
      ind[which(ind %in% rmVal)] <- if(length(reassign)==1 && isTRUE(reassign)) max(as.numeric(names(tab))) -sum(endOrph) else NA} }
  ind }

