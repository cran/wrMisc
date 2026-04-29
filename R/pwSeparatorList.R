#' Suggestions Of Collections Of Classical Separators For Pairwise Combinations
#'
#' @description
#' This function provides collecions of typical separators used for combined names in pair-wise testing for later choice of the most suited.
#' 
#' @details 
#' Depending if one rather looks for separators for cutting existing names of for suggesting separators for fresh concatenation, two differentsets of collections are available via the argument \code{type}.
#'
#' Using \code{type="split"} will default to \code{type="split1"}, similarly \code{type="combine"} will be used as \code{type="combine1"}.
#'   
#' The option \code{type="split1"} has a default collection of shorter/compact separators for splitting
#' The option \code{type="split2"} allows more extensive search for 38 separators (ie combinations with ' ','-' and '_'), but extensive checking may cost CPU time.
#' The option \code{type="split3"} has a shorter collection of 12 basic commonly used separators.
#' The option \code{type="split4"} to  \code{type="split9"} default to 'split1'
#' 
#' The option \code{type="combine1"} does not suggest '--' (neither '__') since if '-' may not be chosen  ins the first place, using '--' as separator next to '-' occuring inside gourp-names may not enhance visibility.
#' The option \code{type="combine2"} contains a collection of sperators using 'vs' or 'versus'
#' The option \code{type="combine3"} is designed for explicite terms (starts with ' - versus - ', for suggesting compact terms rather use \code{type="combine1"}
#' The option \code{type="combine4"} to  \code{type="combine9"} default to 'combine1'
#' 
#' 
#' @param type (character) allows selecting of different types of collections of separators
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a charcter vector with proposed separators for later choosing the most suited
#' @seealso (for running multiple pair-wise test) \code{\link{moderTestXgrp}}, \code{\link{convPairwiseSetup}}, \code{\link{presenceFilt}}, \code{\link{getPWseparator}}, \code{\link{indexGroupsFromPW}}, 
#'   (used underneith:) \code{\link[base]{grep}}, \code{\link[base]{strsplit}}
#' @examples
#' pwSeparatorList(type="split1")
#' @export
pwSeparatorList <- function(type="split1", silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="pwSeparatorList")
  if(isTRUE(debug)) message(fxNa,"With type='",type,"'")
  if(length(type)==0 || any(is.na(type))) type <- "combine1" else type <- type[1]
  if("combine" %in% type) type <- "combine1"
  if("split" %in% type) type <- "split1"
  if(grepl("^split[5-9]$", type)) type <- "split1"
  if(grepl("^combine[5-9]$", type)) type <- "combine1"
  splS <- rep(c("-","_"," ",""), each=4)     # .. used in split2
  splX <- paste0(splS, c("compare","compared"), splS, c("to","to","To","To") )  # .. used in split2
  splY <- paste0(splS, c("vs","versus","to","VERSUS"), splS )                   # .. used in split2 
  switch(type,
    combine1 =c("-","---","_","___",".","-vs-","_vs_","---vs---"," - vs - ","___vs___","  ","--","__"),
    combine2 =c("-vs-","_vs_"," -vs- ","---vs---","___vs___"," - versus - "),
    combine3 =c("  /  "," vs ","-vs-"," - vs - "," - versus - ","___versus__","---vs---","___vs___"),  # for explicit
    combine4 =c("-","---","_","___",".","-vs-","_vs_","---vs-_-","___vs___"," ","  ","--","__"),       # backup 
    split1 =c("-","--","---","_","__","___","-vs-","_vs_"," - vs - ","--vs--","__vs__","---vs---","___vs___","-versus-","_versus_"," versus "," - versus - ","___versus__","-compareTo-","_compareTo_"),
    split2 =unique(c("-","--","---","_","__","___",splY, splX, "__vs__","--vs--"," - vs - ","___vs___","---vs---"," - versus - ","___versus__","   ","  ")),
    split3 =c("-","--","_","__"," - "," -- ","-vs-","_vs_","--vs--","__vs__"," ","  "),
    split4 =c("-","---","_","___",".","-vs-","_vs_","---vs-_-","___vs___"," ","  ","--","__")    # backup 
  )
}

  
