#' Convert GitHub Url-Name For Reading In Raw-Mode
#'
#' This functions converts a given urlName so that (tabular) data from GitHub (https://github.com/) can data be read correctly as tabular data.
#' 
#' @details
#'   
#' Thus, this will exchange the beginning of the site to 'raw.githubusercontent.com' and then exchange 'blob' to 'refs/head' .
#' Alternatively, the user may custom define all terms to get exchanged (as matrix with 2 columns 'old' and 'new').          
#' 
#' Note : The default spearator from  \code{\link[utils]{read.delim}} may suit better for reading the data.
#'  
#' @param urlName (charachter) main url-address
#' @param replTxt (NULL or matrix) adjust/ custom-modify search- and replacement items; default settings will exchange the beginning of the site to 'raw.githubusercontent.com' and then exchange 'blob' to 'refs/head'
#'  otherwise should be matrix with 2 columns, the 1st colimn entries will be used as 'search-for' and the 2nd as 'replace by' for each row 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return corrected urlName
#' @seealso  \code{\link[base]{sub}}; \code{\link[utils]{read.delim}}
#' @examples
#' url1 <- paste0("https://github.com/bigbio/proteomics-metadata-standard/blob/",
#'   "main/datasets/PXD001819/PXD001819.sdrf.tsv") 
#' rbind(ini=url1, new=gitDataUrl(url1))
#' 
#' @export
gitDataUrl <- function(urlName, replTxt=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## convert urlName from git that tabular data can be read correctly, ie remove '/blob/' & change start to 'raw.githubusercontent.com'
  fxNa <- .composeCallName(callFrom, newNa="gitDataUrl")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(urlName) <1) { if(!silent) message(fxNa,"'urlName' is empty, nothing to do")
  } else {
    if(length(replTxt) ==0 || (any(length(dim(replTxt)) !=2, dim(replTxt) < 1:2))) replTxt <- rbind(
      c("^https://github.com/", "https://raw.githubusercontent.com/"),
      c("/blob/", "/refs/heads/"))
    for(i in 1:nrow(replTxt)) {
      ch1 <- grepl(replTxt[i,1], urlName)
      if(isTRUE(ch1)) urlName <- sub(replTxt[i,1], replTxt[i,2], urlName)
    }
  }
  urlName }   
       
