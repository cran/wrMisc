#' Convert ulr-name for reading in raw-mode
#' 
#' This functions converts a given urlName so that from data from git-hub can be read correctly that tabular data.
#' Thus, this will remove '/blob/' and change starting characters to 'raw.githubusercontent.com' 
#' 
#'            
#' 
#' @param urlName (charachter) main url-address
#' @param replTxt (NULL or matrix) adjust/ custom-modify search- and replacement items; should be matrix with 2 columns, 
#'  the 1st colimn entries will be used as 'search-for' and the 2nd as 'replace by' fro each row. 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return corrected urlName
#' @seealso  \code{\link[base]{sub}}; 
#' @examples
#' url1 <- paste0("https://github.com/bigbio/proteomics-metadata-standard/blob/",
#'   "master/annotated-projects/PXD001819/PXD001819.sdrf.tsv") 
#' gitDataUrl(url1)
#' 
#' 
#' 
#' @export
gitDataUrl <- function(urlName, replTxt=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## convert urlName from git that tabular data can be read correctly, ie remove '/blob/' & change start to 'raw.githubusercontent.com'
  fxNa <- .composeCallName(callFrom, newNa="gitDataUrl")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(urlName) <1) { if(!silent) message(fxNa,"'urlName' is empty, nothing to do")
  } else {
    if((any(length(dim(replTxt)) !=2, dim(replTxt) < 1:2))) replTxt <- rbind(
      c("^https://github.com/", "https://raw.githubusercontent.com/"),
      c("/blob/master/", "/master/"))
    chGit <- grepl(replTxt[1,1], urlName)
    if(any(chGit)) {
      for(i in 1:nrow(replTxt)) urlName <- sub(replTxt[i,1], replTxt[i,2], urlName)} 
  }
  urlName }   
       
