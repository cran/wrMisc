#' System-data (compressed format)  
#'
#' This function returns current date (based on Sys.Date) as day/month/year, where month is abbrev to 3 leters and only the mast two dgits of the year are shown. 
#' 
#' 
#' @param style (character) choose style (if \code{wr} the month will be abbreviated to fisrt 3 letters and last two digits of year will be shown  
#' @return character vector with formatted date
#'
#' @seealso \code{\link[base]{date}}, \code{Sys.Date} and \code{\link[base]{Sys.time}},
#' @examples
#' sysDate() 
#' 
#' @export
sysDate <- function(style="wr") {
  ## return date in compressed format  ('wr-style') day/month/year eg 15sep20
  if(length(style) <1) style <- ""
  if("wr" %in% style) {paste0(format(Sys.Date(),"%d"), substr(format(Sys.Date(),"%b"),1,3), substr(format(Sys.Date(),"%Y"),3,4))
  } else format(Sys.Date(), "%d%b%Y") } 
  
   