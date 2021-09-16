#' System-date (compressed format)  
#'
#' This function returns current date (based on Sys.Date) in different format options.
#' 
#' @details   
#' Multiple options for formatting exist :
#' 'univ1' or 'wr' ... (default) compact sytle using day, first 3 letters of English name of month (lowercaps) and last 2 letters of year as ddmmmyy, eg 14jun21 
#' 
#' 'univ2' ... as ddMmmyy, eg 14Jun21
#' 
#' 'univ3' ... as ddMonthyyyy, eg 14June2021
#' 
#' 'univ4' ... as ddmonthyyyy, eg 14june2021
#' 
#' 'univ5' ... as yyyy-mm-dd (output of \code{Sys.Date()}), eg 2021-06-14
#' 
#' 'univ6' ... as yyyy-number of day (in year), eg 2021-165
#' 
#' 'local1' ... compact sytle using day, first 3 letters of current locale name of month (not necessarily unique !) and last 2 letters of year as ddmmmyy, eg 14jui21 
#' 
#' 'local2' ... as ddMmmyy, month based on current locale (not necessarily unique !), eg 14Jui21
#' 
#' 'local3' ... as ddMonthyyyy, month based on current locale , eg 14Juin2021
#' 
#' 'local4' ... as ddmonthyyyy, month based on current locale , eg 14juin2021
#'
#' 'local5' ... as dd-month-yyyy, month based on current locale , eg 14-juin-2021
#'
#' 'local6' ... as yyyymonthddd, month based on current locale , eg 2021juin14
#'
#' 
#' 
#' @param style (character) choose style (default 'univ1' for very compact style)  
#' @return character vector with formatted date
#'
#' @seealso \code{\link[base]{date}}, \code{Sys.Date} and \code{\link[base]{Sys.time}},
#' @examples
#' sysDate() 
#' 
#' @export
sysDate <- function(style="univ1") {
  ## return date in compressed format  ('wr-style') day/month/year eg 15sep20
  if(length(style) <1) style <- "univ1" else if(any(is.na(style))) style <- "univ1"
  if("wr" %in% style) style <- "univ1"
  out <- switch(style,
    abc="abc",
    local1=paste0(format(Sys.Date(),"%d"), substr(format(Sys.Date(),"%b"),1,3), substr(format(Sys.Date(),"%Y"),3,4)),  # ddmmmyy
    local2=paste0(format(Sys.Date(),"%d"), gsub("(^[[:alpha:]])", "\\U\\1", substr(format(Sys.Date(),"%b"),1,3), perl=TRUE), substr(format(Sys.Date(),"%Y"),3,4)),  # ddMmmyy
    local3=paste0(format(Sys.Date(),"%d"), gsub("(^[[:alpha:]])", "\\U\\1", format(Sys.Date(),"%b"), perl=TRUE), format(Sys.Date(),"%Y")),           # ddMonthyy
    local4=format(Sys.Date(), "%d%b%Y"),           # ddmonthyyyy
    local5=format(Sys.Date(),"%v"),                # separated by -, dd-month-yyyy
    local6=format(Sys.Date(), "%Y%b%d"),           # yyyymonthdd
    univ1=paste0(format(Sys.Date(),"%d"), tolower(month.abb[as.integer(format(Sys.Date(),"%m"))]), substr(format(Sys.Date(),"%Y"),3,4)),   # ddmmmyy
    univ2=paste0(format(Sys.Date(),"%d"), month.abb[as.integer(format(Sys.Date(),"%m"))], substr(format(Sys.Date(),"%Y"),3,4)),            # ddMmmyy 
    univ3=paste0(format(Sys.Date(),"%d"), month.name[as.integer(format(Sys.Date(),"%m"))], format(Sys.Date(),"%Y")),  # ddmonthyyyy
    univ4=paste0(format(Sys.Date(),"%d"), tolower(month.name[as.integer(format(Sys.Date(),"%m"))]), format(Sys.Date(),"%Y")),  # ddmonthyyyy
    univ5=Sys.Date(),                                         #yyyy-mm-dd
    univ6=format(Sys.Date(), "%Y-%j") )
  out }  
    
