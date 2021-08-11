#' System-data (compressed format)  
#'
#' This function returns current date (based on Sys.Date) in different format options.
#' 
#' @details   
#' Multiple options for fromatting exist :
#' 'univ1' ... (default) compact sytle using day, first 3 letters of English name of month and last 2 letters of year as ddmmmyy, eg 14jun21 
#' 
#' 'univ2' ... as ddMmmyy, eg 14Jun21
#' 
#' 'univ3' ... as ddmonthyy, eg 14june21
#' 
#' 'univ4' ... as ddMonthyy, eg 14June21
#' 
#' 'univ5' ... as yyyy-mm-dd (output of \code{Sys.Date()}), eg 2021-06-14
#' 
#' 'univ6' ... as yyyy-number of day (in year), eg 2021-165
#' 
#' 'local1' ... compact sytle using day, first 3 letters of current locale name of month (not necessarily unique !) and last 2 letters of year as ddmmmyy, eg 14jui21 
#' 
#' 'local2' ... as ddMmmyy, month based on current locale (not necessarily unique !), eg 14Jui21
#' 
#' 'local3' ... as ddmonthyy, month based on current locale , eg 14juin21
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
  if(length(style) <1) style <- "univ1"
  switch(style,
    local1=paste0(format(Sys.Date(),"%d"), substr(format(Sys.Date(),"%b"),1,3), substr(format(Sys.Date(),"%Y"),3,4)),  # ddmmyy
    local2=paste0(format(Sys.Date(),"%d"), toupper(substr(format(Sys.Date(),"%b"),1,1)), substr(format(Sys.Date(),"%b"),2,3), substr(format(Sys.Date(),"%Y"),3,4)),  # ddMmyy
    local3=paste0(format(Sys.Date(),"%d"), format(Sys.Date(),"%b"), substr(format(Sys.Date(),"%Y"),3,4)),              # ddmonthyy    
    local4=format(Sys.Date(), "%d%b%Y"),           # ddmonthyyyy
    local5=format(Sys.Date(),"%v"),               # separated by -, dd-month-yyyy
    local6=format(Sys.Date(), "%Y%b%d"),           # yyyymonthdd
    univ1=paste0(format(Sys.Date(),"%d"), tolower(month.abb[as.integer(format(Sys.Date(),"%m"))]), substr(format(Sys.Date(),"%Y"),3,4)),   # ddmmyy
    univ2=paste0(format(Sys.Date(),"%d"), month.abb[as.integer(format(Sys.Date(),"%m"))], substr(format(Sys.Date(),"%Y"),3,4)),             # ddMmyy 
    univ3=paste0(format(Sys.Date(),"%d"), tolower(month.name[as.integer(format(Sys.Date(),"%m"))]), substr(format(Sys.Date(),"%Y"),3,4)),  # ddmonthyy
    univ4=paste0(format(Sys.Date(),"%d"), tolower(month.name[as.integer(format(Sys.Date(),"%m"))]), substr(format(Sys.Date(),"%Y"),3,4)),  # ddmonthyy
    univ5=Sys.Date(),                                         #yyyy-mm-dd
    univ6=format(Sys.Date(), "%Y-%j") )
  if("wr" %in% style) {paste0(format(Sys.Date(),"%d"), month.abb[as.integer(format(Sys.Date(),"%m"))], substr(format(Sys.Date(),"%Y"),3,4))
  } else format(Sys.Date(), "%d%b%Y") } 
  
   