#' Simple Package Download Statistics from CRAN
#'
#' This function allows accessing the most recent counts of package downloads availabale on http://www.datasciencemeta.com/rpackages, 
#' obtaining rank quantiles and to compare (multiple) given packages to the bulk data, optionally a plot can be drawn.
#' 
#' @details
#' The task of checking the number of downloads for a given package has been addressed by several packages (eg dlstats, cranlogs, adjustedcranlogs).
#' Detailed articles on this subject have been published on R-Hub (https://blog.r-hub.io/2020/05/11/packagerank-intro/) and on 
#' R-bloggers (https://www.r-bloggers.com/2020/10/a-cran-downloads-experiment/).
#' 
#' This function only allows accessing the most recent counts as listed on the website of www.datasciencemeta.com .  
#' Please note, that reading all lines from the website may take a few seconds.
#' To get a better understanding of the counts read, reference quantiles for download-counts get added by default  (see argument \code{refQuant}). 
#' The (optional) figure can be drawn in linear scale (default, with minor zoom to lower number of counts) or in log (necessary for proper display of the entire range of counts), by setting the argument \code{log="y"}. 
#' 
#' The number of downloads counted by RStudio may not be a perfect measure for the actual usage/popularity of a given package,   
#' the articles cited above discuss this in more detail.
#' For example, multiple downloads from the same IP or subsequent downloads of multiple (older) versions of the same package are counted, too. 
#' 
#' @param queryPackages (character or integer) package names of interest, if \code{integer}, n random packages will be picked by random
#' @param countUrl (character) the url where the dayly counts ara available
#' @param refQuant (numeric) add reference quantile values to output matrix 
#' @param figure (logical) decide of figure should be printed
#' @param log (character) set count-axis of figure to linear or log-scale (by setting \code{log="y"})
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function retuns a matrix with download counts (or \code{NULL} if the web-site can't be accessed or the query-packages are not found there)
#' @seealso packages \href{https://CRAN.R-project.org/package=cranlogs}{cranlogs} and \href{https://CRAN.R-project.org/package=packageRank}{packageRank}
#' @examples
#' ## Let's try a microscopic test-file (NOT representative for true up to date counts !!)
#' pack1 <- c("cif", "bcv", "FinCovRegularization", "wrMisc", "wrProteo")
#' testFi <- file.path(system.file("extdata", package="wrMisc"), "rpackagesMicro.html")
#' packageDownloadStat(pack1, countUrl=testFi, log="y", figure=FALSE)
#' ## For real online counting simply drop the argument countUrl
#' 
#' @export
packageDownloadStat <- function(queryPackages=c("wrMisc","wrProteo","cif","bcv","FinCovRegularization"), countUrl="http://www.datasciencemeta.com/rpackages", 
  refQuant=(1:10)/10, figure=TRUE, log="", silent=FALSE, callFrom=NULL) {
  ## get rank & downloads for all 10% tiles as well as queryPackages
  ## return matrix with 1st line as rank and 2nd as n.dowloads, if inclQuant=TRUE 12th col - end for queryPackages
  fxNa <- .composeCallName(callFrom, newNa="packageDownloadStat")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  inclQuant <- length(refQuant) >0 
  chPa <- length(naOmit(queryPackages)) >0 & (is.character(queryPackages) | is.numeric(queryPackages))
  datOK <- TRUE
  if(chPa) {
    txt <- try(readLines(countUrl, warn=FALSE), silent=TRUE)
    if(!inherits(txt, "try-error")) {
      ## check argument refQuant
      if(any(length(refQuant) < 1, identical(refQuant,NA), isFALSE(refQuant))) {refQuant <- 1; inclQuant <- FALSE}        # minimal setting
      if(inclQuant & length(txt) <900) { inclQuant <- FALSE
        if(!silent) message(fxNa,"Too few data for adding reference centile values/packages")
      }
      if(!is.numeric(refQuant)) refQuant <- (1:10)/10
      chNa <- is.na(refQuant)
      if(any(chNa)) { if(all(chNa)) { refQuant <- (1:10)/10
        } else {
          refQuant <- refQuant[which(!chNa)]
          if(!silent) message(fxNa,"Removing ",sum(refQuant)," invalid reference quantile entries (must be 0 > x >= 1)") }
      } 
      if(debug) {message("rd1"); rd1 <- list(txt=txt,queryPackages=queryPackages,chPa=chPa,chNa=chNa)}
      ## start parsing page
      iniIn <- grep("<tbody>", txt)[1] +2            # position 62 +2
      endIn <- grep("</tbody>", txt)[1] -1              # position  152112 -1
      tab <- matrix(txt[iniIn:endIn], byrow=TRUE, ncol=8)[,2:4]
      paNa <- sub("[[:print:]]+https://cran.r-project.org/web/packages/", "", sub("/index.html.+", "", tab[,2]))
      nDownl <- try(as.integer(gsub(",", "", sub(" +<td>", "", sub("</td> *", "", tab[,3])))), silent=TRUE)
      paRa <- try(as.integer(sub(" +<td>", "", sub("</td> *", "", tab[,1]))), silent=TRUE)
      if(any(sapply(list(nDownl,paRa), inherits, "try-error"), length(paNa) !=length(nDownl))) datOK <- FALSE
    } } else if(!silent) message(fxNa,"Invalid 'queryPackages'")
  if(datOK) {
    ## assemble; verify 'queryPackages'
    ta2 <- data.frame(name=paNa, rank=paRa, downloads=nDownl)
    ta2[order(ta2$rank),]   # order (just in case)
    if(debug) {message("rd2"); rd2 <- list(txt=txt,tab=tab,queryPackages=queryPackages,chPa=chPa,chNa=chNa,iniIn=iniIn,endIn=endIn, paNa=paNa, nDownl=nDownl,paRa=paRa,datOK=datOK)}


    if(is.numeric(queryPackages)) queryPackages <- paNa[sample(1:nrow(tab), as.integer(abs(queryPackages[1])), replace=FALSE)]   # random pick if numeric
    ## search query
    chPa <- match(queryPackages, ta2$name)
    if(any(is.na(chPa))) { if(all(is.na(chPa))) { warning(fxNa,"NONE of queryPackages found; returning NULL"); datOK <- FALSE
      } else {queryPackages <- naOmit(queryPackages) }}
    if(debug) {message("rd3")}
  } else if(!silent) message(fxNa,"Unable to understand information from '",countUrl,"'")
  
  if(datOK) {
    ##
    maxPa <- max(ta2$rank, na.rm=TRUE)
    out <- ta2[chPa,]
    out$centile <- round(100*out$rank/maxPa,1)
    if(debug) {message("rd4")}
  
    if(isTRUE(figure)){
      if(!identical(log,"y")) log <- ""
      yMa <- min(ta2$downloads[round(nrow(ta2)*0.05)], ta2$downloads[1]*0.15)
      suppressWarnings(graphics::plot(downloads ~rank, ta2, las=2, 
        ylim=if(identical(log,"y")) c(100,nDownl[1]) else c(1,yMa), type="l", cex.axis=0.8, 
        xaxs="i", yaxs="i", xlab="Rank",ylab="Number of Downloads", main="CRAN Packages Downloads", log=log))
      graphics::abline(v=pretty(c(rep(100,5), maxPa)), col="grey75", lty=2)
      graphics::abline(h=if(identical(log,"y")) 10^(1:7) else 1:5*20000, col="grey75", lty=2)
       graphics::points(ta2$rank[chPa], ta2$downloads[chPa], col=2)

      arrUp <- if(identical(log,"y")) ta2$downloads[chPa]*2.2 else yMa*0.07 
      arrBo <- if(identical(log,"y")) ta2$downloads[chPa]*1.04 else yMa*0.01 
      graphics::arrows(x0=ta2$rank[chPa], y0=arrUp, x1=ta2$rank[chPa], y1=arrBo, col=2, length=0.11, lwd=1.8)   # ok
      graphics::text(ta2$rank[chPa], arrUp*1.03, labels=ta2$name[chPa], col=2,las=1,cex=1,srt=90,adj=0)

      date1 <- sub(" .+","",sub(".+Last updated: ","",txt[grep("Last updated",txt)]))
      graphics::mtext(paste("as of ",date1," "), cex=0.9, adj=1)
    }

    if(debug) {message("rd5"); rd5 <- list(out=out,txt=txt,tab=tab,ta2=ta2,queryPackages=queryPackages,chPa=chPa,chNa=chNa,iniIn=iniIn,endIn=endIn, paNa=paNa, nDownl=nDownl,paRa=paRa,datOK=datOK)}

    if(inclQuant) {
      ## add quantile (reference-)values
      if(nrow(ta2) <1000) warning(fxNa,"Too few data ! Very imprecise estimation of reference centile ...")
      refQuan2 <- round(maxPa * refQuant)                               
      refInd <- round(nrow(ta2) *refQuant) 
      supl <- ta2$downloads[refInd] 
      names(supl) <- paste0("cent_",100*round(refQuant,3))
      out <- rbind(out, data.frame(name=ta2$name[refInd], rank=ta2$rank[refInd], downloads=supl, centile=100*round(refQuant,3)))
      out <- out[order(out$rank, decreasing=FALSE),] }
    out
}}
    