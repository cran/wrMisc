## ----setup, include=FALSE, echo=FALSE, messages=FALSE, warnings=FALSE---------
suppressPackageStartupMessages({
    library(wrMisc)
})

## ----install, echo=TRUE, eval=FALSE-------------------------------------------
#  ## If not already installed, you'll have to install the package first.
#  ## This is the basic installation commande in R
#  install.packages("wrMisc")

## ----install2, echo=TRUE, eval=FALSE------------------------------------------
#  packages <- c("knitr", "rmarkdown", "BiocManager", "kableExtra", "boot", "data.tree", "data.table", "fdrtool",
#    "RColorBrewer", "Rcpp", "wrMisc", "wrGraph", "wrProteo")
#  checkInstallPkg <- function(pkg) {       # install function
#    if(!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg) }
#  
#  ## install if not yet present
#  sapply(packages, checkInstallPkg)

## ----install3, echo=TRUE, eval=FALSE------------------------------------------
#  ## Installation of limma
#  BiocManager::install("limma")

## ----install4, echo=TRUE, eval=FALSE------------------------------------------
#  ## Now you can open this vignette out of R:
#  vignette("wrMiscVignette1", package="wrMisc")

## ----setup1-------------------------------------------------------------------
library("wrMisc")
library("knitr")

## This is 'wrMisc' version number :
packageVersion("wrMisc")

## ----basicVariability, echo=TRUE----------------------------------------------
grp1 <- rep(LETTERS[1:3], c(3,4,3))
sampNa1 <- paste0(grp1, c(1:3,1:4,1:3))
set.seed(2016); dat1 <- matrix(round(c(runif(50000) +rep(1:1000,50)),3), 
  ncol=10, dimnames=list(NULL,sampNa1))
dim(dat1)
head(dat1)

## ----sdForEachRow, echo=TRUE--------------------------------------------------
head(rowSds(dat1))
system.time(sd1 <- rowSds(dat1))
system.time(sd2 <- apply(dat1,1,sd))

## ----usingApply, echo=TRUE----------------------------------------------------
table(round(sd1,13)==round(sd2,13))

## ----calculateRowCV, echo=TRUE------------------------------------------------
system.time(cv1 <- rowCVs(dat1))
system.time(cv2 <- apply(dat1,1,sd)/rowMeans(dat1))
# typically the calculation using rowCVs is much faster
head(cv1)
# results from the 'conventional' way
head(cv2)

## ----rowGrpMeans1, echo=TRUE--------------------------------------------------
# we already defined the grouping :
grp1

## the mean for each group and row
system.time(mean1Gr <- rowGrpMeans(dat1, grp1))

## ----sdOrCVbyGrp, echo=TRUE---------------------------------------------------
## Now the sd for each row and group
system.time(sd1Gr <- rowGrpSds(dat1, grp1))
# will give us a matrix with the sd for each group & line 
head(sd1Gr)

# Let's check the results of the first line :
sd1Gr[1,] == c(sd(dat1[1,1:3]), sd(dat1[1,4:7]), sd(dat1[1,8:10]))

# The CV :
system.time(cv1Gr <- rowGrpCV(dat1, grp1))
head(cv1Gr)

## ----rowGrpNA1, echo=TRUE-----------------------------------------------------
mat2 <- c(22.2, 22.5, 22.2, 22.2, 21.5, 22.0, 22.1, 21.7, 21.5, 22, 22.2, 22.7,
   NA, NA, NA, NA, NA, NA, NA, 21.2,   NA, NA, NA, NA,
   NA, 22.6, 23.2, 23.2,  22.4, 22.8, 22.8, NA,  23.3, 23.2, NA, 23.7,
   NA, 23.0, 23.1, 23.0,  23.2, 23.2, NA, 23.3,  NA, NA, 23.3, 23.8)
mat2 <- matrix(mat2, ncol=12, byrow=TRUE)
## The definition of the groups (ie replicates)
gr4 <- gl(3, 4, labels=LETTERS[1:3])

## ----rowGrpNA2, echo=TRUE-----------------------------------------------------
rowGrpNA(mat2,gr4)

## ----naOmit, echo=TRUE--------------------------------------------------------
aA <- c(11:13,NA,10,NA)
 
str(naOmit(aA))

# the 'classical' na.omit also stores which elements were NA
str(na.omit(aA))

## ----minDiff, echo=TRUE-------------------------------------------------------
set.seed(2017); aa <- 10*c(0.1 +round(runif(20),2), 0.53, 0.53)
head(aa)

minDiff(aa,ppm=FALSE)

## ----partUnlist_1, echo=TRUE--------------------------------------------------
bb <- list(fa=gl(2,2), ve=31:33, L2=matrix(21:28,ncol=2), li=list(li1=11:14,li2=data.frame(41:44)))
partUnlist(bb)
partUnlist(lapply(bb,.asDF2))

## ----unlist_1, echo=TRUE------------------------------------------------------
head(unlist(bb, recursive=FALSE))

## ----asSepList, echo=TRUE-----------------------------------------------------
bb <- list(fa=gl(2,2), ve=31:33, L2=matrix(21:28,ncol=2), li=list(li1=11:14,li2=data.frame(41:44)))
asSepList(bb)

## ----lappend1, echo=TRUE------------------------------------------------------
li1 <- list(a=1, b=2, c=3)
li2 <- list(A=11, b=2, C=13)
append(li1, li2)

## ----lappend2, echo=TRUE------------------------------------------------------
appendNR(li1, li2)

## ----lrbind, echo=TRUE--------------------------------------------------------
dat2 <- matrix(11:34, ncol=3, dimnames=list(letters[1:8],colnames=LETTERS[1:3]))
lst2 <- by(dat2, rep(1:3,c(3,2,3)), as.matrix)
lst2

# join list-elements (back) into single matrix
lrbind(lst2)

## ----fuseCommonListElem, echo=TRUE--------------------------------------------
val1 <- 10 +1:26
names(val1) <- letters
(lst1 <- list(c=val1[3:6], a=val1[1:3], b=val1[2:3] ,a=val1[12], c=val1[13]))

## here the names 'a' and 'c' appear twice :
names(lst1)

## now, let's fuse all 'a' and 'c'
fuseCommonListElem(lst1)

## ----listBatchReplace1, echo=TRUE---------------------------------------------
lst1 <- list(m1=matrix(11:18,ncol=2), m2=matrix(21:30,ncol=2), indR=31:34, m3=matrix(c(21:23,NA,25:27,NA),ncol=2))
filterLiColDeList(lst1, useLines=2:3)
filterLiColDeList(lst1, useLines="allNA", ref=3)

## ----replInList1, echo=TRUE---------------------------------------------------
(lst1 <- list(aa=1:4, bb=c("abc","efg","abhh","effge") ,cc=c("abdc","efg","efgh")))
listBatchReplace(lst1,search="efg",repl="EFG",silent=FALSE)

## ----listGroupsByNames, echo=TRUE---------------------------------------------
ser1 <- 1:7; names(ser1) <- c("AA","BB","AA.1","CC","AA.b","BB.e","A")

listGroupsByNames(ser1)

## ----listGroupsByNames2, echo=TRUE--------------------------------------------
listGroupsByNames((1:10)/5)

## ----filterList, echo=TRUE----------------------------------------------------
set.seed(2020); dat1 <- round(runif(80),2)
list1 <- list(m1=matrix(dat1[1:40], ncol=8), m2=matrix(dat1[41:80], ncol=8), other=letters[1:8])
rownames(list1$m1) <- rownames(list1$m2) <- paste0("line",1:5)
# Note: the list-element list1$other has a length different to that of filt. Thus, it won't get filtered.
filterList(list1, list1$m1[,1] >0.4)       # filter according to 1st column of $m1 ...
filterList(list1, list1$m1 >0.4) 

## ----matr2list, echo=TRUE-----------------------------------------------------
(mat1 <- matrix(1:12, ncol=3, dimnames=list(letters[1:4],LETTERS[1:3])))
str(matr2list(mat1))

## ----array0, echo=TRUE--------------------------------------------------------
(arr1 <- array(c(6:4,4:24), dim=c(4,3,2), dimnames=list(c(LETTERS[1:4]),
  paste("col",1:3,sep=""),c("ch1","ch2"))))

## ----arrayCV1, echo=TRUE------------------------------------------------------
arrayCV(arr1)

# this is equivalent to
cbind(rowCVs(arr1[,,1]), rowCVs(arr1[,,2]))

## ----arrayCV2, echo=TRUE------------------------------------------------------
arrayCV(arr1, byDim=2)

## ----cutArrayInCluLike, echo=TRUE---------------------------------------------
cutArrayInCluLike(arr1, cluOrg=c(2,1,2,1))

## ----filt3dimArr, echo=TRUE---------------------------------------------------
filt3dimArr(arr1,displCrit=c("col1","col2"), filtCrit="col2", filtVal=7, filtTy=">")

## ----repeated1, echo=TRUE-----------------------------------------------------
## some text toy data
tr <- c("li0","n",NA,NA,rep(c("li2","li3"),2),rep("n",4))

## ----repeated2, echo=TRUE-----------------------------------------------------
table(tr)
unique(tr) 
duplicated(tr, fromLast=FALSE)

## ----repeated3, echo=TRUE-----------------------------------------------------
aa <- c(11:16,NA,14:12,NA,14)
names(aa) <- letters[1:length(aa)]
aa

## ----findRepeated, echo=TRUE--------------------------------------------------
findRepeated(aa) 

## ----firstOfRepeated, echo=TRUE-----------------------------------------------
firstOfRepeated(aa)

aa[firstOfRepeated(aa)$indUniq]          # only unique with their names

unique(aa)                               # unique() does not return any names !

## ----correctToUnique1, echo=TRUE----------------------------------------------
correctToUnique(aa)

correctToUnique(aa, sep=".", NAenu=FALSE)       # keep NAs (ie without transforming to character)

## ----nonAmbiguousNum, echo=TRUE-----------------------------------------------
unique(aa)                                    # names are lost

nonAmbiguousNum(aa)
nonAmbiguousNum(aa, uniq=FALSE, asLi=TRUE)    # separate in list unique and repeated 

## ----cbindNR, echo=TRUE-------------------------------------------------------
## First we'll make soe toy data :
(ma1 <- matrix(1:6, ncol=3, dimnames=list(1:2,LETTERS[3:1])))
(ma2 <- matrix(11:16, ncol=3, dimnames=list(1:2,LETTERS[3:5])))

## now we can join 2 or more matrixes  
cbindNR(ma1, ma2, summarizeAs="mean")       # average of both columns 'C'

## ----firstLineOfDat, echo=TRUE------------------------------------------------
(mat1 <- matrix(c(1:6,rep(1:3,1:3)), ncol=2, dimnames=list(letters[1:6],LETTERS[1:2])))

## ----firstLineOfDat2, echo=TRUE-----------------------------------------------
firstLineOfDat(mat1, refCol=2)

## ----firstOfRepLines, echo=TRUE-----------------------------------------------
mat2 <- matrix(c("e","n","a","n","z","z","n","z","z","b", 
  "","n","c","n","","","n","","","z"), ncol=2)
firstOfRepLines(mat2, out="conc")

# or as index :
firstOfRepLines(mat2)

## ----nonredDataFrame, echo=TRUE-----------------------------------------------
(df1 <- data.frame(cbind(xA=letters[1:5], xB=c("h","h","f","e","f"), xC=LETTERS[1:5])))

## ----nonredDataFrame2, echo=TRUE----------------------------------------------

nonredDataFrame(df1, useCol=c("xB","xC")) 

# without counter or concatenating
df1[which(!duplicated(df1[,2])),]
# or
df1[firstOfRepLines(df1,useCol=2),]

## ----get1stOfRepeatedByCol, echo=TRUE-----------------------------------------
mat2 <- cbind(no=as.character(1:20), seq=sample(LETTERS[1:15], 20, repl=TRUE),
  ty=sample(c("full","Nter","inter"),20,repl=TRUE), ambig=rep(NA,20), seqNa=1:20)
(mat2uniq <- get1stOfRepeatedByCol(mat2, sortBy="seq", sortSupl="ty"))

# the values from column 'seq' are indeed unique
table(mat2uniq[,"seq"])

# This will return all first repeated (may be >1) but without furter sorting 
#  along column 'ty' neither marking in comumn 'ambig').
mat2[which(duplicated(mat2[,2],fromLast=FALSE)),]

## ----nonAmbiguousMat, echo=TRUE-----------------------------------------------
nonAmbiguousMat(mat1,by=2)

## ----nonAmbiguousMat2, echo=TRUE----------------------------------------------
set.seed(2017); mat3 <- matrix(c(1:100,round(rnorm(200),2)), ncol=3,
  dimnames=list(1:100,LETTERS[1:3]));
head(mat3U <- nonAmbiguousMat(mat3, by="B", na="_", uniqO=FALSE), n=15)
head(get1stOfRepeatedByCol(mat3, sortB="B", sortS="B"))

## ----combineReplFromListToMatr, echo=TRUE-------------------------------------
lst2 <- list(aa_1x=matrix(1:12, nrow=4, byrow=TRUE), ab_2x=matrix(24:13, nrow=4, byrow=TRUE))
combineReplFromListToMatr(lst2)

## ----nonRedundLines, echo=TRUE------------------------------------------------
mat4 <- matrix(rep(c(1,1:3,3,1),2), ncol=2, dimnames=list(letters[1:6],LETTERS[1:2]))
nonRedundLines(mat4)

## ----filtSizeUniq, echo=TRUE--------------------------------------------------
# input: c and dd are repeated  :
filtSizeUniq(list(A="a", B=c("b","bb","c"), D=c("dd","d","ddd","c")), filtUn=TRUE, minSi=NULL)

# here a,b,c and dd are repeated  :
filtSizeUniq(list(A="a", B=c("b","bb","c"), D=c("dd","d","ddd","c")), ref=c(letters[c(1:26,1:3)],
  "dd","dd","bb","ddd"), filtUn=TRUE, minSi=NULL)   

## ----makeNRedMatr, echo=TRUE--------------------------------------------------
t3 <- data.frame(ref=rep(11:15,3),tx=letters[1:15],
  matrix(round(runif(30,-3,2),1),nc=2), stringsAsFactors=FALSE)
  
# First we split the data.frame in list  
by(t3,t3[,1],function(x) x)
t(sapply(by(t3,t3[,1],function(x) x), summarizeCols, me="maxAbsOfRef"))
(xt3 <- makeNRedMatr(t3, summ="mean", iniID="ref"))
(xt3 <- makeNRedMatr(t3, summ=unlist(list(X1="maxAbsOfRef")), iniID="ref"))

## ----combineRedBasedOnCol, echo=TRUE------------------------------------------
matr <- matrix(c(letters[1:6],"h","h","f","e",LETTERS[1:5]), ncol=3,
  dimnames=list(letters[11:15],c("xA","xB","xC")))
combineRedBasedOnCol(matr, colN="xB")
combineRedBasedOnCol(rbind(matr[1,],matr), colN="xB")

## ----convMatr2df, echo=TRUE---------------------------------------------------
x <- 1
dat1 <- matrix(1:10,ncol=2)
rownames(dat1) <- letters[c(1:3,2,5)]
## as.data.frame(dat1)  ...  would result in an error
convMatr2df(dat1)
convMatr2df(data.frame(a=as.character((1:3)/2), b=LETTERS[1:3], c=1:3))
tmp <- data.frame(a=as.character((1:3)/2), b=LETTERS[1:3],c=1:3, stringsAsFactors=FALSE)
convMatr2df(tmp)
tmp <- data.frame(a=as.character((1:3)/2), b=1:3, stringsAsFactors=FALSE)
convMatr2df(tmp) 

## ----combineOverlapInfo, echo=TRUE--------------------------------------------
set.seed(2013)
datT2 <- matrix(round(rnorm(200)+3,1), ncol=2, dimnames=list(paste("li",1:100,sep=""),
  letters[23:24]))
# (mimick) some short and longer names for each line
inf2 <- cbind(sh=paste(rep(letters[1:4],each=26),rep(letters,4),1:(26*4),sep=""),
  lo=paste(rep(LETTERS[1:4],each=26), rep(LETTERS,4), 1:(26*4), ",", 
  rep(letters[sample.int(26)],4), rep(letters[sample.int(26)],4), sep=""))[1:100,] 
## We'll use this to test :  
head(datT2,n=10)
## let's assign to each pair of x & y values a 'cluster' (column _clu_, the column _combInf_ tells us which lines/indexes are in this cluster)
head(combineOverlapInfo(datT2, disThr=0.03), n=10)
## it is also possible to rather display names (eg gene or protein-names) instead of index values
head(combineOverlapInfo(datT2, suplI=inf2[,2], disThr=0.03), n=10)

## ----getValuesByUnique, echo=TRUE---------------------------------------------
dat <- 11:19
names(dat) <- letters[c(6:3,2:4,8,3)]
## Here the names are not unique.
## Thus, the values can be binned by their (non-unique) names and a representative values calculated.

## Let's make a 'datUniq' with the mean of each group of values :
datUniq <- round(tapply(dat, names(dat),mean),1)
## now we propagate the mean values to the full vector 
getValuesByUnique(dat, datUniq)
cbind(ini=dat,firstOfRep=getValuesByUnique(dat, datUniq),
  indexUniq=getValuesByUnique(dat, datUniq,asIn=TRUE))

## ----combineByEitherFactor, echo=TRUE-----------------------------------------
nn <- rep(c("a","e","b","c","d","g","f"),c(3,1,2,2,1,2,1))
qq <- rep(c("m","n","p","o","q"),c(2,1,1,4,4))
nq <- cbind(nn,qq)[c(4,2,9,11,6,10,7,3,5,1,12,8),]
## Here we consider 2 columns 'nn' and 'qq' whe trying to regroup common values
##  (eg value 'a' from column 'nn' and value 'o' from 'qq') 
combineByEitherFactor(nq,1,2,nBy=FALSE)

## ----combineByEitherFactor2, echo=TRUE----------------------------------------
## the same, but including n by group/cluster
combineByEitherFactor(nq,1,2,nBy=TRUE)
## Not running further iterations works faster, but you may not reach 'convergence' immediately
combineByEitherFactor(nq,1,2,nBy=FALSE)

## ----combineByEitherFactor3, echo=TRUE----------------------------------------
##  another example
mm <- rep(c("a","b","c","d","e"),c(3,4,2,3,1))
pp <- rep(c("m","n","o","p","q"),c(2,2,2,2,5))
combineByEitherFactor(cbind(mm,pp),1,2, con=FALSE, nBy=TRUE)

## ----multiCharReplace1, echo=TRUE---------------------------------------------
# replace character content
x1 <- c("ab","bc","cd","efg","ghj")
multiCharReplace(x1, cbind(old=c("bc","efg"), new=c("BBCC","EF")))

# works also on matrix and/or to replace numeric content : 
x3 <- matrix(11:16, ncol=2)
multiCharReplace(x3, cbind(12:13,112:113))

## ----multiCharReplace2, echo=TRUE---------------------------------------------
# replace and return logical vactor
x2 <- c("High","n/a","High","High","Low")
multiCharReplace(x2,cbind(old=c("n/a","Low","High"), new=c(NA,FALSE,TRUE)), convTo="logical")

## ----multiMatch1, echo=TRUE---------------------------------------------------
aa <- c("m","k","j; aa","m; aa; bb; o","n; dd","aa","cc")
bb <- c("aa","dd","aa; bb; q","p; cc") 
## result as list of indexes
(bOnA <- multiMatch(aa, bb, method="asIndex"))   # match bb on aa
## more convenient to the human reader
(bOnA <- multiMatch(aa, bb))                     # match bb on aa
(bOnA <- multiMatch(aa, bb, method="matchedL"))  # match bb on aa

## ----checkSimValueInSer, echo=TRUE--------------------------------------------
va1 <- c(4:7,7,7,7,7,8:10)+(1:11)/28600
checkSimValueInSer(va1)
cbind(va=va1, simil=checkSimValueInSer(va1))

## ----findCloseMatch1, echo=TRUE-----------------------------------------------
aA <- c(11:17); bB <- c(12.001,13.999); cC <- c(16.2,8,9,12.5,15.9,13.5,15.7,14.1,5)
(cloMa <- findCloseMatch(x=aA, y=cC, com="diff", lim=0.5, sor=FALSE))       

## ----closeMatchMatrix1, echo=TRUE---------------------------------------------
# all matches (of 2d arg) to/within limit for each of 1st arg ('x'); 'y' ..to 2nd arg = cC
# first let's display only one single closest/best hit
(maAa <- closeMatchMatrix(cloMa, aA, cC, lim=TRUE))  #

## ----closeMatchMatrix2, echo=TRUE---------------------------------------------
(maAa <- closeMatchMatrix(cloMa,aA, cC, lim=FALSE,origN=TRUE))  #
(maAa <- closeMatchMatrix(cloMa, cbind(valA=81:87,aA), cbind(valC=91:99,cC), colM=2,
  colP=2,lim=FALSE))
(maAa <- closeMatchMatrix(cloMa, cbind(aA,valA=81:87), cC, lim=FALSE, deb=TRUE))  #
a2 <- aA; names(a2) <- letters[1:length(a2)];  c2 <- cC; names(c2) <- letters[10+1:length(c2)]
(cloM2 <- findCloseMatch(x=a2, y=c2, com="diff", lim=0.5, sor=FALSE)) 
(maA2 <- closeMatchMatrix(cloM2, predM=cbind(valA=81:87,a2), 
  measM=cbind(valC=91:99,c2), colM=2, colP=2, lim=FALSE, asData=TRUE)) 
(maA2 <- closeMatchMatrix(cloM2, cbind(id=names(a2),valA=81:87,a2), cbind(id=names(c2),
  valC=91:99,c2), colM=3, colP=3, lim=FALSE, deb=FALSE)) 

## ----findSimilFrom2sets, echo=TRUE--------------------------------------------
aA <- c(11:17); bB <- c(12.001,13.999); cC <- c(16.2,8,9,12.5,12.6,15.9,14.1)
aZ <-  matrix(c(aA,aA+20), ncol=2, dimnames=list(letters[1:length(aA)],c("aaA","aZ")))
cZ <-  matrix(c(cC,cC+20), ncol=2, dimnames=list(letters[1:length(cC)],c("ccC","cZ")))
findCloseMatch(cC,aA,com="diff",lim=0.5,sor=FALSE)
findSimilFrom2sets(aA,cC)
findSimilFrom2sets(cC,aA)
findSimilFrom2sets(aA,cC,best=FALSE)
findSimilFrom2sets(aA,cC,comp="ppm",lim=5e4,deb=TRUE)
findSimilFrom2sets(aA,cC,comp="ppm",lim=9e4,bestO=FALSE)
# below: find fewer 'best matches' since search window larger (ie more good hits compete !)
findSimilFrom2sets(aA,cC,comp="ppm",lim=9e4,bestO=TRUE)      

## ----fusePairs, echo=TRUE-----------------------------------------------------
(daPa <- matrix(c(1:5,8,2:6,9), ncol=2))
fusePairs(daPa, maxFuse=4)

## ----elimCloseCoord1, echo=TRUE-----------------------------------------------
da1 <- matrix(c(rep(0:4,5),0.01,1.1,2.04,3.07,4.5), ncol=2); da1[,1] <- da1[,1]*99; head(da1)
elimCloseCoord(da1)

## ----stableMode, echo=TRUE----------------------------------------------------
set.seed(2012); dat <- round(c(rnorm(120,0,1.2), rnorm(80,0.8,0.6), rnorm(25,-0.6,0.05), runif(200)),3)
dat <- dat[which(dat > -2 & dat <2)]
stableMode(dat)

## ----stableMode2, fig.height=8, fig.width=9, fig.align="center",  echo=TRUE----
layout(1:2)
plot(1:length(dat), sort(dat), type="l", main="sorted values", las=1)
abline(h=stableMode(dat), lty=2,col=2)

plot(density(dat, kernel="gaussian", adjust=0.7))
abline(v=stableMode(dat, method="dens"), lty=2, col="red", lwd=2)
abline(v=stableMode(dat, method="binning"), lty=2, col="green")
abline(v=stableMode(dat, method="BBmisc"), lty=2, col="blue")  
abline(v=stableMode(dat, method="allModes"), lty=2, col="grey55")  

## ----stableMode3, echo=TRUE---------------------------------------------------
set.seed(2021)
x <- sample(letters, 50000, replace=TRUE)
stableMode(dat, method="mode")
stableMode(dat, method="allModes")

## ----linModelSelect1, echo=TRUE-----------------------------------------------
li1 <- rep(c(4,3,3:6),each=3) + round(runif(18)/5,2)
names(li1) <- paste0(rep(letters[1:5], each=3), rep(1:3,6))
li2 <- rep(c(6,3:7), each=3) + round(runif(18)/5, 2)
dat2 <- rbind(P1=li1, P2=li2)
exp2 <- rep(c(11:16), each=3)
exp4 <- rep(c(3,10,30,100,300,1000), each=3)

## Check & plot for linear model 
linModelSelect("P1", dat2, expect=exp2)
linModelSelect("P2", dat2, expect=exp2)

## ----plotLinModelCoef1, echo=TRUE---------------------------------------------
set.seed(2020)
x1 <- matrix(rep(c(2,2:5),each=20) + runif(100) +rep(c(0,0.5,2:3,5),20), byrow=FALSE, ncol=10, dimnames=list(LETTERS[1:10],NULL))
## just the 1st regression :
   summary(lm(b~a,data=data.frame(b=x1[,1],a=rep(1:5,each=2))))
## all regressions
x1.lmSum <- t(sapply(lapply(rownames(x1), linModelSelect, dat=x1, expect=rep(1:5,each=2), silent=TRUE, plotGraph=FALSE), 
  function(x) c(x$coef[2,c(4,1)], startFr=x$startLev)))
x1.lmSum <- cbind(x1.lmSum, medQuantity=apply(x1,1,median))
x1.lmSum[,1] <- log10(x1.lmSum[,1])
head(x1.lmSum)

## ----plotLinModelCoef2, echo=TRUE---------------------------------------------
wrGraphOK <- requireNamespace("wrGraph", quietly=TRUE)      # check if package is available
if(wrGraphOK) wrGraph::plotW2Leg(x1.lmSum, useCol=c("Pr(>|t|)","Estimate","medQuantity","startFr"), legendloc="topleft", txtLegend="start at")

## ----ratioAllComb0, echo=TRUE-------------------------------------------------
set.seed(2014); ra1 <- c(rnorm(9,2,1),runif(8,1,2))

## ----ratioAllComb1, echo=TRUE-------------------------------------------------
median(ra1[1:9])/median(ra1[10:17])

## ----ratioAllComb2, echo=TRUE-------------------------------------------------
summary( ratioAllComb(ra1[1:9],ra1[10:17]))
boxplot(list(norm=ra1[1:9],unif=ra1[10:17],rat=ratioAllComb(ra1[1:9],ra1[10:17])))

## ----readCsvBatch, echo=TRUE--------------------------------------------------
path1 <- system.file("extdata", package="wrMisc")
fiNa <-  c("pl01_1.csv","pl01_2.csv","pl02_1.csv","pl02_2.csv")
datAll <- readCsvBatch(fiNa, path1)
str(datAll)

## ----readCsvBatch2, echo=TRUE-------------------------------------------------
## batch reading of all csv files in specified path :
datAll2 <- readCsvBatch(fileNames=NULL, path=path1, silent=TRUE)

## ----readTabulatedBatch1, echo=TRUE-------------------------------------------
path1 <- system.file("extdata", package="wrMisc")
fiNa <-  c("a1.txt","a2.txt")
allTxt <- readTabulatedBatch(fiNa, path1)
str(allTxt)

## ----readVarColumns, echo=TRUE------------------------------------------------
path1 <- system.file("extdata", package="wrMisc")
fiNa <- "Names1.tsv"
datAll <- readVarColumns(fiName=file.path(path1,fiNa), sep="\t")
str(datAll)

## ----readGit1, echo=TRUE------------------------------------------------------
## An example url with tabulated data :
url1 <- "https://github.com/bigbio/proteomics-metadata-standard/blob/master/annotated-projects/PXD001819/PXD001819.sdrf.tsv"
gitDataUrl(url1)
dataPxd <- read.delim(gitDataUrl(url1), sep='\t', header=TRUE)
str(dataPxd)

## ----presenceFilt, echo=TRUE--------------------------------------------------
dat1 <- matrix(1:56,ncol=7)
dat1[c(2,3,4,5,6,10,12,18,19,20,22,23,26,27,28,30,31,34,38,39,50,54)] <- NA
dat1; presenceFilt(dat1, gr=gl(3,3)[-(3:4)], maxGr=0)
presenceFilt(dat1, gr=gl(2,4)[-1], maxGr=1, ratM=0.1)
presenceFilt(dat1, gr=gl(2,4)[-1], maxGr=2, rat=0.5)

## ----cleanReplicates, echo=TRUE-----------------------------------------------

mat3 <- matrix(c(19,20,30, 18,19,28, 16,14,35),ncol=3)
cleanReplicates(mat3,nOutl=1)

## ----normalizeThis0, echo=TRUE------------------------------------------------
set.seed(2015); rand1 <- round(runif(300) +rnorm(300,0,2),3)
dat1 <- cbind(ser1=round(100:1+rand1[1:100]), ser2=round(1.2*(100:1+rand1[101:200])-2),
  ser3=round((100:1+rand1[201:300])^1.2-3))
dat1 <- cbind(dat1, ser4=round(dat1[,1]^seq(2,5,length.out=100)+rand1[11:110],1))
dat1[dat1 <1] <- NA
## Let's get a quick overview of the data
summary(dat1)
## some selected lines (indeed, the 4th column appears always much higher)
dat1[c(1:5,50:54,95:100),]

## ----normalizeThis1, echo=TRUE------------------------------------------------
no1 <- normalizeThis(dat1, refGrp=1:3, meth="mean")
no2 <- normalizeThis(dat1, refGrp=1:3, meth="trimMean", trim=0.4)
no3 <- normalizeThis(dat1, refGrp=1:3, meth="median")
no4 <- normalizeThis(dat1, refGrp=1:3, meth="slope", quantFa=c(0.2,0.8))

## ----normalizeThis_plot1, echo=FALSE,eval=TRUE--------------------------------
boxplot(dat1, main="raw data", las=1)

## ----normalizeThis_plot2, echo=FALSE,eval=TRUE--------------------------------
layout(matrix(1:4,ncol=2))
boxplot(no1, main="mean normalization", las=1)
boxplot(no2, main="trimMean normalization", las=1)
boxplot(no3, main="median normalization", las=1)
boxplot(no4, main="slope normalization", las=1)

## ----coordOfFilt1, echo=FALSE,eval=TRUE---------------------------------------
set.seed(2021); ma1 <- matrix(sample.int(n=40,size=27,replace=TRUE), ncol=9)
## let's test which values are >37
which(ma1 >37)      # doesn't tell which row & col
coordOfFilt(ma1, ma1 >37)

## ----rnormW1, echo=TRUE-------------------------------------------------------
## some sample data :
x1 <- (11:16)[-5]
mean(x1); sd(x1)

## ----rnormW2, echo=TRUE-------------------------------------------------------
## the standard way for gerenating normal random values
ra1 <- rnorm(n=length(x1), mean=mean(x1), sd=sd(x1))
## In particular with low n, the random values deviate somehow from expected mean and sd :
mean(ra1) -mean(x1) 
sd(ra1) -sd(x1)

## ----rnormW3, echo=TRUE-------------------------------------------------------
## random numbers with close fit to expected mean and sd :
ra2 <- rnormW(length(x1), mean(x1), sd(x1))
mean(ra2) -mean(x1) 
sd(ra2) -sd(x1)   # much closer to expected value

## ----moderTest2grp, echo=TRUE-------------------------------------------------
set.seed(2017); t8 <- matrix(round(rnorm(1600,10,0.4),2), ncol=8,
  dimnames=list(paste("l",1:200), c("AA1","BB1","CC1","DD1","AA2","BB2","CC2","DD2")))
t8[3:6,1:2] <- t8[3:6,1:2]+3     # augment lines 3:6 for AA1&BB1
t8[5:8,5:6] <- t8[5:8,5:6]+3     # augment lines 5:8 for AA2&BB2 (c,d,g,h should be found)
t4 <- log2(t8[,1:4]/t8[,5:8])
fit4 <- moderTest2grp(t4, gl(2,2))
## now we'll use limma's topTable() function to look at the 'best' results
if("list" %in% mode(fit4)) {  # if you have limma installed we can look further
  library(limma)
  topTable(fit4, coef=1,n=5)                      # effect for 3,4,7,8
  fit4in <- moderTest2grp(t4, gl(2,2), testO="<")
  if("list" %in% mode(fit4in)) topTable(fit4in, coef=1,n=5) }

## ----moderTestXgrp, echo=TRUE-------------------------------------------------
grp <- factor(rep(LETTERS[c(3,1,4)],c(2,3,3)))
set.seed(2017); t8 <- matrix(round(rnorm(208*8,10,0.4),2), ncol=8,
  dimnames=list(paste(letters[],rep(1:8,each=26),sep=""), paste(grp,c(1:2,1:3,1:3),sep="")))
t8[3:6,1:2] <- t8[3:6,1:2] +3                    # augment lines 3:6 (c-f) 
t8[5:8,c(1:2,6:8)] <- t8[5:8,c(1:2,6:8)] -1.5    # lower lines 
t8[6:7,3:5] <- t8[6:7,3:5] +2.2                  # augment lines 
## expect to find C/A in c,d,g, (h)
## expect to find C/D in c,d,e,f
## expect to find A/D in f,g,(h)  
test8 <- moderTestXgrp(t8, grp) 
head(test8$p.value, n=8) 

## ----pVal2lfdr, echo=TRUE-----------------------------------------------------
set.seed(2017); t8 <- matrix(round(rnorm(160,10,0.4),2), ncol=8, dimnames=list(letters[1:20],
  c("AA1","BB1","CC1","DD1","AA2","BB2","CC2","DD2")))
t8[3:6,1:2] <- t8[3:6,1:2]+3   # augment lines 3:6 (c-f) for AA1&BB1
t8[5:8,5:6] <- t8[5:8,5:6]+3   # augment lines 5:8 (e-h) for AA2&BB2 (c,d,g,h should be found)
head(pVal2lfdr(apply(t8, 1, function(x) t.test(x[1:4],x[5:8])$p.value)))

## ----matchSampToPairw, echo=TRUE----------------------------------------------
## make example if limma is not installed
if(!requireNamespace("limma", quietly=TRUE)) test8 <- list(FDR=matrix(1,nrow=2,ncol=3,dimnames=list(NULL,c("A-C","A-D","C-D")))) 
matchSampToPairw(unique(grp), colnames(test8$FDR)) 

## ----pairWiseConc1, echo=TRUE-------------------------------------------------
mat1 <- matrix(1:8, nrow=2, dimnames=list(NULL, paste0(1:4,"-",6:9)))
numPairDeColNames(mat1)

## ----std1, echo=TRUE----------------------------------------------------------
dat <- matrix(2*round(runif(100),2), ncol=4)
mean(dat); sd(dat)

datS <- scale(dat)
apply(datS, 2, sd)
# each column was teated separately
mean(datS); sd(datS); range(datS)
# the mean is almost 0.0 and the sd almost 1.0

datB <- scale(dat, center=TRUE, scale=FALSE)
mean(datB); sd(datB); range(datB)              # mean is almost 0

## ----std2, echo=TRUE----------------------------------------------------------
datS2 <- standardW(dat)
apply(datS2, 2, sd)
summary(datS2)
mean(datS2); sd(datS2)

datS3 <- standardW(dat, byColumn=TRUE)
apply(datS3, 2, sd)
summary(datS3)
mean(datS3); sd(datS3)

## ----scale1, echo=TRUE--------------------------------------------------------
datR2 <- apply(dat, 2, scaleXY, 1, 100)
summary(datR2); sd(datR2)

## ----clu01, echo=TRUE---------------------------------------------------------
nGr <- 3
irKm <- stats::kmeans(iris[,1:4], nGr, nstart=nGr*4)             # no need to standardize
   table(irKm$cluster, iris$Species)
   #wrGraph::plotPCAw(t(as.matrix(iris[,1:4])), sampleGrp=irKm,colBase=irKm$cluster,useSymb=as.numeric(as.factor(iris$Species)))

## ----clu02, echo=TRUE---------------------------------------------------------
## sort results by cluster number
head(reorgByCluNo(iris[,-5], irKm$cluster))
tail(reorgByCluNo(iris[,-5], irKm$cluster))

## ----clu03, echo=TRUE---------------------------------------------------------
## median an CV
ir2 <- reorgByCluNo(iris[,-5],irKm$cluster,retList=TRUE)

sapply(ir2, function(x) apply(x,2,median))

sapply(ir2, colSds)

## ----filterNetw0, echo=TRUE---------------------------------------------------

lst2 <- list('121'=data.frame(ID=as.character(c(141,221,228,229,449)),11:15), 
  '131'=data.frame(ID=as.character(c(228,331,332,333,339)),11:15), 
  '141'=data.frame(ID=as.character(c(121,151,229,339,441,442,449)),c(11:17)), 
  '151'=data.frame(ID=as.character(c(449,141,551,552)),11:14),
  '161'=data.frame(ID=as.character(171),11),
  '171'=data.frame(ID=as.character(161),11),
  '181'=data.frame(ID=as.character(881:882),11:12) )

## ----filterNetw1, echo=TRUE---------------------------------------------------
(nw1 <- filterNetw(lst2, limInt=20, sandwLim=NULL, remOrphans=FALSE))

## ----filterNetw2, echo=TRUE---------------------------------------------------
(nw2 <- filterNetw(lst2, limInt=20, sandwLim=NULL, remOrphans=TRUE))

## ----filterNetw3, echo=TRUE---------------------------------------------------
(nw3 <- filterNetw(lst2, limInt=20, sandwLim=14, remOrphans=TRUE))

## ----propMatr1, echo=TRUE-----------------------------------------------------
pairs3L <- matrix(LETTERS[c(1,3,3, 2,2,1)], ncol=2)      # loop of 3
(netw13pr <- pairsAsPropensMatr(pairs3L))                # as prop matr

## ----contribToContigPerFrag, echo=TRUE----------------------------------------
path1 <- matrix(c(17,19,18,17, 4,4,2,3), ncol=2,
  dimnames=list(c("A/B/C/D","A/B/G/D","A/H","A/H/I"), c("sumLen","n")))
contribToContigPerFrag(path1)

## ----simpleFragFig, echo=TRUE-------------------------------------------------
frag1 <- cbind(beg=c(2,3,7,13,13,15,7,9,7, 3,3,5), end=c(6,12,8,18,20,20,19,12,12, 4,5,7))
rownames(frag1) <- letters[1:nrow(frag1)]
simpleFragFig(frag1)

## ----countSameStartEnd, echo=TRUE---------------------------------------------
countSameStartEnd(frag1)

## ----pasteC, echo=TRUE--------------------------------------------------------
pasteC(1:4)

## ----color-gradient1, echo=TRUE-----------------------------------------------
set.seed(2015); dat1 <- round(runif(15),2)
plot(1:15, dat1, pch=16, cex=2, las=1, col=colorAccording2(dat1),
  main="Color gradient according to value in y")
# Here we modify the span of the color gradient
plot(1:15, dat1, pch=16, cex=2, las=1, 
  col=colorAccording2(dat1,nStartO=0,nEndO=4,revCol=TRUE), main="blue to red")
# It is also possible to work with scales of transparency
plot(1:9, pch=3, las=1)
points(1:9,1:9,col=transpGraySca(st=0,en=0.8,nSt=9,trans=0.3), cex=42, pch=16)

## ----convColorToTransp, fig.height=6, fig.width=3, echo=TRUE------------------
col0 <- c("#998FCC","#5AC3BA","#CBD34E","#FF7D73")
col1 <- convColorToTransp(col0,alph=0.7)
layout(1:2)
pie(rep(1,length(col0)), col=col0, main="no transparency")
pie(rep(1,length(col1)), col=col1, main="new transparency")

## ----sysDate1, echo=TRUE------------------------------------------------------
## To get started
Sys.Date()

## Compact English names, no matter what your local settings are :
sysDate() 

## ----DateTab, echo=TRUE-------------------------------------------------------
tabD <- cbind(paste0("univ",1:6), c(sysDate(style="univ1"), sysDate(style="univ2"), sysDate(style="univ3"), 
  sysDate(style="univ4"), as.character(sysDate(style="univ5")), sysDate(style="univ6")),
  paste0("   local",1:6), c(sysDate(style="local1"), sysDate(style="local2"), sysDate(style="local3"), 
  sysDate(style="local4"), sysDate(style="local5"), sysDate(style="local6")))   
kable(tabD, caption="Various ways of writing current date")

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

