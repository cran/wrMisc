% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildTree.R
\name{buildTree}
\alias{buildTree}
\title{Connect edges to from tree and extract all possible branches}
\usage{
buildTree(
  disDat,
  startFr = NULL,
  posCo = c("beg", "end"),
  silent = FALSE,
  debug = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{disDat}{(matrix or data.frame) integer values with 1st column, ie start site of fragment, 2nd column as end of fragments, rownames as unique IDs (node-names)}

\item{startFr}{(integer) index for 1st node (typically =1 if 'disDat' sorted by "beg"), should point to a terminal node for consective growing of branches}

\item{posCo}{(character) colnames specifying the begin & start sites in 'disDat', if NULL 1st & 2nd col will be used}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
This function returns a list with $paths (branches as matrix with columns 'sumLen' & 'n'), $usedNodes (character vector of all names used to build tree) and $tree (object from data.tree)
}
\description{
It is assumed that multiple fragments from a common ancestor bay be charcterized by the their start- and end-sites by integer values.  
For example, If 'abcdefg' is the ancestor, the fragments 'bcd' (from position 2 to 4) to  and 'efg' may then be assembled. 
To do so, all fragments must be presented as matix specifying all start- and end-sites (and fragment-names).  
\code{buildTree} searchs contiguous fragments from columns 'posCo' (start/end) from 'disDat' to build tree & extract path information starting with line 'startFr'.
Made for telling if dissociated fragments contribute to long assemblies.
This function uses various functions of package \href{https://CRAN.R-project.org/package=data.tree}{data.tree} which must be installed, too.
}
\examples{
frag2 <- cbind(beg=c(2,3,7,13,13,15,7,9,7,3,7,5,7,3),end=c(6,12,8,18,20,20,19,12,12,4,12,7,12,4)) 
rownames(frag2) <- c("A","E","B","C","D","F","H","G","I", "J","K","L","M","N")
buildTree(frag2)
countSameStartEnd(frag2)
}
\seealso{
package \href{https://CRAN.R-project.org/package=data.tree}{data.tree} original function used \code{\link[data.tree]{Node}}; in this package : for exploiting edge/tree related issues \code{\link{simpleFragFig}}, \code{\link{countSameStartEnd}} and \code{\link{contribToContigPerFrag}},
}
