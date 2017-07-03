\name{PackageSummary}
\alias{PackageSummary}
\title{Summarize findFn Results by Package}
\description{
  Returns a \code{\link{data.frame}} with one row for each 
  package and columns \code{Count} = number of rows in the 
  search results for that package, \code{maxScore} and 
  \code{totalScore} = max and total score for help pages 
  found from that package.
}
\usage{
PackageSummary(x, sortby=NULL)
}
\arguments{
  \item{x}{
    a data.frame with columns \code{Package}, \code{Score}, 
    and \code{Date}.
  }
  \item{sortby}{
    a character vector specifying how the \code{data.frame} 
    returned should be sorted.  Default = c('Count', 
    'MaxScore', 'TotalScore', 'Package') to sort descending 
    on numerics and ascending on alphnumerics.  Specifying 
    sortby = c('c', 't', 'm') is equivalent to c('Count', 
    'TotalScore', 'MaxScore', 'Package', 'Score', 
    'Function').

    Components of \code{sortby} must match either this list 
    or c('Score', 'Function', 'Date', 'Description', 'Link').  
    Any on this latter list are ignored without a warning.  
    This allows the same \code{sortby} used for \code{findFn} 
    to be used here.
  }
}
\details{
  1.  Convert x['Package] to character to automatically 
  drop any unused levels of a factor.  
  
  2.  Compute Count, TotalScore, and MaxScore.  
  
  3.  Find the first occurrence of each Package, and use 
  that to convert the Link to the first help page to 
  \code{pkgLink} = a link for the package.  For example, 
  the Link to 'html' for help('c') is 
  'http://finzi.psych.upenn.edu/R/library/base/html/c.html', 
  and \code{pkgLink} to the 'html' overview for 'base' is 
  'http://finzi.psych.upenn.edu/R/library/base/html/00Index.html'.  
  
  4.  Assemble into a \code{\link{data.frame}}, sort and 
  return.  
}
\value{
  a \code{\link{data.frame}} with one row for each package and 
  columns \code{Package}, \code{Count}, \code{MaxScore}, 
  \code{TotalScore}, \code{Date}, and \code{pkgLink}, 
  sorted as specified by \code{sortby}.
}
\author{Spencer Graves}
\seealso{
  \code{\link[utils]{RSiteSearch}},
  \code{\link{findFn}}
  \code{\link{PackageSum2}}, 
  \code{\link{packageSum}}
}
\examples{
tstdf <- data.frame(Package=letters[c(1,1,2)], Score=2:4,
                    Date=LETTERS[1:3], stringsAsFactors=FALSE)
tstdf$Link <- paste0('http://finzi.psych.upenn.edu/R/library/', 
    tstdf$Package, '/html/', letters[4:6], '.html')
    
tstSum <- PackageSummary(tstdf)
# The answer:
tstSm <- data.frame(Package=letters[1:2], Count=c(a=2, b=1),
    MaxScore=c(3, 4), TotalScore=c(5, 4), 
    Date=LETTERS[c(1, 3)], stringsAsFactors=FALSE)
tstSm$pkgLink <- paste0('http://finzi.psych.upenn.edu/R/library/', 
    tstdf$Package[2:3], '/html/00Index.html')    
row.names(tstSm) <- 1:2
\dontshow{stopifnot(}
all.equal(tstSum, tstSm)
\dontshow{)}
}
\keyword{misc}
