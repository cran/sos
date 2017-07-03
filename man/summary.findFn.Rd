\name{summary.findFn}
\alias{summary.findFn}
\alias{print.summary.findFn}
\title{Summary Method for findFn}
\description{
  Summary Method for objects of class \code{findFn}
}
\usage{
\method{summary}{findFn}(object, minPackages = 12,
                         minCount = NA, ...)
}
\arguments{
  \item{object}{An object of class \code{findFn}}
  \item{minPackages}{
    the minimum number of packages to include in the summary.  
    Other packages with the same count will also appear in 
    the summmary, but packages with a smaller count will not.

    The number of packages displayed will be less than
    \code{minPackages} only when there are fewer than that 
    number of packages containing the search term in its help 
    pages.
  }
  \item{minCount}{
    the minimum count for a package to display.  
    \code{minCount} = 1 displays all packages.  The default is 
    the minimum of the input \code{minCount} and the count for
    package number \code{minPackages}.
  }
  \item{\dots}{ignored}
}
\details{
  Return an object of class c('summary.findFn', 'list') with 
  summary information on only packages satisfying the 
  \code{minPackages} and \code{minCount} criteria.  The 
  \code{minPackages} and \code{minCount} components of the 
  summary output list will be adjusted as necessary to match
  characteristics of \code{object}.  The \code{print} method 
  for a \code{summary.findFn} object will display the 
  \code{minCount}, but \code{minPackages} will be a component 
  of the returned object without being printed.
}
\value{
  An object of class c('summary.findFn', 'list') with the 
  following elements:

  \item{PackageSummary}{
    a data.frame with one row for each package and columns
    \code{Package}, \code{Count}, \code{MaxScore}, 
    \code{TotalScore}, \code{Date}, and \code{pgLink}. This 
    summary is sorted per the \code{sortby} argument in the 
    call to \code{findFn}.
  }
  \item{minPackages, minCount}{
    the \code{minPackages} and \code{minCount} arguments 
    in this call to \code{summary.findFn}.
  }
  \item{matches}{
    the total number of matches returned by \code{findFn}.  
    This is an attribute of a \code{findFn} object;  the 
    number of rows of \code{object} will either be 
    \code{matches} or maxPages*matchesPerPage, whichever 
    is smaller.
  }
  \item{nrow}{
    the number of matches in this \code{findFn} object
  }
  \item{nPackages}{
    the number of packages in this \code{findFn} object
  }
  \item{call}{
    the matched call to \code{findFn}.
  }
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{findFn}},
  \code{\link[utils]{RSiteSearch}}
}
\examples{
  z <- findFn("spline", maxPages = 2)
  summary(z, 2)
}
\keyword{methods}
\keyword{print}
