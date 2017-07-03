\name{installPackages}
\alias{installPackages}
\alias{installPackages.findFn}
\alias{installPackages.packageSum}
\title{
  install packages with minimum count
}
\description{
  Ensure that the most important packages 
  in \code{x} are installed.  "Importance" here is 
  defined in the description of the \code{minCount}
  argument below.  
}
\usage{
%installPackages(x, minCount=sqrt(x[1, 'Count']), ...)

installPackages(x, minCount, ...)
\method{installPackages}{findFn}(x, minCount, ...) 
\method{installPackages}{packageSum}(x, minCount, 
              repos = getOption("repos"), ...)
}
\arguments{
  \item{x}{
    either a character vector to be passed to
    \code{\link{install.packages}} or a \code{findFn} 
    or a \code{packageSum} object
  }
  \item{minCount}{
    Controls how many of the packages identified in 
    \code{x} to pass to \code{\link{install.packages}}.  
    
    If \code{x} is a \code{findFn} or 
    \code{packageSum} object, install every 
    \code{x[, 'Package']} with \code{x[, 'Count']} >= 
    \code{minCount}.  By default, \code{minCount} = 
    \code{sqrt(x[1, 'Count'])}.  
  }
  \item{repos}{
    argument passed to 
    \code{\link{install.packages}}
  }
  \item{\dots}{
    optional arguments passed to 
    \code{\link{install.packages}}
  }
}
\details{
  Functions \code{\link{PackageSum2}} and 
  \code{\link{packageSum}} obtain some of the information 
  displayed from installed packages.  To get more 
  information in those summaries, run 
  \code{installPackages} on a \code{\link{findFn}} or
  \code{\link{packageSum}} object to install more 
  of the packages found.  
}
\value{
  none
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{install.packages}}
  \code{\link{PackageSum2}}
}
%\references{
%  Robert Ferstl and Josef Hayden (2008) "Zero-Coupon Yield Curve
%  Estimation with the Package \code{termstrc}";  available from
%  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1307149},
%  especially expression (9), sec. 3, p. 7, accessed 2009.05.09.
%}
\examples{
##
## 1.  findFn object
##
spl <- findFn("spline", maxPages = 2)
# check the code but do not install anything:
installPackages(spl, minCount=spl[1, 'Count']+1)

# default: install packages with 
# Count>=minCount
\dontrun{
installPackages(spl)
}

##
## 2.  packageSum object
##

splS <- packageSum(spl)
# check the code but do not install anything:
installPackages(splS, splS[1, 'Count']+1)

# install ALL packages
\dontrun{
installPackages(splS, 1)
}

}
\keyword{package}
