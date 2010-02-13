\name{installPackages}
\alias{installPackages}
\title{
  install packages with minimum count
}
\description{
  \code{install.packages} identifed in a \code{findFn} object with
  \code{Count} at least \code{minCount}.
}
\usage{
installPackages(x, minCount=sqrt(x[1, 'Count']), ...)
}
\arguments{
  \item{x}{
    either a character vector to be passed to
    \code{\link{install.packages}} or a \code{findFn} object
  }
  \item{minCount}{
    Install all packages identified in \code{x[['Packages']]} with
    \code{x[['Count']]} exceeding \code{minCount}.
  }
  \item{\dots}{
    optional arguments passed to \code{\link{install.packages}}
  }
}
\details{
  This is useful to prepare for \code{PackageSum2}.
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
spl <- findFn("spline", maxPages = 2)
# check the code but do not install anything:
installPackages(spl, minCount=spl[1, 'Count']+1)

# install ALL packages found
\dontrun{
installPackages(spl, 1)
}

}
\keyword{models}
