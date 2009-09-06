\name{sortFindFn}
\alias{sortFindFn}
\title{Sort a findFn Object}
\description{
  Sort a \code{data.frame} as a findFn object.
}
\usage{
sortFindFn(x, sortby=NULL)
}
\arguments{
  \item{x}{
    a \code{data.frame} to sort and convert to an object of class
    \code{findFn} (if it does not alreay have this class).
  }
  \item{sortby}{
    sort information as for function \code{findFn}.
  }
}
\details{
  1.  pkgSum <- PackageSummary(x, sortby)

  2.  Order x as required for \code{findFn}

  3.  class = c("findFn", "data.frame")
}
\value{
  An object of class c('findFn', 'data.frame') with a
  "PackageSummary" attribute.
}
\author{Spencer Graves}
\seealso{
  \code{\link{findFn}}
  \code{\link{sort}}
  \code{\link{order}}
}
\examples{
tstdf <- data.frame(Package=letters[c(1,1,2)],
                    Function=c('a1', 'a2', 'b3'), Score=2:4,
                    Date=11:13, Description=c('D1', 'D2', 'D3'),
                    Link=c('L1', 'L2', 'L3'), stringsAsFactors=FALSE)
rss <- sortFindFn(tstdf)
}
\keyword{methods}
\keyword{print}
