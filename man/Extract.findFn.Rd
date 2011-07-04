\name{Extract.findFn}
\alias{Extract.findFn}
\alias{[.findFn}
\title{Subset a findFn object}
\description{
  Extract rows from a \code{findFn} object
}
\usage{
\method{[}{findFn}(x, i, j,
    drop = if (missing(i)) TRUE else length(cols) == 1)
%]
}
\arguments{
  \item{x}{An object of class \code{findFn}}
  \item{i}{
    a valid object to select rows of \code{x}, e.g., a vector of all
    positive integers or all negative integers between 1 and nrow(x) or
    a logical vector of length nrow(x).
  }
  \item{j}{
    If not missing, the extraction function returns an object of class
    \code{data.frame} rather than \code{findFn}.
  }
  \item{drop}{
    logical:  if FALSE and \code{j} selects only one column, return that
    column as a vector;  else return a \code{data.frame} if \code{j} is
    present or a \code{findFn} object otherwise.
  }
}
\details{
  1.  if(missing(j)) extract the subset with the \code{PackageSummary}
  attribute recomputed on the subset.

  2.  else return(Extract.data.frame(x, i, j, drop))
}
\value{
  If \code{j} is missing, return an object of class c('findFn',
  'data.frame') else return whatever is returned by
  \code{Extract.data.frame}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{findFn}},
  \code{\link{data.frame}}
}
\examples{
  z <- findFn("spline", maxPages = 2)

  z1 <- z[1,]

  z.2 <- z[, 2]
}
\keyword{methods}
