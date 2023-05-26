\name{Extract.findFn}
\alias{Extract.findFn}
\alias{[.findFn}
\title{Subset a findFn object}
\description{
  Extract rows from a \code{\link{findFn}} object
}
\usage{
\method{[}{findFn}(x, i, j,
    drop = 
    if (missing(i)) TRUE else length(cols) == 1)
%]
}
\arguments{
  \item{x}{An object of class \code{findFn}}
  \item{i}{
    a valid object to select rows of \code{x}, e.g., 
    a vector of all positive integers or all 
    negative integers between 1 and \code{nrow(x)} 
    or a logical vector of length \code{nrow(x)}.
  }
  \item{j}{
    If not missing, the extraction function returns 
    an object of class \code{data.frame} rather than
    \code{findFn}.
  }
  \item{drop}{
    logical:  if \code{FALSE} and \code{j} selects
    only one column, return that column as a vector; 
    else return a \code{data.frame} if \code{j} is
    present or a \code{findFn} object otherwise.
  }
}
\details{
  1.  \code{if(missing(j))} extract the subset with
  the \code{PackageSummary} attribute recomputed on
  the subset.

  2.  else 
  \code{return(Extract.data.frame(x, i, j, drop))}
}
\value{
  If \code{j} is missing, return an object of class 
  \code{c('findFn', 'data.frame')} else return
  whatever is returned by \code{Extract.data.frame}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{findFn}},
  \code{\link{data.frame}}
}
\examples{
  z <- try(findFn("spline", maxPages = 2))
  
  if(!inherits(z, "try-error")){

  z1 <- z[1,]

  z.2 <- z[, 2]
  }
}
\keyword{methods}
