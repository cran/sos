\name{hits}
\alias{hits}
\alias{matches}
\title{
  matches attribute of a findFn object
}
\description{
  Returns the \code{matches} attribute of a \code{findFn} object.
  For the output of \code{findFn}, this is the number of  matches
  for the search term.  For a \code{findFn} object returned by
  \code{unionFindFn} or \code{intersectFindFn}, this is a
  numeric vector if the \code{matches} attributes of the arguments to
  \code{unionFindFn} or \code{intersectFindFn}.
}
\usage{
matches(x)
hits(x)
}
\arguments{
  \item{x}{
    object of class \code{findFn}.
  }
}
\details{
  nrow(x)
  attr(x, 'matches')
}
\value{
  a list with components \code{nrows} and \code{matches}
}
\author{Spencer Graves}
\seealso{
  \code{\link{findFn}}
  \code{\link{unionFindFn}}
  \code{\link{intersectFindFn}}
}
%\references{}
\examples{
des1 <- findFn('differential equations', 1)

des1. <- matches(des1)
des. <- list(nrow=nrow(des1), matches=attr(des1, 'matches'))
\dontshow{stopifnot(}
all.equal(des1., des.)
\dontshow{)}
}
\keyword{misc}
