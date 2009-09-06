\name{unionFindFn}
\alias{unionFindFn}
\alias{intersectFindFn}
\alias{Ops.findFn}
\title{
  Combine findFn Objects
}
\description{
  Combines to \code{findFn} objects into a new \code{findFn}
  object with only one row for any help page duplicated between the
  two.  \code{unionFindFn} removes duplicate entries.
  \code{intersectFindFn} keeps only the duplicates.
}
\usage{
unionFindFn(e1, e2, sortby=NULL)
intersectFindFn(e1, e2, sortby=NULL)

\S3method{Ops}{findFn}(e1,e2)
# This supports "|" for "unionFindFn"
# and "&" for "intersectFindFn".
}
\arguments{
  \item{e1, e2}{
    objects of class \code{findFn}.
  }
  \item{sortby}{
    Optional \code{sortby} argument used by \code{sortFindFn} and
    \code{findFn}.  Default is the \code{sortby} argument
    in \code{attr(e1, 'call')}.
  }
}
\details{
  1.  e12 <- rbind(e1, e2)

  2.  For any (\code{Package}, \code{Function}) appearing in both
  \code{e1} and \code{e2}, the row with the largest \code{Score} is
  retained and the other is deleted.

  3.  Apply \code{sortFindFn} to the rebuild the summary and sort
  the result as desired.

  4.  attr(e12, 'matches') <- c(attr(e1, 'matches'), attr(e2,
  'matches'))
}
\note{
  Binary operators \samp{&} and \samp{|} are implemented for the S3
  class \samp{findFn}
}
\value{
  an object with class c('findFn', 'data.frame') as returned by
  \code{sortFindFn} and \code{findFn}.
}
\author{
  Spencer Graves and Romain Francois
}
\seealso{
  \code{\link{findFn}}
  \code{\link{sortFindFn}}
}
%\references{}
\examples{
des1 <- findFn('differential equations', 1)
de1 <- findFn('differential equation', 1)
# each retrieves 1 page of 20 hits
# but not the same 20

de.s <- unionFindFn(des1, de1)
# combines the two, eliminating duplicates.

# or the sorter version:
de.s. <- des1 | de1
\dontshow{stopifnot(}
all.equal(de.s, de.s.)
\dontshow{)}

# Keep only the common entries.
de2 <- intersectFindFn(des1, de1)
de2. <- des1 & de1
\dontshow{stopifnot(}
all.equal(de2, de2.)
\dontshow{)}

# summary and print still work with the combined object.
summary(de.s)
de.s

summary(de2)
de2

\dontshow{
# The following gave an error;  now fixed 2009.05.12
x <- findFn("spline", maxPages = 1, quiet = TRUE)
y <- findFn("winbugs", maxPages = 1, quiet = TRUE)
intersectFindFn( x, y )
}

}
\keyword{misc}
