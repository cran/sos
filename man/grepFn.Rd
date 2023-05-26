\name{grepFn}
\alias{grepFn}
\title{
  Match pattern in a column of a matrix or data.frame
}
\description{
  Search for \code{pattern} in a \code{column} of a matrix or
  data.frame using \code{grep}.  If \code{value} = TRUE (the 
  default), return the selected subset of \code{x}.
}
\usage{
grepFn(pattern, x, column='Function', ignore.case=FALSE,
       perl=FALSE, value=TRUE, fixed=FALSE,
       useBytes=FALSE, invert=FALSE)
}
\arguments{
  \item{x}{
    a matrix or data.frame containing a column named \code{column}.
  }
  \item{pattern, ignore.case, perl, fixed, useBytes, invert}{
    as for \code{grep}
  }
  \item{column}{
    character string giving the column of \code{x} in which to search
    for \code{pattern}.
  }
  \item{value}{
    logical:  If TRUE, return the selected subset of \code{x}.  If
    FALSE, return the row numbers returned by \code{grep}.
  }
}
\details{
  1.  g <- grep(pattern, x[, column])

  2.  if(value)return(x[g, ]) else return(g)
}
\value{
  If(value) return an object of the same class as \code{x} containing
  those rows of \code{x} with \code{x[, column]} matching
  \code{pattern}.

  Else, return an integer vector identifying the rows of \code{x} with
  \code{x[, column]} matching \code{pattern}.
}
\author{
  Spencer Graves, Sundar Dorai-Raj
}
\seealso{
  \code{\link{findFn}}
  \code{\link[base]{grep}}
}
%\references{}
\examples{
z <- cbind(a=1:2, Function=c('s', 'spline'))
z. <- grepFn("spline", z)

\dontshow{stopifnot(}
all.equal(z., z[2,,drop=FALSE])
\dontshow{)}
}
\keyword{misc}
