\name{writeFindFn2xls}
\alias{writeFindFn2xls}
\title{
  Write a findFn object to an Excel file
}
\description{
  Write a \code{findFn} object to an Excel file with sheets for
  \code{\link{PackageSum2}}, the \code{findFn} table, and the
  \code{call} attribute of the \code{findFn} object.
}
\usage{
writeFindFn2xls(x,
       file.=paste(deparse(substitute(x)), 'xls', sep='.'),
       csv, ...)
}
\arguments{
  \item{x}{
    An object of class \code{findFn}
  }
  \item{file.}{
    Name of Excel file to create.
  }
  \item{csv}{
    logical:  if TRUE, write three *.csv files rather than one
    *.xls file.  Default is FALSE if software is available to
    write a *.xls file and TRUE otherwise.
  }
  \item{\dots}{
    optional arguments to \code{write.csv} used if
  }
}
\details{
  1.  Create the sheets

  2.  if(csv | \code{!require(RODBC)} | cannot otherwise
  create an Excel file) write.csv with names ending '-sum.csv' for
  \code{PackageSum2}, '.csv' for the main table, and '-call.csv' for the
  \code{call} sheet.

  3.  Open connection

  4.  Write the sheets

  5.  Done.
}
\value{
  The name of the file created is returned invisibly.
}
\author{
  Spencer Graves with help from Dirk Eddedbuettel,
  Gabor Grothendiek, and Marc Schwartz.
}
\seealso{
  \code{\link{findFn}},
  \code{\link[RODBC]{odbcConnect}},
  \code{\link[RODBC]{sqlSave}},
  \code{\link[RODBC]{odbcClose}}
}
\examples{
splineSearch <- findFn("spline", maxPages = 1)

writeFindFn2xls(splineSearch)

writeFindFn2xls(splineSearch, csv=TRUE)
}
\keyword{print}

