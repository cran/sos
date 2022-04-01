\name{writeFindFn2xls}
\alias{writeFindFn2xls}
\alias{findFn2xls}
\title{
  Write a findFn object to an Excel file
}
\description{
  Write a \code{\link{findFn}} object to an Excel 
  file with sheets for \code{\link{PackageSum2}}, 
  the \code{findFn} table, and the \code{call}
  attribute of the \code{findFn} object.
}
\usage{
writeFindFn2xls(x,
       file.=paste(deparse(substitute(x)), 'xls', 
       sep='.'), csv, ...)
findFn2xls(x,
       file.=paste(deparse(substitute(x)), 'xls', 
       sep='.'), csv, ...)
}
\arguments{
  \item{x}{
    An object of class \code{findFn}
  }
  \item{file.}{
    Name of Excel file to create.  If a file of this
    name already exists, it will be overwritten.
  }
  \item{csv}{
    logical:  if \code{TRUE}, write three 
    \code{*.csv} files rather than one \code{*.xls}
    file.  Default is \code{FALSE} if software is
    available to write a \code{*.xls} file and 
    \code{TRUE} otherwise.
  }
  \item{\dots}{
    optional arguments to \code{write.csv} used if
  }
}
\details{
  \code{findFn2xls} is an alias for
  \code{writeFindFn2xls};  both
  functions do the same thing.
}
\value{
  The name of the file created is returned invisibly.
}
\author{
  Spencer Graves with help from Dirk Eddelbuettel,
  Gabor Grothendiek, and Marc Schwartz.
}
\seealso{
  \code{\link{findFn}},
  \code{\link[RODBC]{odbcConnect}},
  \code{\link[RODBC]{sqlSave}},
  \code{\link[RODBC]{odbcClose}}
%  \code{\link[dataframes2xls]{write.xls}}
  \code{\link[WriteXLS]{WriteXLS}}
}
\examples{
splineSearch <- findFn("spline", maxPages = 1)

writeFindFn2xls(splineSearch)

findFn2xls(splineSearch, csv=TRUE)
}
\keyword{print}

