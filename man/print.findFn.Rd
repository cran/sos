\name{print.findFn}
\alias{print.findFn}
\title{
  print a findFn object
}
\description{
  Print a \code{findFn} object to a file and pass it to a 
  web browser
}
\usage{
\method{print}{findFn}(x, where, title, openBrowser = TRUE,
	template, ...)
}
\arguments{
  \item{x}{
    An object of class \code{findFn}
  }
  \item{where}{
    a character vector interpreted as follows:

    If length(where)==1, it must be either 'HTML' or 
    'console' or the name of a colum of \code{x} or the 
    name of a file to hold the file created to be displayed 
    in a web browser.

    If length(where)>1, it must be the names of columns of 
    \code{x} to be displayed on the console.  If 
    \code{where} is a vector of names of columns of \code{x}, 
    those columns will be printed to the console, and there 
    will be no display in a web browser.  If \code{where} ==
    'console', the following columns of \code{x} are 
    displayed:  c('Count', 'Package', 'Function', 'Score', 
    'Date').
  }
  \item{title}{
    An optional title to give the HTML file. Default is to use the
    original query string.
  }
  \item{openBrowser}{
    logical; if \code{TRUE} and \code{where} is missing or 'HTML',
    launch default browser after building table
  }
  \item{template}{
    Template file used by \code{brew}
  }
  \item{\dots}{ignored}
}
%\details{ more later}
\value{
  The full path and name of the file created is returned invisibly.
}
\author{
  Sundar Dorai-Raj, Spencer Graves, Romain Francois, Uwe Ligges
}
\seealso{
  \code{\link{findFn}},
  \code{\link[utils]{RSiteSearch}},
  \code{\link[utils]{browseURL}}
  \code{\link[brew]{brew}}
}
\examples{
splineSearch <- findFn("spline", maxPages = 2)
if(!fda::CRAN()){
  print(splineSearch, 'console')
  splineSearch # all columns in a browser
}
}
\keyword{print}
\keyword{methods}

