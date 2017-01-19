\name{findFn}
\alias{findFn}
\alias{?}
\alias{???}
\title{
  Search Help Pages
}
\description{
  Returns a data.frame from
  \code{\link[utils]{RSiteSearch}(string, "function")} which can be
  sorted and subsetted by user specifications and viewed in an HTML
  table.  The default sort puts first packages with the most matches
  (\code{Count}), with ties broken using the sum of the match scores for
  all the hits in that package (\code{TotalScore}), etc.
}
\usage{
findFn(string, maxPages = 20, sortby = NULL, verbose = 1, ...)
%findFn(string, maxPages = 20, sortby = NULL, verbose = 1,
%       omitPackages='^tmp/', ...)
##???string(maxPages)
}
\arguments{
  \item{string}{
    A character string. See \code{\link[utils]{RSiteSearch}}.
  }
  \item{maxPages}{
    The maximum number of pages to download assuming 20 links per page.
  }
  \item{sortby}{
    a character vector specifying how the \code{data.frame} returned
    should be sorted.  Default = c('Count', 'MaxScore', 'TotalScore',
    'Package', 'Score', 'Function') to sort descending on numerics and
    ascending on alphnumerics.  Specifying sortby = c('c', 't', 'm')
    is equivalent to c('Count', 'TotalScore', 'MaxScore', 'Package',
    'Score', 'Function').
  }
  \item{verbose}{
    an integer:  if 0, no output is printed to the console.  The
    default 1 displays an initial line with the number of pages to be
    retrieved and the number of matches obtained;  if the number of
    matches to be downloaded is less, this also is displayed on the
    initial line.  This is followed by a second line counting the pages
    downloaded.

    If greater than 1, additional information is provided on the
    download process.
  }
%   \item{e1}{ \code{string} }
%   \item{e2}{ \code{maxPages} }
  \item{\dots}{ignored}
}
\details{
  \code{findFn} searchs the help pages of packages covered
  by the \code{RSiteSearch} archives.  To restrict the search to only
  packages installed locally, use \code{help.search}.

  1.  Access the \code{RSitSearch} engine with \code{string},
  restricting to "functions", storing \code{Score}, \code{Package},
  \code{Function}, \code{Date}, \code{Description}, and \code{Link} in a
  \code{data.frame}.

  2.  Compute \code{Count}, \code{MaxScore} and \code{TotalScore} for
  each \code{Package} accessed.  Combine them in a matrix
  \code{PackageSummary}.

  3.  Sort \code{PackageSummary} in the order defined by the occurrance
  of c('Count', 'MaxScore', 'TotalScore', 'Package') in \code{sortby}.

  4.  Merge \code{PackageSummary} with the \code{data.frame} of search
  matches.

  5.  Sort the combined \code{data.frame} as defined by \code{sort.}.

  6.  Make the result have class c("findFn", "data.frame") and add
  attributes \code{matches}, \code{PackageSummary}, \code{string}, and
  \code{call}.

  7.  Done.
}
\value{
  an object of class c('findFn', 'data.frame') with columns and
  attributes as follows:

  \item{Columns}{

    \itemize{
      \item{Count}{ Total number of matches downloaded in this package}
      \item{MaxScore}{
	maximum of the \code{Score} over all help pages selected within
	each \code{Package}.  See \code{Score} below or the Namazu
	website (link below) for more information on how the score is
	determined.
      }
      \item{TotalScore}{
	sum of the \code{Score} over all help pages selected within
	each \code{Package}.  See \code{Score} below or the Namazu
	website (link below) for more information on how the score is
	determined.
      }
      \item{Package}{
	Name of the package containing a help page meeting the search
	criteria
      }
      \item{Function}{
	Name of the help page found that meets the indicated search
	criterion.
      }
      \item{Date}{ Date of the help page}
      \item{Score}{
	Score returned by \code{RSiteSearch}, discussed in the Namazu
	website (link below).
      }
      \item{Description}{ Title of the help page}
      \item{Link}{ Universal Resource Locator (URL) for the help page}
    }
  }

  \item{Attributes}{

    \itemize{
      \item{matches}{
	an integer = total number of matches found by the search.  This
	typically will exceed the number of rows found, because the
	search algorithm sometimes finds things that are not help pages
	for packages.
      }
      \item{PackageSummary}{
	a data.frame with one row for each package and columns
	\code{Package}, \code{Count}, \code{MaxScore}, \code{TotalScore},
	and \code{Date}, sorted as in the \code{sort.} argument.
      }
      \item{string}{
	the \code{string} argument in the \code{call}.
      }
      \item{call}{the matched call}
    }
  }
}
\author{
  Spencer Graves, Sundar Dorai-Raj, Romain Francois.  Duncan
  Murdoch suggested the "???" alias for "findFn" and 
  contributed the code for it.

  Special thanks to Jonathan Baron and Andy Liaw.  Baron maintains the
  RSiteSearch data base.  Liaw and Baron created the \code{RSiteSearch}
  function in the \code{utils} package.
}
\seealso{
  \code{\link{help.search}} to search only installed packages.
  \code{\link[utils]{RSiteSearch}},
  \code{\link[utils]{download.file}}
  \url{http://finzi.psych.upenn.edu/search.html} for a web 
    interface to this same search capability with more 
    general options.
  \code{findFn} searches only "Target:  Functions" from that 
    site, ignoring the R-help arcihves.

  \url{https://www.r-project.org/search.html} for a list of 
    alternative R search capabilities, each of which may be 
    best for different types of inquiries.

  \code{\link{findFunction}} for a completely different
  function with a similar name.
}
\references{
  \url{http://www.namazu.org/doc/tips.html.en#weight} - reference on
  determining \code{Score}
}
\examples{
# Skip these tests on CRAN,
# because they take more than 5 seconds
if(!fda::CRAN()){

z <- findFn("spline", maxPages = 2)
# alternative
zq <- ???spline(2)

\dontshow{stopifnot(}
all.equal(z, zq)
\dontshow{)}

# To search for 2 terms, not necessarily together:
RSS <- findFn('RSiteSearch function', 1)
matches(RSS)

# To search for an exact string, use braces:
RSS. <- findFn('{RSiteSearch function}', 1)
matches(RSS.) # list(nrow = 0, matches = 0)

# example in which resulting page has some unicode characters
Lambert <- findFn("Lambert")
Lambert
##
## Too many matches
##
fa <- findFn('factor analysis')
# Finds too many matches to process;
# reports Inf matches but returns none.
# When this happens, use a more restrictive search

# check
fa0 <- RSS.[seq(1, length=0),]
attr(fa0, 'matches') <- Inf
attr(fa0, 'PackageSummary') <- PackageSummary(fa0)
attr(fa0, 'string') <- 'factor+analysis'
attr(fa0, 'call') <- call('findFn', string = "factor analysis")

\dontshow{stopifnot(}
all.equal(fa, fa0)
\dontshow{)}
}
}
\keyword{misc}
