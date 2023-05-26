\name{findFn}
\alias{findFn}
\alias{?}
\alias{???}
\title{
  Search Help Pages
}
\description{
  Returns a data.frame from
  \code{\link[utils]{RSiteSearch}(string, "function")} 
  which can be sorted and subsetted by user
  specifications and viewed in an \code{HTML} table.  
  The default sort puts first packages with the most
  matches (\code{Count}), with ties broken using 
  the sum of the match scores for all the hits in
  that package (\code{TotalScore}), etc.
}
\usage{
findFn(string, maxPages = 100, sortby = NULL, 
        verbose = 1, ...)
}
\arguments{
  \item{string}{
    A character string. See
    \code{\link[utils]{RSiteSearch}}.
  }
  \item{maxPages}{
    The maximum number of pages to download 
    assuming 20 links per page.
  }
  \item{sortby}{
    a character vector specifying how the 
    \code{data.frame} returned should be sorted. 
    Default = 
    \code{c('Count', 'MaxScore', 'TotalScore', 'Package', 'Score', 'Function')}
    to sort descending on numerics and
    ascending on alphanumerics.  Specifying 
    \code{sortby} = \code{c('c', 't', 'm')} is
    equivalent to 
    \code{c('Count', 'TotalScore', 'MaxScore', 'Package', 'Score', 'Function')}.
  }
  \item{verbose}{
    an integer:  if 0, no output is printed to the
    console.  The default 1 displays an initial 
    line with the number of pages to be retrieved 
    and the number of matches obtained;  if the
    number of matches to be downloaded is less, 
    this also is displayed on the initial line.  
    This is followed by a second line counting the
    pages downloaded.

    If greater than 1, additional information is
    provided on the download process.
  }
  \item{\dots}{ignored}
}
\details{
  \code{findFn} searches the help pages of packages
  covered by the \code{RSiteSearch} archives.  To
  restrict the search to only packages installed
  locally, use \code{\link{help.search}}.

  1.  Access the \code{\link{RSiteSearch}} engine 
  with \code{string}, restricting to "functions",
  storing \code{Score}, \code{Package}, 
  \code{Function}, \code{Date}, \code{Description},
  and \code{Link} in a \code{data.frame}.

  2.  Compute \code{Count}, \code{MaxScore} and
  \code{TotalScore} for each \code{Package} 
  accessed.  Combine them in a matrix
  \code{PackageSummary}.

  3.  Sort \code{PackageSummary} in the order 
  defined by the occurrence of 
  \code{c('Count', 'MaxScore', 'TotalScore', 'Package')}
  in \code{sortby}.

  4.  Merge \code{PackageSummary} with the 
  \code{data.frame} of search matches.

  5.  Sort the combined \code{data.frame} as 
  defined by \code{sort.}.

  6.  Make the result have class 
  \code{c("findFn", "data.frame")} and add
  attributes \code{matches}, 
  \code{PackageSummary}, \code{string}, and
  \code{call}.

  7.  Done.
}
\value{
  an object of class 
  \code{c('findFn', 'data.frame')} with columns 
  and attributes as follows:

  \item{\code{Columns}}{

    \itemize{
      \item{\code{Count}}{ Total number of matches 
        downloaded in this package}
      \item{\code{MaxScore}}{
	      maximum of the \code{Score} over all help
	      pages selected within each \code{Package}. 
	      See \code{Score} below or the Namazu
	     	website (link below) for more information 
	     	on how the score is determined.
      }
      \item{\code{TotalScore}}{
      	sum of the \code{Score} over all help 
      	pages selected within each \code{Package}. 
      	See \code{Score} below or the Namazu
      	website (link below) for more information 
      	on how the score is determined.
      }
      \item{\code{Package}}{
      	Name of the package containing a help 
      	page meeting the search criteria
      }
      \item{\code{Function}}{
      	Name of the help page found that meets 
      	the indicated search criterion.
      }
      \item{\code{Date}}{Date of the help page}
      \item{\code{Score}}{
      	Score returned by \code{RSiteSearch},
      	discussed in the Namazu	website (link 
      	below).
      }
      \item{\code{Description}}{ 
        Title of the help page
      }
      \item{\code{Link}}{ 
        Universal Resource Locator (URL) for the 
        help page
      }
    }
  }

  \item{\code{Attributes}}{

    \itemize{
      \item{\code{matches}}{
      	an integer = total number of matches found 
      	by the search.  This typically will exceed
      	the number of rows found, because the
      	search algorithm sometimes finds things 
      	that are not help pages for packages.
      }
      \item{\code{PackageSummary}}{
      	a \code{data.frame} with one row for each 
      	package and columns \code{Package},
      	\code{Count}, \code{MaxScore}, 
      	\code{TotalScore}, and \code{Date}, sorted 
      	as in the \code{sort.} argument.
      }
      \item{\code{string}}{
      	the \code{string} argument in the 
      	\code{call}.
      }
      \item{\code{call}}{the matched call}
    }
  }
}
\author{
  Spencer Graves, Sundar Dorai-Raj, Romain Francois.
  Duncan Murdoch suggested the "???" alias for
  \code{findFn} and contributed the code for it.
  
  Special thanks to Gennadiy Starostin, 
  Vienna University of Economics and Business
  (Wirtschaftsuniversitaet Wien), who in early 
  2021 took over maintenance of the 
  \code{\link{RSiteSearch}} data base, updated 
  its structure, and rewrote 
  \code{findFn} to match.  

  Special thanks to Jonathan Baron, and Andy 
  Liaw. Baron maintained the 
  \code{RSiteSearch} data base for many years.  
  Liaw and Baron created the \code{RSiteSearch} 
  function in the \code{utils} package. 
}
\seealso{
  \code{\link{help.search}} to search only 
      installed packages.
  \code{\link[utils]{RSiteSearch}},
  \code{\link[utils]{download.file}}
  \code{findFn} searches only "Target:  Functions"
    from that site, ignoring the R-help archives.

  For alternative R search capabilities, see:  

  * \href{https://en.wikiversity.org/wiki/Searching_R_Packages}{"Searching R Packages" on Wikiversity}
  
  * \href{https://journal.r-project.org/archive/2018/RJ-2018-058/index.html}{Julia Silge, John C. Nash, and Spencer Graves (2018) Navigating the R Package Universe, The R Journal, 10(2) 558-563.}
  
  * \url{https://search.r-project.org} for 
    a list of alternative R search capabilities, 
    each of which may be best for different types 
    of inquiries.

  * \code{\link{findFunction}} for a completely
  different function with a similar name.
}
\references{
  \url{http://www.namazu.org/doc/tips.html.en#weight}
  - reference on determining \code{Score}
}
\examples{
# Skip these tests on CRAN,
# because they take more than 5 seconds
if(!CRAN()){

z <- try(findFn("spline", maxPages = 2))
# alternative
zq <- try(???spline(2))

# Confirm z == zq except for 'call' 
  attr(z, 'call') <- NULL
  attr(zq, 'call') <- NULL

if(!(inherits(z, "try-error") ||
     inherits(zq, "try-error"))){

\dontshow{stopifnot(}
all.equal(z, zq)
\dontshow{)}

# To search for 2 terms, not necessarily together:
RSS <- try(findFn('RSiteSearch function', 1))
matches(RSS)

# To search for an exact string, use braces:
RSS. <- try(findFn('{RSiteSearch function}', 1))
matches(RSS.) # list(nrow = 0, matches = 0)

# example in which resulting page has some unicode characters
Lambert <- try(findFn("Lambert"))
Lambert

# Example that "found  2  link(s) without dates" on 2021-06-26
webScr <- try(findFn('web scraping'))

# Example that "found 0 matches" on 2021-09-06
try(findFn('{open history map}'))
}
}
}
\keyword{misc}
