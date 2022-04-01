\name{CRAN}
\alias{CRAN}
\title{
  Test if running as CRAN
}
\description{
  This function allows package developers to run tests 
  themselves that should not run on CRAN or with 
  \code{"R CMD check --as-cran"} because of compute 
  time constraints with \code{CRAN} tests.
}
\usage{
CRAN(CRAN_pattern, n_R_CHECK4CRAN)
}
\arguments{
  \item{CRAN_pattern}{
    a regular expressions to apply to the names of 
    \code{Sys.getenv()} to identify possible 
    \code{CRAN} parameters.  Defaults to 
    \code{Sys.getenv('_CRAN_pattern_')} if available 
    and \code{'^_R_'} if not.
  }
  \item{n_R_CHECK4CRAN}{
    Assume this is \code{CRAN} if at least 
    \code{n_R_CHECK4CRAN} elements of 
    \code{Sys.getenv()} have names matching 
    x.  Defaults to
    \code{Sys.getenv('_n_R_CHECK4CRAN_')} if 
    available and 5 if not.
  }
}
\details{
  The "Writing R Extensions" manual says that 
  \code{"R CMD check"} can be customized 
  "by setting environment variables 
  \code{_R_CHECK_*_:}, as described in" the 
  Tools section of the "R Internals" manual.

  \code{'R CMD check'} was tested with 
  R 3.0.1 under Fedora 18 Linux and with
  \code{Rtools 3.0} from April 16, 2013 under 
  Windows 7.  With the \code{'--as-cran'}
  option, 7 matches were found;  without it, 
  only 3 were found.  These numbers were 
  unaffected by the presence or absence of the 
  '--timings' parameter.  On this basis, the 
  default value of \code{n_R_CHECK4CRAN} was 
  set at 5.

  1.  \code{x. <- Sys.getenv()}

  2.  Fix \code{CRAN_pattern} and 
  \code{n_R_CHECK4CRAN} if missing.

  3.  Let \code{i} be the indices of 
  \code{x.} whose names match all the patterns 
  in the vector \code{x.}

  4.  Assume this is \code{CRAN} if 
  \code{length(i) >= n_R_CHECK4CRAN}.

}
\value{
  a logical scalar with attributes 
  \code{'Sys.getenv'} containing the 
  results of \code{Sys.getenv()} and 
  'matches' containing \code{i} per step 
  3 above.
}
\author{
  Spencer Graves (copied from the 
  \code{fda} package)
}
\seealso{
  \code{\link{Sys.getenv}}
  \code{\link[testthat]{skip_on_cran}}, which uses 
  ["the NOT_CRAN env var set by devtools and friends"](https://testthat.r-lib.org/reference/skip.html).  This 
  \code{CRAN} function does NOT require a user to 
  set any environment variable. 
}
\examples{
% Look in sos-ex.Rout
% for the results from R CMD check
% Modified defaults were tested with environment variables
% _CRAN_pattern_ = 'A' and _n_R_CHECK4CRAN_ = '10'
% 'R CMD check' found 26 matches and CRAN() returned TRUE
cran <- CRAN()
str(cran)
gete <- attr(cran, 'Sys.getenv')
(ngete <- names(gete))

iget <- grep('^_', names(gete))
gete[iget]

#\dontrun is sometimes run on CRAN.  See
#https://github.com/ThinkR-open/prepare-for-cran
#accessed 2021-06-14
if (interactive()) {
if(CRAN()){
  stop('CRAN')
} else {
  stop('NOT CRAN')
}
}
}
\keyword{environment}

