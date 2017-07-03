packageSum <- function(x,
    fields=c("Title", "Version", "Author",
       "Maintainer", "Packaged", 'helpPages',
       'vignette', 'URL'), 
    lib.loc=NULL, ...){
#  cat('packageSum generic\n')
  UseMethod('packageSum')
}

packageSum.findFn <- function(x,
    fields=c("Title", "Version", "Author",
        "Maintainer", "Packaged", 'helpPages',
        'vignette', 'URL'), 
    lib.loc=NULL, ...){
#  cat('packageSum.findFn: enter\n')
  ps2 <- packageSum(attr(x, 'PackageSummary'), 
             fields, lib.loc, ...)
  if(is.null(attr(ps2, 'call')))
    attr(ps2, 'call') <- attr(x, 'call')
  if(is.null(attr(ps2, 'string')))
    attr(ps2, 'string') <- attr(x, 'string')
  ps2 
}

packageSum.list <- function(x,
    fields=c("Title", "Version", "Author",
        "Maintainer", "Packaged", 'helpPages',
        'vignette', 'URL'), 
    lib.loc=NULL, ...){
#  cat('packageSum.list: enter\n')
  ps2 <- packageSum(x$PackageSummary, fields, 
             lib.loc, ...)
  if(is.null(attr(ps2, 'call')))
    attr(ps2, 'call') <- attr(x, 'call')
  if(is.null(attr(ps2, 'string')))
    attr(ps2, 'string') <- attr(x, 'string')
  class(ps2) <- c("packageSum", "data.frame")
  ps2 
}

packageSum.data.frame <- function(x,
    fields=c("Title", "Version", "Author", 
      "Maintainer", "Packaged", 'helpPages',
      'vignette', 'URL'), 
    lib.loc=NULL, ...){
##
## 1.  PackageSum2
##
#  cat('packageSum.data.frame: enter\n')
  ps2 <- PackageSum2(x, fields=fields, 
                     lib.loc=NULL, ...)
  if(is.null(attr(ps2, 'call')))
    attr(ps2, 'call') <- attr(x, 'call')
  if(is.null(attr(ps2, 'string')))
    attr(ps2, 'string') <- attr(x, 'string')
##
## 2.  assign class 
##
  class(ps2) <- c("packageSum", "data.frame")
  ps2
}
