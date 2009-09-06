grepFn <- function(pattern, x,  column='Function',
       ignore.case=FALSE, extended=TRUE, perl=FALSE,
       value=TRUE, fixed=FALSE, useBytes=FALSE, invert=FALSE) {
##
## 1.  grep
##
  g <- grep(pattern, x[, column], ignore.case=ignore.case,
            extended=extended, perl=perl, value=FALSE, fixed=fixed,
            useBytes=useBytes, invert=invert)
##
## 2.  value?
##
  {
    if(value)return(x[g, , drop=FALSE]) else return(g)
  }
}
