"[.findFn" <- function(x, i, j,
    drop = if (missing(i)) TRUE else length(j) == 1) {
##
## 1.  missing(j)
##
  class(x) <- 'data.frame'
  if(missing(j)){
    if(missing(i))return(x[, , drop=TRUE])
# We do not know if this is x[i] or x[i,].
    sysCall <- sys.call()
    sysCall. <- deparse(substitute(sysCall))  
    sysC4 <- strsplit(sysCall., ',')[[1]]
# For df2[1], sysC4 = chr[1:2] "`.findFn`(df2", " 1)"    
# For df2[1, ], sysC4 = chr[1:3] 
#   "`.findFn`(df2", " 1", ")"
# For df2[1, drop=FALSE], sysC4 = chr[1:3]
#   "`.findFn`(df2", " 1", " drop = FALSE)"
    if(length(sysC4)>2){
      xi <- x[i, , drop=FALSE]
      attr(xi, 'PackageSummary') <- PackageSummary(xi)
      class(xi) <- c("findFn", 'data.frame')
    } else {
      xi <- x[, i, TRUE]
    }
    return(xi)
  }
##
## 2.  select columns, return data.frame
##
  x[i, j, drop=drop]
}
