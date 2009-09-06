PackageSum2 <- function(x,
      fields=c("Title", "Version", "Author", "Maintainer", "Packaged",
          'helpPages', 'vignette'), lib.loc=NULL, ...){
  UseMethod('PackageSum2')
}

PackageSum2.findFn <- function(x,
      fields=c("Title", "Version", "Author", "Maintainer", "Packaged",
          'helpPages', 'vignette'), lib.loc=NULL, ...){
  PackageSum2(attr(x, 'PackageSummary'), fields, lib.loc, ...)
}

PackageSum2.list <- function(x,
      fields=c("Title", "Version", "Author", "Maintainer", "Packaged",
          'helpPages', 'vignette'), lib.loc=NULL, ...){
  PackageSum2(x$PackageSummary, fields, lib.loc, ...)
}

PackageSum2.data.frame <- function(x,
      fields=c("Title", "Version", "Author", "Maintainer", "Packaged",
          'helpPages', 'vignette'), lib.loc=NULL, ...){
##
## 1.  Create character matrix for fields
##
  nf <- length(fields)
  nx <- nrow(x)
#  xout <- x
#  for(ic in seq(1, length=nf))xout[[fields[ic]]] <- rep('', nx)
  xP <- as.character(x$Package)
  xnew <- matrix('', nx, nf, dimnames=list(xP, fields))
##
## 2.  installed packages?
##
  instPkgs <- .packages(TRUE)
##
## 3.  Get packageDescription for each package
##
  for(ip in seq(1, length=nx)){
    if(xP[ip] %in% instPkgs){
      pkgDesci <- packageDescription(x$Package[ip], lib.loc=lib.loc)
      pkgHelp <- help(pac=x$Package[ip], lib.loc=lib.loc)
      for(ic in seq(1, length=nf)){
        if(fields[ic] == "Packaged"){
          {
            if(is.null(pkgDesci$Packaged))
              pkgd <- (strsplit(pkgDesci$Built, ';')[[1]][3])
            else
              pkgd <- (strsplit(pkgDesci$Packaged, ';')[[1]][1])
          }
          xnew[ip, ic] <- pkgd
#          xout$Packaged[ip] <- pkgd
        }
        else
          if(fields[ic] %in% names(pkgDesci))
            xnew[ip, ic] <- pkgDesci[[fields[ic]]]
#            xout[ip, fields[ic]] <- pkgDesci[[fields[ic]]]
        else {
          if(fields[ic] == 'helpPages'){
            helpInfo2 <- pkgHelp$info[[2]]
            nhr <- (length(helpInfo2) -
                    sum(substring(helpInfo2, 1, 1) == ' '))
            xnew[ip, ic] <- nhr
          }
          else {
            if(fields[ic] == 'vignette') {
              vig <- (vignette(package=x$Package[ip])$results)
              {
                if(nrow(vig)>1){
                  clps <- paste(vig[, 'Item'], collapse=', ')
                  xnew[ip, ic] <- paste(nrow(vig), clps, sep=':  ')
                }
                else if(nrow(vig)>0)
                  xnew[ip, ic] <- vig[ 1, 'Title']
              }
            }
          }
        }
      }
    }
  }
##
## 4.  Done
##
  cbind(x, as.data.frame(xnew, stringsAsFactors=FALSE))
}
