PackageSummary <- function(x, sortby=NULL){
##
## 1.  Convert Package to character to avoid corruption
##     from any unused levels
  xP <- as.character(x$Package)
##
## 2.  Count, maxSc, totSc
##
  Count <- tapply(rep(1,nrow(x)), xP, length)
  Sc <- as.numeric(as.character(x$Score))
  maxSc <- tapply(Sc, xP, max)
  totSc <- tapply(Sc, xP, sum)
##
## 3.  Find the first occurrance of each Package to get Date,
##     which would be corrupted by tapply
##
  iP <- tapply(seq(1, length=nrow(x)), xP, function(x)x[1])
  pkgSum <- data.frame(Package=xP[iP], Count=as.numeric(Count),
                    MaxScore=as.numeric(maxSc),
                    TotalScore=as.numeric(totSc), Date=x$Date[iP],
                    stringsAsFactors=FALSE)
  L. <- x$Link
  if(is.null(L.)||(length(L.)<1)){
    L1 <- character(length(iP))
  } else {
    L0 <- strsplit(L.[iP], '/')
    L1 <- sapply(L0, function(x){
      paste(c(utils::head(x, -1), '00Index.html'), collapse='/')
    })
  }
  pkgSum$pkgLink <- L1
##
## 4.  Sort
##
  s0 <-  c('Count', 'MaxScore', 'TotalScore', 'Package',
           'Score', 'Function', 'Date', 'Description', 'pkgLink')
  s0. <- tolower(s0)
  {
    if(is.null(sortby)) sortby <-  s0
    else {
      s1 <- match.arg(tolower(sortby), s0., TRUE)
      s1. <- c(s1, s0.[!(s0. %in% s1)])
      names(s0) <- s0.
      sortby <- s0[s1.]
    }
  }
  pkgSort <- sortby[sortby %in%
                   c('Count', 'MaxScore', 'TotalScore', 'Package')]
  pkgKey <- with(pkgSum,
                 data.frame(Package, Count=-Count, MaxScore=-MaxScore,
                            TotalScore=-TotalScore))
  o <- do.call('order', pkgKey[pkgSort])
  pkgSum[o, ]
}
