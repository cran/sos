## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sos)

## ----Petal.Len----------------------------------------------------------------
(Petal.Length <- help.search('Petal.Length'))

## ----PL.RSiteSearch-----------------------------------------------------------
library(sos)
if(!CRAN()){
  RSiteSearch('Petal.Length')
}

## ----PetalLen_sos-------------------------------------------------------------
library(sos)
PL <- findFn('Petal.Length')
class(PL)
dim(PL)

## ----PL_sos-------------------------------------------------------------------
PL. <- ???Petal.Length
class(PL.)
dim(PL.)

## ----summary.PL,results='hide'------------------------------------------------
# the following table has been
# manually edited for clarity
summary(PL)

## ----summary.PL-print, echo=FALSE---------------------------------------------
s <- summary(PL)
blank <- data.frame(Package = "<...>",
      Count = "", MaxScore = "", TotalScore = "",
      Date = "", pkgLink='')
s$PackageSummary[] <- lapply(s$PackageSummary[], as.character)
row.names(s$PackageSummary) <-
  as.character(s$PackageSummary$Package)
s$PackageSummary <- rbind(s$PackageSummary['yaImpute', ],
                          blank,
                          s$PackageSummary['datasets', ],
                          blank)
print(s, row.names = FALSE)

## ----Petal.Length.sos.3,results='hide'----------------------------------------
PL[PL$Package == 'datasets', 'Function']

## ----Petal.Length.sos.3-print,echo=FALSE--------------------------------------
print(PL[PL$Package == 'datasets', 'Function'], max.levels = 0)

## ----RSiteSearch-spline,results='hide'----------------------------------------
if(!CRAN()){
  RSiteSearch('spline')
}

## ----RSiteSearch-spline-numpages,results='hide',echo=FALSE--------------------
getRSiteSearchHits <- function(description) {
  today <- format(Sys.time(), "%Y-%m-%d")
  con <- url(description)
  on.exit(close(con))
  lines <- try(readLines(con))
  if(class(lines) == 'try-error'){
    return(list(hits=0, date=today))
  }
  pattern <- "^.*<!-- HIT -->([0-9]+)<!-- HIT -->.*$"
  hits <- sub(pattern, "\\1", lines[grep(pattern, lines)])
  list(hits = hits, date = today)
}
splineHits <- getRSiteSearchHits("http://search.r-project.org/cgi-bin/namazu.cgi?query=spline&max=20&result=normal&sort=score&idxname=Rhelp08&idxname=functions&idxname=views")

## ----RSiteSearch-spline-fun, results='hide'-----------------------------------
if(!CRAN()){
  RSiteSearch('spline', 'fun')
}

## ----RSiteSearch-spline-fun-numpages,results='hide',echo=FALSE----------------
splineFunHits <- getRSiteSearchHits("http://search.r-project.org/cgi-bin/namazu.cgi?query=spline&max=20&result=normal&sort=score&idxname=functions")

## ----RSiteSearch-spline-fun2,results='hide'-----------------------------------
if(!CRAN()){
  RSiteSearch('spline', 'fun')
}

## ----RSiteSearch-spline-fun2-numpages,results='hide',echo=FALSE---------------
splineFunHits <- getRSiteSearchHits("http://search.r-project.org/cgi-bin/namazu.cgi?query=spline&max=20&result=normal&sort=score&idxname=functions")

## ----sos-spline-maxPages-999,results='hide'-----------------------------------
splineAll <- findFn('spline', maxPages = 999)

## ----sos-spline-subset,results='hide'-----------------------------------------
selSpl <- splineAll[, 'Function'] == 'spline'
splineAll[selSpl, ]

## ----sos-spline-grep,results='hide'-------------------------------------------
if(!CRAN()){
  grepFn('spline', splineAll, ignore.case = TRUE)
}

## ----sos-spline-grep2,results='hide',echo=FALSE-------------------------------
g <- grepFn('spline', splineAll, ignore.case = TRUE)
gFunc6 <- as.character(g[6, "Function"])
gPac6 <- as.character(g[6, "Package"])
gScore6 <- g[6, "Score"]
gCount6 <- g[6, "Count"]
# Apparently, nlevels(splineAll$Package)
# splineAll$Package used to be a factor; not it's not
nPacSplineAll <- length(table(splineAll$Package))

## ----writeFindFn2xls-options,echo=FALSE---------------------------------------
op <- options(width = 80)

## ----writeFindFn2xls,results='hide'-------------------------------------------
writeFindFn2xls(splineAll)

## ----writeFindFn2xls-options2,echo=FALSE--------------------------------------
options(op)

## ----install-and-write-options,echo=FALSE-------------------------------------
op <- options(width=80)

## ----install-and-write,results='hide'-----------------------------------------
splineAll <- findFn('spline', maxPages = 999)
# Do not include in auto test
#installPackages(splineAll)
writeFindFn2xls(splineAll)

## ----install-and-write-options-undo,echo=FALSE--------------------------------
options(op)

## ----differntial-equations,results='hide'-------------------------------------
de <- findFn('differential equation')
des <- findFn('differential equations')
de. <- de | des

