findFn2xls <- function(x,
           file.=paste(deparse(substitute(x)), 'xls', sep='.'),
           csv, ...) {
  writeFindFn2xls(x, file.=file., csv=csv, ...)
}

writeFindFn2xls <- function(x,
           file.=paste(deparse(substitute(x)), 'xls', sep='.'),
           csv, ...) {
###
###
### I.  Define internal function
###
###
  wFF2x <- function(x,
           file.=paste(deparse(substitute(x)), 'xls', sep='.'),
           csv, ...) {
##
## 1.  exists(file.)?
##
    if(file.exists(file.))file.remove(file.)
    if(nrow(x)<1){
      cat('No matches;  nothing written.\'n')
      return(invisible(''))
    }
##
## 2.  csv?
##
    sum2 <- PackageSum2(x)
    sum2$Date <- as.character(as.Date(sum2$Date))
    cl <- data.frame(call=as.character(attr(x, 'call')),
                     stringsAsFactors=FALSE)
    writeFindFn2csv <- function(x, file., sum2.=sum2, cl.=cl, ...){
      f.xls <- regexpr('\\.xls', file.)
      if(f.xls>0)file. <- substring(file., 1, f.xls-1)
#
      file3 <- paste(file., c('-sum.csv', '.csv', '-call.csv'), sep='')
      write.csv(sum2., file3[1], ...)
      write.csv(x, file3[2], ...)
      write.csv(cl., file3[3], ...)
      'done'
    }
    if((!missing(csv)) && csv){
      writeFindFn2csv(x, file., ...)
      return(invisible(file.))
    }
##
## 3.  Will WriteXLS work?
##
    x2 <- lapply(x, function(x)
               if(is.numeric(x)) x else as.character(x))
    x2. <- as.data.frame(x2)
    if(require(WriteXLS) && testPerl()) {
      WriteXLS(c('sum2', 'x2.', 'cl'), ExcelFileName=file.,
             SheetNames=c('PackageSum2', 'findFn', 'call') )
      return(invisible(file.))
    }
##
## 4.  How about RODBC?
##
    if(require(RODBC)){
      if(!(.Platform$OS.type=="windows")){
        warning('Does not work on non-Windows platform without ',
                'WriteXLS;  writing csv files')
        writeFindFn2csv(x, file., ...)
        return(invisible(file.))
      }
      xlsFile <- odbcConnectExcel(file., readOnly=FALSE)
      on.exit( odbcClose(xlsFile) )
    } else {
      warning('RODBC not available and WriteXS will not work;  ',
              'writing csv files')
      writeFindFn2csv(x, file., ...)
      return(invisible(file.))
    }
##
## 5.  Create the sheets
##
    sum2. <- sqlSave(xlsFile, sum2, tablename='PackageSum2')
    x. <- sqlSave(xlsFile, as.data.frame(x2), tablename='findFn')
#
    cl. <- sqlSave(xlsFile, cl, tablename='call')
##
## 6.  Done
##
    return(invisible(file.))
  }
###
###
### II.  Do
###
###
  wFF <- try(wFF2x(x, file., csv, ...))
  if(class(wFF)=='try-error'){
    cat('error converted to warning;  writing 2 csv files\n')
    wFF <- wFF2x(x, file., csv=TRUE, ...)
  }
  wFF
}
