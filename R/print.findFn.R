print.findFn <- function(x,
    where, title,
    openBrowser = TRUE,
    template,  ...) {
##
## 0.  If x has 0 rows, don't go
##    further ...
##
  cl <- deparse(substitute(x)) 
#  print(cl)
  if (nrow(x) < 1) {
    cat("x has zero rows;  ", 
        "nothing to display.\n")
    if (missing(where))
      where <- ""
    return(invisible(where))
  }
##
## 1.  where?
##
  if (missing(where))
    where <- "HTML"
  ##
  if (length(where) == 1 && 
      where == "console")
    where <-  c("Count", "Package",
        "Function", "Score", "Date")
  ##
  if (all(where %in% names(x))) {
    print.data.frame(x[, where])
    return(invisible(""))
  }
  if (length(where)>1)
    stop("if length(where)>1, ", 
      "where must be names of columns of", 
      " x;  they are not.  where = ",
         paste(where, collapse=", "))
  if (toupper(where) == "HTML") {
    f0 <- tempfile()
    for(i in 1:111) {
      File <- paste(f0, ".html", 
                    sep = "")
      fInf <- file.info(File)
      if(all(is.na(fInf)))
        break
      ## file exists so try another
      f0 <- paste(f0, "1", sep="")
    }
  } else {
    File <- where
  }
##
## 2.  title, Dir?
##
  string <- attr(x, "string")
  if (missing(title)) {
    title <- paste('Help pages for', string)
    titSum <- paste('package summary for', string)
  } else {
    titSum <- paste('packageSum(', string, ')')
  }
  Dir <- dirname(File)
  if (Dir == ".") {
    Dir <- getwd()
    File <- file.path(Dir, File)
  } else {
    dc0 <- dir.create(Dir, FALSE, TRUE)
  }
##
## 3.  print(packageSum(...))
##   ... moved to the end 
#  if((toupper(where)=='HTML') && openBrowser){
#    sumLink <- print(packageSum(x, title=title, ...))
#  } else sumLink <- ''
##
## 4.  Get call including search
##     string
##
#  cl <- match.call()
  Ocall <- attr(x, "call")
  Oc0 <- deparse(Ocall)
  Oc. <- gsub('\"', "'", Oc0)
#  if(Oc.=='NULL')Oc. <- '...'
  Oc.[Oc.=='NULL'] <- '...'
  Oc1 <- paste(cl, "<-", paste(Oc., collapse=''))
#  Oc2 <- paste0('For a package summary:  ', 
#  Oc2 <- paste0('For more info, call installPackages', 
#          ' before packageSum')
  pkgS <- paste0('packageSum(', cl, ',...)')
#  ocall <- paste(cl, "<-", Oc1)
#  ocall <- parse(text=Ocx)
##
## 5.  sorttable.js?
##
##  Dir <- tools:::file_path_as_absolute( dirname(File) )
##  This line is NOT ENOUGH:
##     browseURL(File) needs the full path in File
  js <- system.file("js", "sorttable.js", package = "sos")
  if (!file.exists(js)) {
    warning("Unable to locate 'sorttable.js' file")
  } else {
    ##*** Future:
    ## Replace "Dir\js" with a temp file
    ## that does not exist, then delete it on.exit
    file.copy(js, Dir)
  }
##
## 6.  Modify x$Description
##
## save "x" as "xin" for debugging
  xin <- x
# Allow x to have a NULL Description 
# to simplify testing of other sos functions   
  Desc <- x$Description 
  if(is.null(Desc))Desc <- ''
  x$Description <- gsub("(^[ ]+)|([ ]+$)", 
      "", as.character(Desc), useBytes = TRUE)
  x[] <- lapply(x, as.character)
##
## 7.  template for brew?
##
  hasTemplate <- !missing(template)
  if (!hasTemplate) {
    templateFile <- system.file("brew",
        "default", "results.brew.html",
        package = "sos")
    template <- file(templateFile, 
        encoding = "utf-8", open = "r" )
  }
## "brew( template,  File )" malfunctioned;
## try putting what we need in a special environment
  xenv <- new.env()
# str(ocall)
#language findFn(string = "spline", maxPages = 1)
  assign("Oc1", Oc1, envir = xenv)
#  assign("Oc2", Oc2, envir = xenv)
  assign('pkgS', pkgS, envir=xenv)
#  assign('sumLink', sumLink, envir=xenv)
#  ocall <- paste0(Oc1, '; ', Oc2)
#  assign("ocall", ocall, envir = xenv)
  assign("x", x, envir = xenv)
  ##
  brew::brew(template, File, envir = xenv)
  if (!hasTemplate) {
    close(template)
  }
##
## 8.  Was File created appropriately?  
##       If no, try Sundar's original code
##
  FileInfo <- file.info(File)
  if (is.na(FileInfo$size) || FileInfo$size <= 0) {
    if (is.na(FileInfo$size)) {
      warning("Brew did not create file ", File)
    } else {
      warning("Brew created a file of size 0")
    }
    cat("Ignoring template.\n")
## Sundar's original construction:
    con <- file(File, "wt")
    on.exit(close(con))
    .cat <- function(...)
      cat(..., "\n", sep = "", file = con, append = TRUE)
    ## start
    cat("<html>", file = con)
    .cat("<head>")
    .cat("<title>", title, "</title>")
    .cat("<script src=sorttable.js type='text/javascript'></script>")
    ## Set up ??? ... with a multiline quote :(  :(  :(
    .cat("<style>\n",
         "table.sortable thead {\n",
         "font: normal 10pt Tahoma, Verdana, Arial;\n",
         "background: #eee;\n",
         "color: #666666;\n",
         "font-weight: bold;\n",
         "cursor: hand;\n",
         "}\n",
         "table.sortable th {\n",
         "width: 75px;\n",
         "color: #800;\n",
         "border: 1px solid black;\n",
         "}\n",
         "table.sortable th:hover {\n",
         "background: #eea;\n",
         "}\n",
         "table.sortable td {\n",
         "font: normal 10pt Tahoma, Verdana, Arial;\n",
         "text-align: center;\n",
         "border: 1px solid blue;\n",
         "}\n",
         ".link {\n",
         "padding: 2px;\n",
         "width: 600px;\n",
         "}\n",
         ".link:hover {\n",
         "background: #eeb;\n",
         "}\n",
         "a {\n",
         "color: darkblue;\n",
         "text-decoration: none;\n",
         "border-bottom: 1px dashed darkblue;\n",
         "}\n",
         "a:visited {\n",
         "color: black;\n",
         "}\n",
         "a:hover {\n",
         "border-bottom: none;\n",
         "}\n",
         "h1 {\n",
         "font: normal bold 20pt Tahoma, Verdana, Arial;\n",
         "color: #00a;\n",
         "text-decoration: underline;\n",
         "}\n",
         "h2 {\n",
         "font: normal bold 12pt Tahoma, Verdana, Arial;\n",
         "color: #00a;\n",
         "}\n",
         "table.sortable .empty {\n",
         "background: white;\n",
         "border: 1px solid white;\n",
         "cursor: default;\n",
         "}\n",
         "</style>\n",
         "</head>")
    ##  Search results ... ???
    .cat("<h1>findFn Results</h1>")
# str(ocall)
#language findFn(string = "spline", maxPages = 1)    
    .cat("<h2>call: <font color='#800'>",
         paste(Ocall, collapse = ""), "</font></h2>\n")
    .cat("<table class='sortable'>\n<thead>")
    link <- as.character(x$Link)
    desc <- gsub("(^[ ]+)|([ ]+$)", "", as.character(x$Description), useBytes = TRUE)
    x$Link <- sprintf("<a href='%s' target='_blank'>%s</a>", link, desc)
    x$Description <- NULL
    ## change "Link" to "Description and Link"
    ilk <- which(names(x) == "Link")
    names(x)[ilk] <- "Description and Link"
    ##
    .cat("<tr>\n  <th style='width:40px'>Id</th>")
    .cat(sprintf("  <th>%s</th>\n</tr>",
                 paste(names(x), collapse = "</th>\n  <th>")))
    .cat("</thead>\n<tbody>")
    paste.list <- c(list(row.names(x)),
                    lapply(x, as.character), sep = "</td>\n  <td>")
    tbody.list <- do.call("paste", paste.list)
    tbody <- sprintf("<tr>\n  <td>%s</td>\n</tr>", tbody.list)
    tbody <- sub("<td><a", "<td class=link><a", tbody, useBytes = TRUE)
    .cat(tbody)
    ## another (shorter) multiline thingy ???
    .cat("</tbody></table></body></html>")
  }
  ##
  ## 9.  Display in a browser?
  ##
  if (openBrowser) {
    FileInf2 <- file.info(File)
    if (is.na(FileInf2$size)) {
      warning("Did not create file ", File,
              ";  nothing to give to a browser.")
    } else {
      if (FileInf2$size <= 0) {
        warning("0 bytes in file ", File, ";  nothing to give to a browser.")
      } else {
#       display HTML "File";  browseURL accesses 
#          variable(s) in this calling function 
#          like 'title'   
        utils::browseURL(File)
        pS <- packageSum(x, title=titSum, ...)
        print(pS)
      }
    }
  }
  invisible(File)
}
