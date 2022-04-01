findFn <- function(string,
                   maxPages = 100,
                   sortby = NULL,
                   verbose = 1, ...) {
##
## 1.  Define internal function
## 
  parseLinks <- function(links) {
    lnk <- paste0("https://search.r-project.org",sub(".*url=\\\"(.*)\\\" .*$", 
               "\\1", links, useBytes = TRUE))
    desc <- sub(".*title=\\\"(.*)\\\".*$", 
                 "\\1", links, useBytes = TRUE)
    nl <- length(lnk)
    nd <- length(desc)
    if((nd>0) && (nl != nd)){
      msg <- paste0('ERROR: length(desc) = ', nd, 
          ' != length(lnk) = ', nl, '\n')
      print(list(lnk, desc))
      stop(msg)
    }
#       
    nld <- min(nl, nd)
    ild <- seq(1, length=nld)
    list(link = lnk[ild], description = desc[ild])
  }
#  
  parseHTML <- function(href) {
    link <- try(url(href))
    on.exit(close(link))
    if (inherits(link, "try-error")) {
      warning("An error occurred opening ", href,
              "\nfindFn needs Internet access;  is it available?")
      ch0 <- character(0)
      ans <- data.frame(Package = ch0,
                        Function = ch0,
                        Date = ch0,
                        Score = numeric(0),
                        Description = ch0,
                        Link = ch0, stringsAsFactors=FALSE)
      attr(ans, "matches") <- 0
      return(ans)
    }
    html <- try(readLines(link))
    if (inherits(html, "try-error")) {
      warning("An error occurred in readLine(link), link = ", link,
              "\nfindFn needs Internet access;  is it available?")
      ch0 <- character(0)
      ans <- data.frame(Package = ch0,
                        Function = ch0,
                        Date = ch0,
                        Score = numeric(0),
                        Description = ch0,
                        Link = ch0, stringsAsFactors=FALSE)
      attr(ans, "matches") <- 0
      return(ans)
    }
#   Find the hit count
    hitRows <- html[grep("^<results .* Matches=", html, useBytes = TRUE)]
    Hits <- as.numeric(sub("^<results .*Matches=\\\"(.*)\\\" .*$", "\\1", hitRows, useBytes = TRUE))
#   Find dates
    dateRows <- grep("^<hit .* modtime=", html, useBytes = TRUE)
    linkRows <- grep(".*url=", html, useBytes = TRUE)
#   Assume linkRows == (dateRows+1)
#   Look for links without Dates
    linkWdate <- (linkRows %in% (dateRows+1))
    nlWOd <- sum(!linkWdate) 
    if(nlWOd>0){
      cat('Note: found ', nlWOd, ' link(s) without dates:\n')
      for(lWOd in linkRows[!linkWdate]){
        cat(html[lWOd], '\n')
      }
    }
    dateWlink <- (dateRows %in% (linkRows-1))
    ndWOl <- sum(!dateWlink)
    if(ndWOl>0){
      cat("Note: found ', ndWOl, ' date(s) without links:\n")
      for(dWOl in dateRows[!dateWlink]){
        cat(html[dWOl], '\n')
      }
    }
    DateRows <- html[dateRows[dateWlink]]
    Date <- as.numeric(sub("^<hit .*modtime=\\\"(.*)\\\".*$", "\\1", DateRows, useBytes = TRUE))
#    
    linksRows <- html[linkRows[linkWdate]]
#      
    pattern <-
      "^.*/(CRAN|R)/refmans/(.*)/html/(.*)\\.html.*$"
    pac <- sub(pattern, "\\2", linksRows, useBytes = TRUE)
    fun <- sub(pattern, "\\3", linksRows, useBytes = TRUE)
    
    scoreCh <- sub(".*relevance=\\\"(.*)%\\\".*", "\\1", html[grep(".*relevance=", html, useBytes = TRUE)], useBytes = TRUE)
    score <- as.numeric(scoreCh)
    pLinks <- parseLinks(linksRows)
#    if (length(pac) < 1 && length(Date) > 0) {
    if (length(pac) < 1) {
      countDocs <- grep("Too many documents hit. Ignored",
                        html, useBytes = TRUE)
#      tooMany <- length(tooMany) > 0
      tooMany <- (length(countDocs)>0)
      if (tooMany) {
        Hits <- Inf
        warning("Too many documents hit.  Ignored")
      } else {
        if(length(Date)>0){
            op <- 'SOFTWARE PROBLEM:  dates found without'
            oops <- paste(op, 'content;  ignored.')
            warning(oops)
        }
      }
      Date <- Date[numeric(0)]
    }
    Ans <- data.frame(Package = pac,
                      Function = fun,
                      Date = as.POSIXct(Date, origin="1970-01-01"),
                      Score = score,
                      Description = pLinks$description,
                      Link = pLinks$link, stringsAsFactors=FALSE)
    oops <- (substring(pac, 1, 1)=='<')
    ans <- Ans[!oops,]
    attr(ans, "matches") <- Hits
    ans # end parseHTML
  }
  ##
  ## end internal functions
  ##
  quiet <- (verbose < 2)
##
## 2.  Set up query
##
## the following no longer works properly with R 4.1.0:  
#  if (substr(string, 1, 1) != "{") {
#    string <- gsub(" ", "+", string)
#  } else {
#  ## scan(url(...)) fails with spaces
#    string <- gsub(" ", "%20", string)
#  }
## With R 4.1.0, the previous code for parseHTML
## no longer worked with "{...}" to search for 
## a string, not multiple words.
## However, RSiteSearch('{...}') worked ...
## by searching for '"..."'.  
## SO if string begins with "{" and ends woth "}", 
## wrap that in double quotes 
## UNLESS it contains a double quote, 
## in which case give a warning.
  ns <- nchar(string)
  if((substring(string, 1, 1) == '{') &&
     (substring(string, ns, ns) == "}")){
    String <- gsub(" ", "%20", string)
    substring(String, 1, 1) <- '"'
    substring(String, ns+2, ns+2) <- '"'
  } else {
    String <- gsub(" ", "+", string)
  }
  fmt <- paste("https://search.r-project.org/?",
          "P=%s&HITSPERPAGE=20&FMT=xml&SORT=&DB=cran-help&DB=r-help",
               sep = "")
  href <- sprintf(fmt, String)
##
## 3.  Query
##
  ##  3.1.  Set up
  ans <- parseHTML(href)
  hits. <- attr(ans, "matches")
  if (length(hits.) < 1) {
    warning("HIT not found in HTML;  processing one page only.")
    hits. <- nrow(ans)
    attr(ans, "matches") <- hits.
  } else {
    if (length(hits.) > 1) {
      warning("HIT found more than once in first HTML page; ",
              "first 2 = ", hits.[1], ", ", hits.[2],
              ";  processing one page only")
      hits. <- nrow(ans)
      attr(ans, "matches") <- hits.
    }
  }
  if (is.na(hits.)) {
    warning("HIT found, not numeric, in the first HTML page; ",
            "processing one page only")
    hits. <- nrow(ans)
    attr(ans, "matches") <- hits.
  }
  if (verbose) {
    es <- if (hits. == 1) "" else "es"
    cat("found ", hits., " match", es, sep = "")
  }
  ##  3.2.  Retrieve
  n <- min(ceiling(hits./20), maxPages)
  if((hits.<Inf) && (nrow(ans) < hits.)) {
    if (verbose)
      cat(";  retrieving", n, c("page", "pages")[1 + (n > 1)])
    if (verbose) {
      if ((20 * n) < hits.) {
        cat(",", 20 * n, "matches.\n")
      } else {
        cat("\n")
      }
    }
    for(i in seq(2, length = n - 1)) {
      if (!quiet) {
        cat("retrieving page ", i, " of ", n, "\n", sep = "")
      } else if (verbose > 0) {
        cat(i, "")
        if((i%%10)==0) cat('\n')
        utils::flush.console()
      }
      href.i <- sprintf("%s&[=%d", href, i)
      ans.i <- parseHTML(href.i)
      ans <- rbind(ans, ans.i)
    }
    if (verbose>0) cat("\n")
  } else {
    cat("\n")
  }
  ##
  ## 4.  Compute Summary
  ##
  ans$Score <- as.numeric(as.character(ans$Score))
  pkgSum <- PackageSummary(ans)
  if((hits.<Inf) && (hits.>0)){
      nlk <- sum(pkgSum$Count)
      cat('Downloaded', nlk, 'links in', nrow(pkgSum), 'packages.\n')
  }
  ##
  ## 5.  Sort order
  ##
  s0 <- c("Count", "MaxScore", "TotalScore", "Package",
          "Score", "Function", "Date", "Description", "Link")
  s0. <- tolower(s0)
  if (is.null(sortby)) {
    sortby <-  s0
  } else {
    s1 <- match.arg(tolower(sortby), s0., TRUE)
    s1. <- c(s1, s0.[!(s0. %in% s1)])
    names(s0) <- s0.
    sortby <- s0[s1.]
  }
  ##
  ## 6.  Merge(packageSum, ans)
  ##
  packageSum <- pkgSum
  rownames(pkgSum) <- as.character(pkgSum$Package)
  pkgSum$Package <- NULL
  pkgSum$Date <- NULL
  pkgS2 <- pkgSum[as.character(ans$Package), , drop = FALSE]
  rownames(pkgS2) <- NULL
  Ans <- cbind(as.data.frame(pkgS2), ans)
  ##
  ## 7.  Sort Ans by "sort."
  ##
  Ans.num <- Ans[, c("Count", "MaxScore", "TotalScore", "Score")]
  ans.num <- cbind(as.matrix(Ans.num), Date=as.numeric(Ans$Date) )
  Ans.ch <- Ans[, c("Package","Function", "Description", "Link")]
  ans.ch <- as.data.frame(as.matrix(Ans.ch))
  ansKey <- cbind(as.data.frame(-ans.num), ans.ch)
  ##
  oSch <- do.call("order", ansKey[sortby])
  AnSort <- Ans[oSch, ]
  ##
  ## 8.  attributes
  ##
  rownames(AnSort) <- NULL
  ##
  attr(AnSort, "matches") <- hits.
  attr(AnSort, "PackageSummary") <- packageSum
  attr(AnSort, "string") <- String
  attr(AnSort, "call") <- match.call()
  class(AnSort) <- c("findFn", "data.frame")
  AnSort
}
