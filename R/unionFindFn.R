unionFindFn <- function(e1, e2, sortby=NULL) {
##
## 1.  rbind
##
  e12Names <- intersect(names(e1), names(e2))
  k12 <- length(e12Names)
  if(k12 != length(names(e1)))warning('Columns in e1 not in e2')
  if(k12 != length(names(e2)))warning('Columns in e2 not in e1')
#
 xy <- rbind(e1[, e12Names], e2[, e12Names])
##
## 2.  Find and merge duplicates
##
  if(!('Link' %in% names(xy))) stop('Neither x nor y contain Link')
  uL <- table(xy$Link)
  dups <- names(uL[uL>1])
  ndups <- length(dups)
  keep <- rep(TRUE, nrow(xy))
  Sc <- xy$Score
  for(i in seq(1, length=ndups)){
    whichi <- which(xy$Link == dups[i])
    if(Sc[whichi[1]]>Sc[whichi[2]])
      keep[whichi[2]] <- FALSE
    else keep[whichi[1]] <- FALSE
  }
  xy. <- xy[keep, ]
##
## 3.  Rebuild summary and resort
##
  xys <- sortFindFn(xy.[,
     c('Package', 'Score', 'Function', 'Date', 'Description', 'Link')],
                          sortby)
##
## 4.  Fix attributes
##
  attr(xys, 'matches') <- c(attr(e1, 'matches'), attr(e2, 'matches'))
  attr(xys, 'string') <- paste(attr(e1, 'string'), attr(e2, 'string'),
                               sep=' | ')
  attr(xys, 'call') <- call( "(", call( "|",
                    attr(e1, "call"), attr(e2, "call") ) )
##
## 5.  Done
##
  xys
}
