`?` <- function (e1, e2)
{
##
## NOTE:  This function does not work the same
## when sourced as when run as part of the package
## after "R CMD build" / "R CMD INSTALL" 
## 2021-06-13: It does not process properly:
## With ???spline(2)
## e2=2 when compiled but the default 100 otherwise
##  
  call <- match.call()
  Call <- as.character(call)
#  print(str(Call))
# Call = character vector of length 2  
  if("`?`(`?`(" == substring(Call[2], 1, 8)){
#    cat('found "`?`(`?`("\n')
    arg1 <- as.list(call[-1])#`?`(`?`(...
#    cat('arg1 = ')
#    print(str(arg1))
    arg2 <- as.list(arg1[[1]][-1])#`?`(`...
#    cat('arg2 = ')
#    print(str(arg2))
    arg3 <- as.list(arg2[[1]][-1])
#    cat('arg3 = ')
#    print(str(arg3))
    if(is.symbol(arg3[[1]])){
      fe1 <- as.character(arg3[[1]])
#      cat('fe1 = ', fe1, '\n')
      return(findFn(fe1))
    } else {
      fe1 <- as.character(arg3[[1]][1])
      fe2 <- as.list(arg3[[1]][-1])
#      cat('fe1 = ', fe1, '; fe2 =\n')
#      print(str(fe2))
      return(findFn(fe1, fe2[[1]]))
    }
  }
## The following code was written by 
## Duncan Murdoch ~2009
## It had problems with R 4.1.0;
## Spencer Graves write the above code 
## go make it work with R 4.1.0;
## Murdoch's code still seems to work in 
## the cases not covered by Graves' code.  
  original <- function() {
  # call the original ? function
    call[[1]] <- quote(utils::`?`)
    return(eval(call, parent.frame(2)))
  }

  # We don't handle requests with type
  if (!missing(e2)) {
    return(original())
  }

# We only handle function calls with double ??
#    (not counting the original one)
  topicExpr1 <- substitute(e1)
#  print(str(topicExpr1))
#  print(topicExpr1)
  if (!is.call(topicExpr1)
      || length(topicExpr1) != 2
      || topicExpr1[[1]] != "?"
      || !is.call(topicExpr1[[2]])
      || length(topicExpr1[[2]]) != 2
      || topicExpr1[[2]][[1]] != "?")
    return(original())

 # Get the expression
  topicExpr <- topicExpr1[[2]][[2]]
#  print(str(topicExpr))
#  print(topicExpr)

  # Construct our call to RSiteSearch.function
  if (is.call(topicExpr)) {
# It must not be a call to ?,
#      that would mean there are 4 or more
    if (topicExpr[[1]] == "?")return(original())
    lastArg <- length(topicExpr)
    topicExpr[[lastArg+1]] <- as.character(topicExpr[[1]])
    names(topicExpr)[[lastArg+1]] <- "string"
#    topicExpr[[1]] <- quote(sos::findFn)
    topicExpr[[1]] <- quote(findFn)
    f. <- eval(topicExpr, parent.frame(1))
  } else {
#    	RSiteSearch.function(as.character(topicExpr))
#    ff <- findFn(as.character(topicExpr))
    f. <- do.call('findFn', list(as.character(topicExpr)))
  }
  f.
}
