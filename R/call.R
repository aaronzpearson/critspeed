## helper function ##

# generic function for clean helper function call
callCS <- function(FUN, ...) {
  
  FUN <- match.fun(FUN)
  FUN(...)
  
}
