## helper function ##

# standardizes lower and upper bounds, and initial parameter estimates
control <- function(control = list(
  lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
  upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
  start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2))) {
  
  
  cs.low <- control$lower["cs"]
  dp.low <- control$lower["dp"]
  v0.low <- control$lower["v0"]
  a.low <- control$lower["a"]
  b.low <- control$lower["b"]
  f.low <- control$lower["f"]
  
  cs.high <- control$upper["cs"]
  dp.high <- control$upper["dp"]
  v0.high <- control$upper["v0"]
  a.high <- control$upper["a"]
  b.high <- control$upper["b"]
  f.high <- control$upper["f"]
  
  cs.start <- control$start["cs"]
  dp.start <- control$start["dp"]
  v0.start <- control$start["v0"]
  a.start <- control$start["a"]
  b.start <- control$start["b"]
  f.start <- control$start["f"]
  
  return(list(low = data.frame(cs.low, dp.low, v0.low, 
                               a.low, b.low, f.low),
              high = data.frame(cs.high, dp.high, v0.high,
                                a.high, b.high, f.high),
              start = data.frame(cs.start, dp.start, v0.start,
                                 a.start, b.start, f.start)
  )
  )
  
}