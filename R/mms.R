## helper functions ##

# called from `critspeed()`
mms.data.frame <- function(player.speed,
                           dur = 600,
                           sample.rate = 10, 
                           method = c("mean", "median")) {
  
  if(length(method) != 1) {method = "mean"}
  
  vect <- callCS("mms",
              player.speed = player.speed,
              sample.rate = sample.rate,
              dur = dur,
              method = method)
  
  if(dur == "roecker") {
    n. <- c(0L, 0.3L, 0.5L, 1L, 2L, 3L, 4L,
            5L, 6.5L, 10L, 13.5L, 18L,
            30L, 60L, 120L, 300L, 600L,
            900L, 1200L, 1800L, 2400L,
            2700L)
  } else if(is.numeric(dur)) {
    n. <- seq(0, dur, by = 1/sample.rate)
  } 
  
  
  
  mms.df <- data.frame(duration = n.[-1],
                       max.mean.speed = vect)
  
  if(method == "median") {colnames(mms.df) = c("duration", "max.median.speed")}
  
  mms.df
  
}

# either max mean or max median speed vector
mms <- function(player.speed,
                dur = 600,
                sample.rate = 10, 
                method = c("mean", "median")) {
  
  if(length(method) != 1) {method = "mean"}
  
  
  switch(method,
         mean = callCS("mms.mean",
                       x = player.speed, 
                       n = dur, 
                       sample.rate = sample.rate),
         median = callCS("mms.median",
                         x = player.speed,
                         n = dur, 
                         sample.rate = sample.rate)
  )
  
  
}

# max mean speed vector
mms.mean <- function(x,
                     n = 600,
                     sample.rate = 10) {
  
  if(length(n) > 1) {stop("n must be either a positive integer or 'roecker'")}
  
  if(n == "roecker") {
    n. <- c(0.3L, 0.5L, 1L, 2L, 3L, 4L,
            5L, 6.5L, 10L, 13.5L, 18L,
            30L, 60L, 120L, 300L, 600L,
            900L, 1200L, 1800L, 2400L,
            2700L) * sample.rate
  } else if(is.numeric(n)) {
    n. <- 1:(n*sample.rate)
  } 
  
  if(n == "roecker" & 
     sum(n. %% 1) > 0) {stop("Non-integer values returned when multiplying sample.rate by: \n 0.3, 0.5, 1, 2, 3, 4, 5, 6.5, 10, 13.5, 18, 30, 60, 120, 300, 600, 900, 1200, 1800, 2400, 2700")}
  
  
  max.mean.vel <- do.call(
    rbind,
    lapply(
      data.table::frollmean(x, n = n.),
      max, na.rm = TRUE)
  )
  
  invisible(gc())
  
  max.mean.vel
}

# max median speed vector
mms.median <- function(x,
                       n = 600,
                       sample.rate = 10) {
  
  if(length(n) > 1) {stop("n must be either a positive integer or 'roecker'")}
  
  if(n == "roecker") {
    n. <- c(0.3L, 0.5L, 1L, 2L, 3L, 4L,
            5L, 6.5L, 10L, 13.5L, 18L,
            30L, 60L, 120L, 300L, 600L,
            900L, 1200L, 1800L, 2400L,
            2700L) * sample.rate
  } else if(is.numeric(n)) {
    n. <- 1:(n*sample.rate)
  } 
  
  if(n == "roecker" & 
     sum(n. %% 1) > 0) {stop("Non-integer values returned when multiplying sample.rate by: \n 0.3, 0.5, 1, 2, 3, 4, 5, 6.5, 10, 13.5, 18, 30, 60, 120, 300, 600, 900, 1200, 1800, 2400, 2700")}
  
  max.median.vel <- do.call(
    rbind,
    lapply(
      data.table::frollapply(x, n = n., FUN = Rfast::med),
      max, na.rm = TRUE)
  )
  
  invisible(gc)
  
  max.median.vel
  
}

