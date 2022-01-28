# max mean speed
max.mean.speed <- function(x, n = 600) {

  max.mean.vel <- do.call(
    rbind,
    lapply(
      data.table::frollmean(x, n = c(1:n)),
      max, na.rm = TRUE)
    )

  invisible(gc())

  max.mean.vel
}

max.median.speed <- function(x, n = "roeker") {
  
  if(n == "roeker") {
    n = c(0.3, 0.5, 1, 2, 3, 4, 
          5, 6.5, 10, 13.5, 18, 
          30, 60, 120, 300, 600, 
          900, 1200, 1800, 2400, 
          2700)
    } else {
     n = n
   }
  
  max.median.vel <- do.call(
    rbind,
    lapply(
      data.table::frollapply(x, n = n, FUN = Rfast::med),
      max, na.rm = TRUE)
  )
  
  invisible(gc)
  
  max.median.vel
  
}


# builds a data.frame from the max.mean.speed function
# used in the crit.speed functions
max.mean.speed.df <- function(player.speed, sample.rate = 10, dur = 600) {

  dat <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                    max.mean.speed = max.mean.speed(player.speed, n = dur * sample.rate))

  dat

}


