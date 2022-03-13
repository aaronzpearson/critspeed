#' Max Median Speed
#'
#' @param x player speed vector
#' @param n durations
#' @param sample.rate in Hz
#'
#' @export
max.median.speed <- function(x,
                             n = "roeker",
                             sample.rate = 10) {

  f <- function(x,
                n = "roeker",
                sample.rate = 10) {

  if(n == "roeker") {

    n. = round(c(0.3, 0.5, 1, 2, 3, 4,
           5, 6.5, 10, 13.5, 18,
           30, 60, 120, 300, 600,
           900, 1200, 1800, 2400,
           2700) * sample.rate, 0)

  }

  if(n != "roeker") {

    n. = round(n * sample.rate, 0)

  }

      max.median.vel <- do.call(
        rbind,
        lapply(
          data.table::frollapply(x, n = n., FUN = Rfast::med),
          max, na.rm = TRUE)
      )

  invisible(gc)

  max.median.vel

  }

  speed = x
  n. = n
  hz = sample.rate

  suppressMessages(
    suppressWarnings(

      f(x = speed,
        n = n.,
        sample.rate = hz)

    )
  )

}


#' Max Median Speed Data Frame
#'
#' @param player.speed player speed vector
#' @param sample.rate in Hz
#' @param dur durations
#'
#' @export
max.median.speed.df <- function(player.speed,
                                sample.rate = 10,
                                dur = "roeker") {

  if(dur == "roeker") {

    n. = c(0.3, 0.5, 1, 2, 3, 4,
                 5, 6.5, 10, 13.5, 18,
                 30, 60, 120, 300, 600,
                 900, 1200, 1800, 2400,
                 2700)

  }

  if(dur != "roeker") {

    n. = seq(1, dur, by = 1/ sample.rate)

  }

  if(length(n. > 10)) {

    message("This shouldn't too be long...
             ")

  }

  if(sample.rate != 10) {
    message("Warning: This function is written for 10 Hz data
            dur was rounded to handle errors.")
  }

  if(length(player.speed) < max(n.* sample.rate)) {
    message("Warning: Data does not contain enough observations
            -Inf or Inf values returned")
  }

  dat <- data.frame(duration = n.,
                    max.med.speed = max.median.speed(player.speed,
                                                     n = n.,
                                                     sample.rate = sample.rate)
                    ) # changed colname from max.mean.speed to max.med.speed
                                                                                                       # does this break crit.speed fns ?
                                                                                                       # if yes, force colname change
                                                                                                       # colnames(df) <- c(duration, max.mean.speed)

  dat

}
