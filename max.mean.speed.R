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


# builds a data.frame from the max.mean.speed function
# used in the crit.speed functions
max.mean.speed.df <- function(player.speed, sample.rate = 10, dur = 600) {

  dat <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                    max.mean.speed = max.mean.speed(player.speed, n = dur * sample.rate))

  dat

}


