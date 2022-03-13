#' Max Mean Speed
#'
#' @param x player speed vector
#' @param n duration
#'
#' @export
max.mean.speed <- function(x,
                           n = 600) {

  max.mean.vel <- do.call(
    rbind,
    lapply(
      data.table::frollmean(x, n = c(1:n)),
      max, na.rm = TRUE)
    )

  invisible(gc())

  max.mean.vel
}


#' Max Mean Speed Data Frame
#'
#' @param player.speed player speed vector
#' @param sample.rate in Hz
#' @param dur duration
#'
#' @export
max.mean.speed.df <- function(player.speed,
                              sample.rate = 10,
                              dur = 600) {

  # builds a data.frame from the max.mean.speed function
  # used in the crit.speed functions

  dat <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                    max.mean.speed = max.mean.speed(player.speed,
                                                    n = dur * sample.rate))

  dat

}
