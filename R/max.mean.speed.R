#' Max Mean Speed Data Frame
#' 
#' A data frame that contains the greatest average speed per duration.
#' 
#' The function `max.mean.speed` calculates the moving averages for the velocity data for durations 
#' from 0.1 sec to `dur` sec using a default resolution (`sample.rate`) of 10 Hz. 600 s was chosen as the 
#' longest duration because this duration was previously used in similar studies (Delaney et al., 2018; 
#' Quod, Martin, Martin, & Laursen, 2010).  
#' 
#' Next, the `max.mean.speed` extracts the highest mean velocity for each duration. It is recommended to repeat this 
#' process for all data sets per athlete, thereby obtaining the velocity-duration profile using the greatest 
#' moving average velocities for each duration across all files.  
#' 
#' The user is returned a data set via `max.mean.speed.df` that contains the greatest average speed that a  
#' player can maintain for a given duration. The user has the ability to select a max duration of length 1L 
#' (1 value) or can input a vector to return distinct max mean speed values. Observations are returned at the 
#' given sample rate. For example, if `dur` is 600 and `sample.rate` is 10, a data frame containing 6000 max
#' mean speed observations will be returned. Conversely, if `dur` is c(10, 50, 100) and `sample.rate` is 10, a 
#' data frame containing 3 max mean speed observations will be returned. Note that `dur` 600 and c(1:600) return 
#' the same data sets. Also note that the use of multiple `dur` values is not always supported in functions that 
#' `max.mean.speed.df` feeds into. 
#' 
#' The default `dur` and `sample.rate` arguments have been validated for use in the modelling process. It 
#' is not suggested that the user changes these values. 
#' 
#' Please note that the max mean speed is calculated using `data.table::frollmean` which uses a C++ back-end. 
#' In doing so, this function can be temporarily taxing on your system. It is suggested that this 
#' function be called when the system has available RAM and CPU. Otherwise, an error message can be 
#' returned indicating that a vector of size x Mb cannot be assigned.  
#' 
#' For those interested in the maximal median speed, like that used by Roecker, Mahler, Heyde, Röll, & 
#' Gollhofer (2017) see `max.median.speed.df`. Please note that calculating the max mean speed is much 
#' faster than the max median speed. 
#' 
#'
#' @param player.speed player speed vector
#' @param sample.rate in Hz, default set to 10 Hz
#' @param dur max duration, default set to 600 s
#' 
#' @seealso max.median.speed.df, max.mean.speed, max.median.speed
#'
#' @export
max.mean.speed.df <- function(player.speed,
                              sample.rate = 10,
                              dur = 600) {

  # builds a data.frame from the max.mean.speed function
  # used in the crit.speed functions
  
  if(length(dur) == 1) {d = seq(0.1, dur, by = 1/sample.rate)}
  if(length(dur) != 1) {d = dur}

  dat <- data.frame(duration = d,
                    max.mean.speed = max.mean.speed(player.speed,
                                                    n = dur * sample.rate))

  dat

}

#' Max Mean Speed
#' 
#' This is a helper function that feeds into `max.mean.speed.df`.
#' 
#' The arguments for this function are minimal and it is not suggested that this function be 
#' used on its own.
#' 
#' `n` is in arbitrary units and does not take into account sample rate. Therefore, 
#' users should be cautious when calling `max.mean.speed` exclusively. 
#' 
#' @param x player speed vector
#' @param n duration
max.mean.speed <- function(x,
                           n = 600) {
  
  if(length(n) == 1) {n. = 1:n}
  if(length(n) > 1) {n. = n}
  
  max.mean.vel <- do.call(
    rbind,
    lapply(
      data.table::frollmean(x, n = n.),
      max, na.rm = TRUE)
  )
  
  invisible(gc())
  
  max.mean.vel
}