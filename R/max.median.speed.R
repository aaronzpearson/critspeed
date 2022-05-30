#' Max Median Speed Data Frame
#' 
#' A data frame that contains the greatest median speed per duration.
#' 
#' This function emulates the maximal median speed calculation that was introduced 
#' by Roecker, Mahler, Heyde, Röll, & Gollhofer (2017).
#' 
#' This function calls on `max.median.speed` to calculate the maximal median 
#' speed that a player can maintain for a given duration. The `dur` argument can take on 
#' either a value of 1L (a single value), a vector of multiple values,or "roecker". 
#' When users input a value of 1L, the functions treats it as the maximal duration that 
#' the maximal median speed should be calculated per duration. For example, if `dur` is set 
#' to 600 with `sample.rate` set to 10 Hz, a data frame will be returned with durations 
#' ranging from 0.1 to 600 s. If a vector of values is provided to the `dur` argument, the 
#' function will call upon these values in seconds. Therefore, a `dur` of c(0.1, 10, 100, 600) 
#' returns a data frame containing four observations of 0.1, 10, 100, and 600 s. Finally, if 
#' `dur` is set to "roecker", durations are set to those suggested in the original paper 
#' by Roecker, Mahler, Heyde, Röll, & Gollhofer (2017): 0.3, 0.5, 1, 2, 3, 4, 5, 6.5, 10, 
#' 13.5, 18, 30, 60, 120, 300, 600, 900, 1200, 1800, 2400, and 2700 s.
#' 
#' The max median speed is calculated using `Rfast::med` which uses a C++ back-end. 
#' In doing so, this function can be temporarily taxing on your system. It is suggested that this 
#' function be called when the system has available RAM and CPU. Otherwise, an error message can be 
#' returned indicating that a vector of size x Mb cannot be assigned.
#' 
#' Please note that calculating the max median value can be significantly slower 
#' than calculating the max mean speed. For those interested in the max mean speed, please 
#' see `max.mean.speed.df`.
#'
#' @param player.speed player speed vector
#' @param sample.rate in Hz, default set to 10 Hz
#' @param dur duration(s), default set to "roecker"
#' 
#' @seealso max.median.speed, max.mean.speed.df, max.mean.speed
#' 
#' @references Roecker, K., Mahler, H., Heyde, C., Röll, M., & Gollhofer, A. (2017). 
#' The relationship between movement speed and duration during soccer matches. PLoS 
#' ONE, 12(7), e0181781. https://doi.org/10.1371/journal.pone.0181781 
#'
#' @export
max.median.speed.df <- function(player.speed,
                                sample.rate = 10,
                                dur = "roecker") {

  suppressMessages(
    suppressWarnings(
      
  if(dur == "roecker") {

    n. = c(0.3, 0.5, 1, 2, 3, 4,
                 5, 6.5, 10, 13.5, 18,
                 30, 60, 120, 300, 600,
                 900, 1200, 1800, 2400,
                 2700)

  } else if(dur != "roecker" && length(dur) == 1L) {

    n. = seq(1, dur, by = 1/ sample.rate)

  } else if(dur != "roecker" & length(dur) > 1L) {
    
    n. = dur
    
  }
  
    )
  )

  if(length(n.) > 10) {

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
                                                     sample.rate = sample.rate))
  suppressMessages(
    suppressWarnings(
      return(dat)
    )
  )
  

}


#' Max Median Speed
#'
#'This is a helper function that feeds into `max.median.speed.df`.
#' 
#' The arguments for this function are minimal and it is not suggested that this function be 
#' used on its own.
#' 
#' `n` is in arbitrary units and does not take into account sample rate. Therefore, 
#' users should be cautious when calling `max.median.speed` exclusively. 
#' 
#' @param x player speed vector
#' @param n durations
#' @param sample.rate in Hz
#' 
#' @references Roecker, K., Mahler, H., Heyde, C., Röll, M., & Gollhofer, A. (2017). 
#' The relationship between movement speed and duration during soccer matches. PLoS 
#' ONE, 12(7), e0181781. https://doi.org/10.1371/journal.pone.0181781 
#' 
#' @export
max.median.speed <- function(x,
                             n = "roecker",
                             sample.rate = 10) {
  
  f <- function(x,
                n = "roecker",
                sample.rate = 10) {
    
    if(n == "roecker") {
      
      n. = round(c(0.3, 0.5, 1, 2, 3, 4,
                   5, 6.5, 10, 13.5, 18,
                   30, 60, 120, 300, 600,
                   900, 1200, 1800, 2400,
                   2700) * sample.rate, 0)
      
    }
    
    if(n != "roecker") {
      
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