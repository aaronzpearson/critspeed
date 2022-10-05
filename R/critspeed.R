#' Maximal Mean and Maximal Median Speed per Duration
#' 
#' A `data.frame` that contains the greatest mean or median speed per duration.
#' 
#' The `critspeed()` function calculates the moving averages or medians for an athlete's speed over a given duration. `player.speed` should be 
#' input as the speed variable from the player's tracking data.
#' 
#' @param player.speed the player's speed vector
#' @param dur duration(s) to calculate max mean or max median speed, default set to 600 s (see notes)
#' @param sample.rate in Hz, default set to 10 Hz
#' @param method indicates whether the max mean or max median speed should be calculated
#' 
#' @note 
#' 
#' \itemize{
#' \item `dur`: Either an integer of length 1L or `"roecker"`
#' \itemize{
#' \item When an integer is input, the `data.frame` returned will encompass all values from 0.1 to `dur`
#' \item An input of `"roecker"` selects the following durations, as was suggested by Roecker, Mahler, Heyde, Röll, & Gollhofer (2017): 0L, 0.3L, 0.5L, 1L, 2L, 3L, 4L, 5L, 6.5L, 10L, 13.5L, 18L, 30L, 60L, 120L, 300L, 600L, 900L, 1200L, 1800L, 2400L, 2700L. Note that these durations *must* return integers when multiplied by the `sample.rate`. For example, 0.3 s at 10 Hz returns a value of 3. If the `sample.rate` is 5, the value returned is 1.5 and will result in an error.
#' }
#' \item If `method` is assigned as `"median"`, processing time and resources are significantly increased. It is suggested that users opt for `dur = "roecker"` to minimize processing time and resources. 
#' \item This function relies heavily on `data.table` and `Rfast`, both of which use a C++ backend. In doing so, 
#' processing time is significantly decreased at the cost of computer resources. It is suggested that CPU and RAM 
#' be available when running `critspeed()` to minimize the risk of an error occuring. Common `Error` messages 
#' returned are that there is not enough memory available to assign a vector of size x MB. To avoid this error, 
#' sufficient RAM must be available on your local machine.
#' 
#' }
#' 
#' @seealso [model], [compile], [model.parameters], [plot.model]
#'
#' @export
critspeed <- function(player.speed, 
                      dur = 600,
                      sample.rate = 10, 
                      method = c("mean", "median")) {
  
  callCS("mms.data.frame",
         player.speed = player.speed,
         dur = dur,
         sample.rate = sample.rate,
         method = method)
  
}


#' Greatest Maximal Mean and Median Speed per Duration
#' 
#' A lightweight function that returns the individuals greatest max mean and median speed per duration.
#' 
#' This function provides practitioners the ability to return the greatest max mean and median speed per duration. The `data`
#' and `...` arguments must be `data.frame`s or `data.table`s. It is suggested that practitioners use the `critspeed()` function 
#' to generate the required `data.frame`s. 
#' 
#' Users are returned a `data.frame` that consists of either two or three variables. The first variable is always `duration`. The 
#' second variable is either the maximal mean speed or maximal median speed per duration. If users input `data.frame`s that include 
#' maximal mean and maximal median speeds, they will be returned a data frame with `duration`, `max.mean.speed`, and `max.median.speed` 
#' variables (if they generated the data using the `critspeed()` function). This ensures that max mean and max median 
#' speeds are not assumed to be interchangeable.
#'
#' @param data data set containing at least two variables (see notes)
#' @param ... other data sets that fulfill the requirements described in **Notes**
#' 
#' @note 
#' 
#' Users must be aware of the following requirements for the following arguments:    
#' 
#' \enumerate{
#' \item `data` is a `data.frame` must satisfy the following criteria:
#' \itemize{
#' \item The first column is the duration variable
#' \item The second column is the maximal mean speed or maximal median speed variable
#' \item Subsequent columns will be return an error and must be removed before processing
#' }
#' 
#' Users can generate appropriate `data.frame`s using the `critspeed()` function
#' 
#' }
#'
#' @export
compile <- function(data, ...) { # best of multiple sessions
  
  data = list(data, ...)

  mmv <- data.table::rbindlist(data, fill = TRUE)
  
  # mmv.max <- mmv[, max(mmv, na.rm = TRUE), by = "duration"]
  mmv.max <- mmv[ , lapply(.SD, max, na.rm = TRUE), 
                 by = "duration", 
                 .SDcols = colnames(mmv)[2:ncol(mmv)]]
  
  return(
    data.frame(
      mmv.max
      )
    )
  
}



