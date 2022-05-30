#' Generate a critspeed Data Frame
#' 
#' A data frame containing the max mean or max median player velocity for the selected duration.
#' 
#' This function takes on one or multiple data sets in `list` form and returns the max mean or max median 
#' velocity over the input `dur`. If a single session is input, it should be in the `raw` form, being GPS or
#' LPS data. If a single session is input and `raw` is set to FALSE, the returned data set will match the 
#' data loaded into the function.
#' 
#' Inputting multiple sessions in a `list` format returns the greatest max mean or max median speed for all 
#' sessions. Therefore, the `global` data set contains the greatest speed among all observations from all sessions. 
#' It does not matter whether the data is `raw` when calculating the max mean or max median speed for all 
#' observations. To note, when loading multiple `raw` data sets, processing time and required computer resources 
#' increase. This is further exemplified when `roecker` is set to `TRUE`. 
#' 
#' `dur` can be set to either a value of length 1L (single value) or `roecker`. When a single value is input, the 
#' duration calculated from 0.1 to `dur`. If `dur` is set to `roecker`, the durations calculated are set to 
#' those suggested in the original paper by Roecker, Mahler, Heyde, Röll, & Gollhofer (2017): 0.3, 0.5, 1, 2, 
#' 3, 4, 5, 6.5, 10, 13.5, 18, 30, 60, 120, 300, 600, 900, 1200, 1800, 2400, and 2700 s. 
#' 
#' This function relies heavily on `data.table` and `Rfast`, both of which use a C++ backend. In doing so, 
#' processing time is significantly decreased at the cost of computer resources. It is suggested that CPU and RAM 
#' be available when running `critspeed::critspeed` to minimize the risk of an error occuring. Common `Error` messages 
#' returned are that there is not enough memory available to assign a vector of size x MB. To avoid this error, 
#' sufficient RAM must be available on your local machine. 
#' 
#' Please see `help(max.mean.speed.df)` and `help(max.median.speed.df)` for greater details on the underlying 
#' functions used to process the data. 
#' 
#' @seealso max.mean.speed.df, max.median.speed.df
#'
#' @param data a list of data frame(s)
#' @param speed.col name that corresponds with the speed/ velocity variable
#' @param sample.rate in Hz, default set to 10 Hz
#' @param dur max duration, default set to 600 s
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#' @param raw.data data type being either raw (unprocessed) or critspeed (processed), defaults set to TRUE
#' 
#' @export
critspeed <- function(data = list(),
                      speed.col = "max.mean.speed",
                      raw.data = TRUE,
                      sample.rate = 10, 
                      dur = 600,
                      roecker = FALSE) {

  cs.critspeed(data = data,
               speed.col = speed.col,
               raw.data = raw.data,
               sample.rate = sample.rate,
               dur = dur,
               roecker = roecker)
  
}


cs.critspeed <- function(data = list(),
                         speed.col = "max.mean.speed",
                         raw.data = TRUE,
                         sample.rate = 10, 
                         dur = 600,
                         roecker = FALSE) {
  
  if(raw.data == FALSE) {
    
    
    set.one.max.mean <- do.call(rbind,
                                lapply(
                                  data,
                                  function(x) {
                                    data.frame(duration = 1:nrow(x)/sample.rate,
                                               max.mean.speed = x[, speed.col])
                                  }
                                )
    )
    
    colnames(set.one.max.mean) <- c("duration", "max.mean.speed")
    
  }
  
  if(raw.data == TRUE & 
     roecker == FALSE) {
    
    set.one.max.mean <- data.frame()
    
    for(i in seq_along(data)) {
      
      max.mean.speed.vect = max.mean.speed(data[[i]][, speed.col],
                                           n = dur * sample.rate)
      
      temp.df <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                            max.mean.speed = max.mean.speed.vect)
      
      
      set.one.max.mean <- rbind(set.one.max.mean, temp.df)
      
    }
  }
  
  if(raw.data == TRUE & 
     roecker == TRUE) {
    
    set.one.max.mean <- data.frame()
    for(i in seq_along(data)) {
      
      max.mean.speed.vect = max.median.speed(data[[i]][, speed.col],
                                           n = dur * sample.rate)
      
      temp.df <- data.frame(duration = seq(0.1, dur, by = 1/sample.rate),
                            max.mean.speed = max.mean.speed.vect)
      
      set.one.max.mean <- rbind(set.one.max.mean, temp.df)
      
    }
    
  }
 
  
  set.one.max.mean <- data.table::data.table(set.one.max.mean)
  set.one.max.mean.global <- set.one.max.mean[, max(max.mean.speed), by = duration]
  colnames(set.one.max.mean.global) <- c("duration", "max.mean.speed")
  
  if(roecker == TRUE) {colnames(set.one.max.mean.global) <- c("duration", "max.median.speed")}
  
  return(set.one.max.mean.global)
   
}