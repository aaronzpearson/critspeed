#' Compare Model Parameters and Goodness-of-Fits
#' 
#' A data set with model results from multiple player sessions. 
#' 
#' This function allows users to compare model results from multiple player sessions. `data` and `data2` can also take on 
#' one or multiple sessions. The advantage of `data` and `data2` taking on a list of sessions allows users to input 
#' a week or month of sessions and compare parameter estimates between weeks or months. Doing so provides practitioners 
#' the ability to track changes in a player's critical speed, D', and max speed. 
#' 
#' If `raw.data = TRUE` function calls on `critspeed::critspeed` to process the data which it then feeds into `model.results`. 
#' If `raw.data = FALSE`, `model.results` is called directly. 
#' 
#' As with `critspeed::critspeed`, this function relies heavily on `data.table` and `Rfast`, both of which use a C++ 
#' backend. In doing so, processing time is significantly decreased at the cost of computer resources. It is suggested that 
#' CPU and RAM be available when running `critspeed::critspeed` to minimize the risk of an error occuring. Common `Error` messages 
#' returned are that there is not enough memory available to assign a vector of size x MB. To avoid this error, 
#' sufficient RAM must be available on your local machine. 
#' 
#' @seealso critspeed, max.mean.speed.df, max.median.speed.df, model.results
#'
#' @param data group 1 of sessions
#' @param data2 group 2 of sessions
#' @param speed.col column name for player speed
#' @param sample.rate in Hz, default set to 10 Hz
#' @param dur max duration, default set to 600 s
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#' @param raw.data data type being either raw (unprocessed) or critspeed (processed), defaults set to TRUE
#' @param cv.2 start duration for CV2, default set to 120 s
#' @param d.prime.estim D' estimate, default set to 100 m
#' @param crit.speed.estim critical speed estimate, default set to 3.5 m/s
#' @param max.speed.estim max speed, default set to 12 m/s
#' 
#' @seealso critspeed.list
#'
#' @export
model.results.compare <- function(data = list(),
                              data2 = list(),
                              speed.col = "max.mean.speed",
                              raw.data = TRUE,
                              sample.rate = 10,
                              dur = 600,
                              roecker = FALSE, 
                              cv.2 = 120, 
                              d.prime.estim = 150,
                              max.speed.estim = 12,
                              crit.speed.estim = 3.5) {
  
  cs.list <- cs.results.observed.list(data = data,
                 data2 = data2,
                 speed.col = speed.col,
                 raw.data = raw.data,
                 sample.rate = sample.rate,
                 dur = dur,
                 roecker = roecker)
  
  results.1 <- cs.results.model(player.speed = cs.list[[1]]$max.mean.speed,
                             raw.data = raw.data,
                             sample.rate = sample.rate,
                             cv.2 = cv.2, 
                             dur = dur,
                             d.prime.estim = d.prime.estim,
                             crit.speed.estim = crit.speed.estim,
                             max.speed.estim = max.speed.estim,
                             roecker = roecker)
  
  results.2 <- cs.results.model(player.speed = cs.list[[2]]$max.mean.speed,
                             raw.data = raw.data,
                             sample.rate = sample.rate,
                             cv.2 = cv.2,
                             dur = dur,
                             d.prime.estim = d.prime.estim,
                             crit.speed.estim = crit.speed.estim,
                             max.speed.estim = max.speed.estim,
                             roecker = roecker)
  
  results = rbind(results.1, results.2)
  
  
  results.list <- data.frame(data.set = c(rep("1", 5), rep("2", 5)))
  results.list <- cbind(results.list, results)
  
  results.list
  
}


#' Build Multiple critspeed Data Sets
#' 
#' Returns a list of data sets that contain the max mean or max median speed per duration.
#' 
#' This function, and `cs.results.observed.list` return the max mean or max median player speed per 
#' duration. The functions differ in that `cs.results.observed.list` feeds into `model.results.compare` 
#' which requires alternate variable names. 
#' 
#' For users interested in manually generating multiple critspeed data sets, it is recommended 
#' that they call `critspeed.list` which will return variable names that correspond with whether 
#' `roecker` is set to `TRUE` or `FALSE`. 
#'
#' @param data group 1 of sessions
#' @param data2 group 2 of sessions
#' @param speed.col column name for player speed
#' @param sample.rate in Hz, default set to 10 Hz
#' @param dur max duration, default set to 600 s
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#' @param raw.data data type being either raw (unprocessed) or critspeed (processed), defaults set to TRUE
#'
#' @export
critspeed.list <- function(data = list(),
                              data2 = list(),
                              speed.col = "max.mean.speed",
                              raw.data = TRUE,
                              sample.rate = 10,
                              dur = 600,
                              roecker = FALSE) {
  
  cs.list <- cs.results.observed.list(data = data,
                           data2 = data2,
                           speed.col = speed.col,
                           raw.data = raw.data,
                           sample.rate = sample.rate,
                           dur = dur,
                           roecker = roecker)
  
  if(roecker == TRUE) {
    colnames(cs.list[[1]]) <- c("duration", "max.median.speed")
    colnames(cs.list[[2]]) <- c("duration", "max.median.speed")
  }
  
  cs.list
  
}

cs.results.observed.list <- function(data = list(),
                                     data2 = list(),
                                     speed.col = "max.mean.speed",
                                     raw.data = TRUE,
                                     sample.rate = 10,
                                     dur = 600,
                                     roecker = FALSE) {

  set.one <- cs.critspeed(data = data,
                       speed.col = speed.col, 
                       raw.data = raw.data,
                       sample.rate = sample.rate,
                       dur = dur,
                       roecker = roecker)
  
  set.two <- cs.critspeed(data = data2,
                       speed.col = speed.col, 
                       raw.data = raw.data,
                       sample.rate = sample.rate,
                       dur = dur,
                       roecker = roecker)


  set.one <- data.table(set.one)
  set.one.global <- set.one[, max(max.mean.speed), by = duration]
  colnames(set.one.global) <- c("duration", "max.mean.speed")

  set.two <- data.table(set.two)
  set.two.global <- set.two[, max(max.mean.speed), by = duration]
  colnames(set.two.global) <- c("duration", "max.mean.speed")


  critpeed.df.list <- list(set.one.global,
                           set.two.global)

  critpeed.df.list

}
