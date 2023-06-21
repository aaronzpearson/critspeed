#' Maximal Mean and Maximal Median Speed per Duration
#' 
#' A \code{data.frame} that contains the greatest mean or median speed per duration.
#' 
#' The \code{critspeed()} function calculates the moving averages or medians for an athlete's speed over a given duration. \code{player.speed} should be 
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
#' \item \code{dur}: Either an integer of length 1L or \code{"roecker"}
#' \itemize{
#' \item When an integer is input, the \code{data.frame} returned will encompass all values from 0.1 to \code{dur}
#' \item An input of \code{"roecker"} selects the following durations, as was suggested by Roecker, Mahler, Heyde, Röll, & Gollhofer (2017): 0L, 0.3L, 0.5L, 1L, 2L, 3L, 4L, 5L, 6.5L, 10L, 13.5L, 18L, 30L, 60L, 120L, 300L, 600L, 900L, 1200L, 1800L, 2400L, 2700L. Note that these durations *must* return integers when multiplied by the \code{sample.rate}. For example, 0.3 s at 10 Hz returns a value of 3. If the \code{sample.rate} is 5, the value returned is 1.5 and will result in an error.
#' }
#' \item If \code{method} is assigned as \code{"median"}, processing time and resources are significantly increased. It is suggested that users opt for \code{dur = "roecker"} to minimize processing time and resources. 
#' \item This function relies heavily on \code{data.table} and \code{Rfast}, both of which use a C++ back-end. In doing so, 
#' processing time is significantly decreased at the cost of computer resources. It is suggested that CPU and RAM 
#' be available when running \code{critspeed()} to minimize the risk of an error occurring. Common \code{Error} messages 
#' returned are that there is not enough memory available to assign a vector of size x MB. To avoid this error, 
#' sufficient RAM must be available on your local machine.
#' 
#' }
#' 
#' @seealso \code{\link{model}}, \code{\link{compile}}, \code{\link{speed.observations}}, \code{\link{model.parameters}}, \code{\link{plot.model}}, \code{\link{fitted.model}}
#'
#' @export
#' 
#' @examples 
#' 
#' data(sessionRaw)
#' 
#' \dontrun{
#' # Max Mean Speed
#' 
#' session.mean <- critspeed(sessionRaw$speed,
#' dur = 600,
#' sample.rate = 10,
#' method = "mean")
#' 
#' # head(session.mean)
#' 
#' # Max Median Speed
#' 
#' session.median <- critspeed(sessionRaw$speed,
#' dur = "roecker",
#' sample.rate = 10,
#' method = "median")
#' 
#' # head(session.median)
#' 
#' }
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
#' This function provides practitioners the ability to return the greatest max mean and median speed per duration. The \code{data}
#' and \code{...} arguments must be \code{data.frame}s or \code{data.table}s. It is suggested that practitioners use the \code{critspeed()} function 
#' to generate the required \code{data.frame}s. 
#' 
#' Users are returned a \code{data.frame} that consists of either two or three variables. The first variable is always \code{duration}. The 
#' second variable is either the maximal mean speed or maximal median speed per duration. If users input \code{data.frame}s that include 
#' maximal mean and maximal median speeds, they will be returned a data frame with \code{duration}, \code{max.mean.speed}, and \code{max.median.speed} 
#' variables (if they generated the data using the \code{critspeed()} function). This ensures that max mean and max median 
#' speeds are not assumed to be interchangeable.
#' 
#' Indexing returns the session from which the max mean or max median speed was calculated. The variable, `.id`, returns an integer that corresponds with 
#' the position of the \code{data.frame} when input into \code{compile()}.
#'
#' @param data data set containing at least two variables (see notes)
#' @param ... other data sets that fulfill the requirements described in \strong{Notes}
#' @param index returns the session that contained the greatest max mean or max median speed, default set to TRUE
#' 
#' @note 
#' 
#' Users must be aware of the following requirements for the following arguments:    
#' 
#' \enumerate{
#' \item \code{data} is a \code{data.frame} must satisfy the following criteria:
#' \itemize{
#' \item The first column is the duration variable called \code{duration}
#' \item The second column is the maximal mean speed or maximal median speed variable and named accordingly (\code{max.mean.speed} or \code{max.median.speed})
#' \item Subsequent columns will return an error and must be removed before processing
#' }
#' \item \code{index} returns a warning when variable names are inconsistent
#' 
#' Users can generate appropriate \code{data.frame}s using the \code{critspeed()} function
#' 
#' }
#' 
#' @seealso \code{\link{critspeed}}
#'
#' @export
#' 
#' @examples 
#' 
#' data(sessionMaxMeanSpeed)
#' data(sessionMaxMedianSpeed)
#' data(sessionRaw)
#' 
#' # Compile Max Mean Speed
#' 
#' \dontrun{
#' 
#' session.mean <- critspeed(sessionRaw$speed,
#' dur = 600,
#' sample.rate = 10,
#' method = "mean")
#' 
#' compile.mean <- compile(sessionMaxMeanSpeed, session.mean, index = TRUE)
#' 
#' # head(compile.mean)
#'
#'
#' # Compile Max Mean and Max Median Speed
#' 
#' compile.mean.median <- compile(sessionMaxMeanSpeed, session.mean, sessionMaxMedianSpeed, index = TRUE)
#' 
#' # returns a warning and will not index the max mean and max median speeds
#' # head(compile.mean.median)
#' }
#' 
#' 
compile <- function(data, ..., index = TRUE) { # best of multiple sessions
  
  callCS("max.critspeed",
         data = data,
         ...,
         index = index)
  
}

## helper function ##

# extended compile call
max.critspeed <- function(data, ..., index = TRUE) {
  
  mmv = data.table::rbindlist(list(data, ...), 
                              idcol = TRUE,
                              fill = TRUE,
                              use.names = TRUE)
  
  mmv[is.na(mmv)] <- 0
  cols <- colnames(mmv)
  
  if(length(colnames(mmv)) == 3) {
    
    mmv.temp <- mmv
    
    colnames(mmv.temp) <- c(".id", "duration", "mmv")
    
    mmv.max <- aggregate(mmv ~ duration,
                         data = mmv.temp,
                         FUN = max,
                         na.rm = FALSE)
    
    colnames(mmv.max) <- cols[-1]
    
    if(index == TRUE) {
      
      mmv.merge <- merge(mmv.max, mmv)
      mmv.merge <- mmv.merge[order(mmv.merge$duration), ]
      row.names(mmv.merge) <- 1:nrow(mmv.merge)
      return(mmv.merge)
      
    } else if(index == FALSE) {
      
      colnames(mmv.max) <- cols[-1]
      return(mmv.max)
      
    }
    
  }
  
  if(length(colnames(mmv)) == 4) {
    
    mmv.temp <- mmv
    
    colnames(mmv.temp) <- c(".id", "duration", "mmv1", "mmv2")
    
    mmv.max <- aggregate(cbind(mmv1, mmv2) ~ duration,
                         data = mmv.temp,
                         FUN = max,
                         na.rm = FALSE)
    
    if(index == TRUE) {warning("Indexing is not possible when data frames have inconsistent variable names. 
                               \nThe returned data frame has not been indexed")}
    
    colnames(mmv.max) <- cols[-1]
    mmv.max[mmv.max == 0] <- NA
    
    return(mmv.max)
    
  }
}
