#' Max Mean and Median Observations
#'
#' A \code{data.frame} that contains the observations that underpin the maximal mean or median speed. 
#' 
#' The \code{speed.observations()} function returns a \code{data.frame} that contains three variables: \code{index}, \code{time} and 
#' \code{player.speed}. The \code{index} variable corresponds to which vector the observations came from, and \code{time} variable 
#' corresponds to the time, or duration, that the observations correspond to. 
#' 
#' When there is one speed vector input, the default index is \code{1}.
#'
#' @param player.speed the player's speed vector
#' @param ... supplemental speed vectors 
#' @param dur duration of length 1L to calculate max mean or max median speed
#' @param sample.rate in Hz, default set to 10 Hz
#' @param method indicates whether the max mean or max median speed should be calculated
#'
#' @export
#' 
#' @examples 
#' 
#' data(sessionRaw)
#' 
#' # Max Mean Speed Observations
#' 
#' max.mean.observations <- speed.observations(player.speed = sessionRaw$speed,
#' dur = 20,
#' sample.rate = 10,
#' method = "mean")
#' 
#' # head(max.mean.observations)
#' 
#' # Max Median Speed Observations
#' 
#' max.median.observations <- speed.observations(player.speed = sessionRaw$speed,
#' dur = 20,
#' sample.rate = 10,
#' method = "median")
#' 
#' # head(max.median.observations)
speed.observations <- function(player.speed,
                               ...,
                               dur = NULL,
                               sample.rate = 10,
                               method = c("mean", "median")) {
  
  callCS("observations", 
         player.speed = player.speed, 
         ... = ...,
         dur = dur, 
         sample.rate = sample.rate, 
         method = method)
  
}

## helper functions ##
observations <- function(player.speed,
                         ...,
                         dur = NULL,
                         sample.rate = 10,
                         method = c("mean", "median")) {
  
  dat <- list(player.speed, ...)
  n. <- dur * sample.rate
  
  if(method != "median") {method = "mean"}
  
  mmv.max <- switch(method,
                    
                    mean = observation.mean(data = dat,
                                            duration = n.),
                    
                    median = observation.median(data = dat,
                                                duration = n.))
  
  mmv.max.index <- which.max(mmv.max$max.mean.speed) 
  
  invisible(gc())
  
  
  ## pull observations from the specific vector
  observation.vector <- dat[[mmv.max.index]]
  player.speed.df <- data.frame(index = mmv.max.index,
                                time = seq(0.1, length(observation.vector)/10, by = 1/sample.rate),
                                player.speed = observation.vector)
  
  if(method == "mean") { max.mmv <- which.max(data.table::frollmean(observation.vector, n = n.)) }
  if(method == "median") { max.mmv <- which.max(data.table::frollapply(observation.vector, n = n., FUN = Rfast::med)) }
  
  player.speed.df <- player.speed.df[(max.mmv-(n.-1)): max.mmv, ]
  row.names(player.speed.df) <- NULL
  return(player.speed.df)
  
}


# max mean indexing
observation.mean <- function(data = list(), 
                             duration = NULL) {
  
  mmv.max <- data.frame()
  for(i in seq_along(data)) {
    
    mmv <- max(data.table::frollmean(data[[i]], n = duration), na.rm = TRUE)
    mmv.temp <- data.frame(index = i,
                           duration = duration,
                           max.mean.speed = mmv)
    
    mmv.max <- rbind(mmv.max, mmv.temp)
    
  }
  
  return(mmv.max)
  
}


# max median indexing
observation.median <- function(data = list(),
                               duration = NULL) {
  
  mmv.max <- data.frame()
  for(i in seq_along(data)) {
    
    mmv <- max(RollingWindow::RollingMedian(data[[i]], window = duration), na.rm = T)
    mmv.temp <- data.frame(index = i,
                           duration = duration,
                           max.median.speed = mmv)
    
    mmv.max <- rbind(mmv.max, mmv.temp)
    
  }
  
  return(mmv.max)
  
}
