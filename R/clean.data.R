#' Clean Raw Player Tracking Data
#' 
#' A data set that contains a player's speed, player's speed in metric, and satellite quality. Data only includes realistic 
#' and attainable player speeds and high satellite quality.
#' 
#' This function is intended to help users clean and process their data. In particular, `clean.data` removes unrealistic player 
#' running values (default set to > 12 m/s), maintains high satellite quality (hdop < 2), and can convert a player's speed 
#' to `m/s`. 
#' 
#' Please note that this function simply removes observations that do not meet inclusion criterion. Users are encouraged 
#' to visualize their data prior to processing it to ensure that the tracking data is realistic. For example, GPS units 
#' are often left on during bus rides and at half-time. In these cases, users should create distinct data sets that 
#' correspond to a player's actual time while wearing the GPS units before processing and modelling the data.
#' 
#' If the player tracking data does not contain satellite quality, set `satellite.quality = NA`. 
#' 
#' This function can convert the following units to m/s: feet (`ft`), yards (yd), kilometers (km), and miles (mi) per 
#' second (s), minute (min), and hour (h). For example, a player's speed in yards per second should be input as `yd/s`.
#'
#' @param player.speed player speed vector
#' @param satellite.quality typically refers the hdop vector
#' @param max.speed player's max attainable speed, default set to 12 m/s
#' @param max.satellite.quality max hdop quality, default set to 2
#' @param metrics metric or unit of the player speed vector
#'
#' @export
clean.data <- function(player.speed,
                       satellite.quality,
                       max.speed = 12,
                       max.satellite.quality = 2,
                       metrics = "m/s") {
  
  dat <- data.frame(duration = 1:length(player.speed))
  
  dat$player.speed = player.speed
  
  suppressWarnings(
    suppressMessages(
      
      if(!is.na(satellite.quality)) { dat$satellite.quality = satellite.quality }    
      
    )
  )
  
  dat$player.speed.metric = convert.to.metric(player.speed, units = metrics)
  
  dat <- subset(dat, player.speed.metric <= max.speed)
  
  suppressWarnings(
    suppressMessages(  
  
      if(!is.na(satellite.quality)) { dat <- subset(dat, satellite.quality <= max.satellite.quality) }
    
      )
  )

  dat   
  
}

convert.to.metric <- function(value, units = "m/s") {
  
  # converts values to metric
  # in m/s
  
  # functions create objects (to_metric & from_metric)
  # from_metric = 1/to_metric
  distance <- convert.to.meters(value, strsplit(units, "/")[[1]][1])
  duration <- convert.to.seconds(strsplit(units, "/")[[1]][2])
  
  distance/ duration
  
}

convert.to.meters <- function(value, units = c("ft", "km", "m", "mi", "yd")) {
  
  # converts ft, km, mi, yd to meters
  # provides some leeway on different spelling of each input
  
  if(units %in% c("meters", "m", "meter")) {
    return(value)
    
  } else if(units %in% c("feet", "ft", "fts", "f")) {
    return(value * 0.3048)
    
  } else if(units %in% c("kilometers", "kilometer", "k", "km")) {
    return(value * 1000)
    
  } else if(units %in% c("miles", "mile", "mi")) {
    return(value * 1609.344)
    
  } else if(units %in% c("yard", "yards", "yd", "yds")) {
    return(value * 0.9144)
    
  }
  
}

convert.to.seconds <- function(units = c("second, minute, hour")) {
  
  # converts min and hours to seconds
  
  if(units %in% c("seconds", "second", "s", "sec")) {
    1
    
  } else if(units %in% c("min", "m", "minute", "minutes")) {
    1*60
    
  } else if(units %in% c("hour", "h", "hr", "hours")) {
    1*60*60
    
  }
  
}

