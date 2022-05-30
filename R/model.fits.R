#' Fitted Results
#' 
#' This function returns the set duration, log(duration), and the model fits and residuals for the OmVD, 5 parameter, 
#' 3 parameter, and 2 parameter, exponential, and exponential decay models.
#' 
#' Using nonlinear regression, `minpack.lm::nlsLM`, you can fit the six mathematical models (CV2, CV3, OmVD, 5-PL, exp, exp decay). 
#' The initial values for the regressions are `crit.speed.estim = 3.5` in m/s, `d.prime.estim = 150` in m, and 
#' `max.speed.estim = 12` in  m/s. These values were chosen after nonlinear regressions with an array of starting values 
#' (1, 3.5, and 5 for CV; 80, 150, and 1500 for D’; and 7, 12 and 15 for Vmax) converged to the same final values.  
#' The regression optimized parameters using the Levenberg-Marquardt algorithm to minimize the residual sum-of-squares (RSS).  
#' 
#' `cs.results.fitted` returns a data frame that contains the selected duration, 
#' the player's max mean or max median speed vector, model fits, and model residuals. The returned data
#' frame can then be used to produce plots like those created using the `cs.results.plot` function.
#' 
#' `raw.data` should be set to `FALSE` if it is pre-processed max mean or max median speed data. To 
#' pre-process the data, the functions `critspeed`, `max.mean.speed.df` or `max.median.speed.df` must be called, and the 
#' resulting data frame saved as an object or loaded from your device.
#' 
#' If `raw.data` is set to `TRUE`, you can opt for the duration, `dur`, to be set to one (1L) or "roecker" 
#' values. Setting `dur` to a single value will return fitted values for 1/`sample.rate` to the desired max duration. When 
#' `dur` is set to "roecker", the default durations are 0.3, 0.5, 1, 2, 3, 4, 5, 6.5, 10, 13.5, 18,
#' 30, 60, 120, 300, 600, 900, 1200, 1800, 2400, and 2700 s. If `roecker` is set to `TRUE` and a single value 
#' is provided, the durations will be 1/`sample.rate` to the assigned `dur`. If `roecker` is set to `TRUE` and multiple values 
#' are provided, the provided duration vector will be assessed in s. Please note that when interested in the max 
#' mean speed, `dur` MUST be a duration of 1L (a single value) and `rocker` MUST be set to `FALSE`.
#' 
#' The `d.prime.estim`, `crit.speed.estim`, and `max.speed.estim` should be set to realistic values. The default
#' values have been validated. If different values are selected, the results should be either similar or the same 
#' when the nls function (`minpack.lm::nlsLM`) attempts to minimize the RSS. 
#' 
#' The 2 parameter model has been validated when the minimum duration is set to 120 s, or 2 min. This value, `cv.2`, 
#' can be over-written. Please note that by selecting a different minimum duration will return different 
#' model results. Although they can be similar, they are not the same and can vary greatly. 
#' 
#' @note Errors can arise when `dur` and `roecker` are used inappropriately. Errors can also arise when the 
#' data set does not contain enough observations. Ensure that you have properly utilized your data and the provided 
#' arguments when errors are returned.
#' 
#' @param player.speed player's speed vector
#' @param raw.data data type being either raw (unprocessed) or critspeed (processed), default set to TRUE
#' @param sample.rate in Hz, default set to 10
#' @param cv.2 start duration for CV2, default set to 120 s
#' @param dur duration, default set to 600
#' @param d.prime.estim D' estimate, default set to 100 m
#' @param crit.speed.estim critical speed estimate, default set to 3.5 m/s
#' @param max.speed.estim max speed, default set to 12 m/s
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#'
#' @export
model.fits <- function(player.speed,
                      raw.data = TRUE,
                      sample.rate = 10,
                      cv.2 = 120,
                      dur = 600,
                      d.prime.estim = 100,
                      crit.speed.estim = 3.5,
                      max.speed.estim = 12,
                      roecker = FALSE) {
  
  cs.results.fitted(player.speed = player.speed,
                    raw.data = raw.data,
                    sample.rate = sample.rate,
                    cv.2 = cv.2,
                    dur = dur,
                    d.prime.estim = d.prime.estim,
                    crit.speed.estim = crit.speed.estim,
                    max.speed.estim = max.speed.estim,
                    roecker = roecker)
  
  }

cs.results.fitted <- function(player.speed,
                              raw.data = TRUE,
                              sample.rate = 10,
                              cv.2 = 120,
                              dur = 600,
                              d.prime.estim = 100,
                              crit.speed.estim = 3.5,
                              max.speed.estim = 12,
                              roecker = FALSE) {
  
  # helper function for model.fit()
  
  # calculate max mean speed of raw data
  if(raw.data == TRUE & 
     roecker == FALSE) {

    dat = max.mean.speed.df(player.speed = player.speed,
                            sample.rate = sample.rate,
                            dur = dur)
    
    # calculate max median speed of raw data
  } else if(raw.data == TRUE &
            roecker == TRUE) {
    
    dat = max.median.speed.df(player.speed = player.speed,
                              sample.rate = sample.rate,
                              dur = dur)
    
    # assigns max.median.speed variable to max.mean.speed for future model fits
    # reassigned as max.median.speed before being returned to the user
    colnames(dat) <- c("duration", "max.mean.speed")
    
    } else if(raw.data == FALSE) {

    # builds a simple data frame with pre-processed (global) data
    dat <- data.frame(max.mean.speed = player.speed,
                      duration = (1:length(player.speed))/ sample.rate)
    # dat <- dat[1:(dur*sample.rate), ]

  }

  # builds a data set specifically for the 2p model
  # can take on a min duration >= 0
  dat.cv2 <- subset(dat, duration >= cv.2)
  
  # modelling process
  extended.model <- cs.extended.model(data = dat,
                                      d.prime.estim = d.prime.estim,
                                      crit.speed.estim = crit.speed.estim,
                                      max.speed.estim = max.speed.estim)

  five.param <- cs.five.param(data = dat,
                              crit.speed.estim = crit.speed.estim,
                              max.speed.estim = max.speed.estim)

  three.param <- cs.three.param(data = dat,
                                d.prime.estim = d.prime.estim,
                                crit.speed.estim = crit.speed.estim,
                                max.speed.estim = max.speed.estim)

  two.param <- cs.two.param(data = dat.cv2,
                            d.prime.estim = d.prime.estim,
                            crit.speed.estim = crit.speed.estim)

  exponential.model <- cs.exponential(data = dat,
                                      crit.speed.estim = crit.speed.estim,
                                      max.speed.estim = max.speed.estim)

  # exponential.decay.model <- cs.exponential.decay(data = dat)

  # builds extensive data set from initial data
  # does not overwrite dat for troubleshooting needs
  dat.fit <- dat[1:(dur*sample.rate), ]
  dat.fit$dur.log = log10(dat.fit$duration)

  dat.fit$omni.pred <- predict(extended.model, newdata = dat.fit)
  dat.fit$omni.resid <- dat.fit$omni.pred - dat.fit$max.mean.speed
  
  dat.fit$five.p.pred <- predict(five.param, newdata = dat.fit)
  dat.fit$five.p.resid <- dat.fit$five.p.pred - dat.fit$max.mean.speed

  dat.fit$three.p.pred <- predict(three.param, newdata = dat.fit)
  dat.fit$three.p.resid <- dat.fit$three.p.pred - dat.fit$max.mean.speed

  dat.fit$exp.pred <- predict(exponential.model, newdata = dat.fit)
  dat.fit$exp.resid <- dat.fit$exp.pred - dat.fit$max.mean.speed

  dat.cv2$two.p.pred <- predict(two.param)
  dat.cv2$two.p.resid <- dat.cv2$max.mean.speed - dat.cv2$two.p.pred
  
  # merge 2p model with all other models my duration and max.mean.speed
  dat.fit <- merge(dat.fit,
                   dat.cv2,
                   by = c("duration", "max.mean.speed"),
                   all = TRUE)

  # explicitly sorts duration from 0.1 to `dur` for plotting purposes
  # sometimes plots out of order without this step
  dat.fit <- dat.fit[order(dat.fit$duration), , drop = FALSE]
  
  # returns a max duration of dur * sample.rate
  # avoids errors that can arise when dat, dat.fit, and dat.cv2 are of different lengths (nrow) 
  dat.fit <- dat.fit[1:(dur*sample.rate), ]
  
  # reassign max.mean.speed to max.median.speed if roecker is TRUE
  if(roecker == TRUE) {colnames(dat.fit)[2] <- "max.median.speed"}

  return(dat.fit)

}
