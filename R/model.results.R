#' Critical Speed Model Results
#' 
#' This function returns the fitted parameter estimates for the the OmVD, 5 parameter, 
#' 3 parameter, and 2 parameter, and exponential models. The exponential decay model is omitted 
#' because of parameter redundancies with the exponential model.
#' 
#' Unlike `model.fits` that returns the fitted values, this function returns the optimized parameters using 
#' the Levenberg-Marquardt algorithm that was used to minimize the residual sum-of-squares (RSS). Using nonlinear regression, 
#' `minpack.lm::nlsLM`, you can fit the five mathematical models (CV2, CV3, OmVD, 5-PL, exp). The initial values 
#' for the regressions are `crit.speed.estim = 3.5` in m/s, `d.prime.estim = 150` in m, and `max.speed.estim = 12` 
#' in  m/s. These values were chosen after nonlinear regressions with an array of starting values (1, 3.5, and 5 
#' for CV; 80, 150, and 1500 for D’; and 7, 12 and 15 for Vmax) converged to the same final values. Goodness of fit 
#' values are also included in the output and include root square error (RSE), residual sum-of-squares (RSS),
#' Akaike's Inclusion Criterion (AIC), and Bayesion Information Criterion (BIC). 
#' 
#' `global.data` should be set to `TRUE` if it is pre-processed max mean or max median speed data. To 
#' pre-process the data, the functions `max.mean.speed.df` or `max.median.speed.df` must be called, and the 
#' resulting data frame saved as an object or loaded from your device.
#' 
#' If `global.data` is set to `FALSE`, you can opt for the duration, `dur`, to be set to one (1L) or "roecker" 
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
#' arguments when errors are returned.#' 
#' 
#' @param player.speed player's speed vector
#' @param raw.data data type being either raw (unprocessed) or critspeed (processed), defaults set to TRUE
#' @param sample.rate in Hz, default set to 10
#' @param cv.2 start duration for CV2, default set to 120 s
#' @param dur duration, default set to 600
#' @param d.prime.estim D' estimate, default set to 150 m
#' @param crit.speed.estim critical speed estimate, default set to 3.5 m/s
#' @param max.speed.estim max speed, default set to 12 m/s
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#'
#' @export
model.results <- function(player.speed,
                          raw.data = TRUE,
                          sample.rate = 10,
                          cv.2 = 120, # minimum duration (default = 2 minutes)
                          dur = 600,
                          d.prime.estim = 150,
                          crit.speed.estim = 3.5,
                          max.speed.estim = 12,
                          roecker = FALSE) {
  
  cs.results.model(player.speed = player.speed,
                   raw.data = raw.data,
                   sample.rate = sample.rate,
                   cv.2 = cv.2, # minimum duration (default = 2 minutes)
                   dur = dur,
                   d.prime.estim = d.prime.estim,
                   crit.speed.estim = crit.speed.estim,
                   max.speed.estim = max.speed.estim,
                   roecker = roecker)
  
}

cs.results.model <- function(player.speed,
                             raw.data = TRUE,
                             sample.rate = 10,
                             cv.2 = 120, # minimum duration (default = 2 minutes)
                             dur = 600,
                             d.prime.estim = 150,
                             crit.speed.estim = 3.5,
                             max.speed.estim = 12,
                             roecker = FALSE) {

  if(raw.data == TRUE & 
     roecker == FALSE) {

    dat = max.mean.speed.df(player.speed = player.speed,
                            sample.rate = sample.rate,
                            dur = dur)
    
    colnames(dat) <- c("duration", "max.mean.speed")

  } else if(raw.data == TRUE & 
            roecker == TRUE) {
    
    dat = max.median.speed.df(player.speed = player.speed,
                              sample.rate = sample.rate,
                              dur = dur)
    
    colnames(dat) <- c("duration", "max.mean.speed")
    
  } else {

    dat <- data.frame(max.mean.speed = player.speed,
                      duration = (1:length(player.speed))/ sample.rate)

  }

  dat.cv2 <- subset(dat, duration >= cv.2)

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

  # inverse.model <- cs.inverse(data = dat,
  #                             d.prime.estim = d.prime.estim,
  #                             crit.speed.estim = crit.speed.estim)

  cs.results <- data.frame(model = c(#"Exponential Decay",
                                     "Exponential Model",
                                     "Two Parameter",
                                     "Three Parameter",
                                     "Five Parameter",
                                     "OmniDomain"),

                           critical.speed = c(
                             # coef(exponential.decay.model)[[3]],
                             coef(exponential.model)[[1]],
                             coef(two.param)[[2]],
                             coef(three.param)[[2]],
                             coef(five.param)[[4]],
                             coef(extended.model)[[2]]
                           ),



                           d.prime = c(
                             # NA,
                             NA,
                             coef(two.param)[[1]],
                             coef(three.param)[[1]],
                             NA,
                             coef(extended.model)[[1]]
                           ),

                           max.speed = c(
                             # coef(exponential.decay.model)[[1]],
                             # predict(exponential.decay.model, newdata = data.frame(duration = 1)),
                             coef(exponential.model)[[2]],
                             NA,
                             coef(three.param)[[3]],
                             coef(five.param)[[5]],
                             coef(extended.model)[[3]]
                           ),

                           rse = c(
                             # summary(exponential.decay.model)[[3]],
                             summary(exponential.model)[[3]],
                             summary(two.param)[[3]],
                             summary(three.param)[[3]],
                             summary(five.param)[[3]],
                             summary(extended.model)[[3]]
                           ),

                           rss = c(
                             # summary(exponential.decay.model)[[3]]^2 * summary(exponential.decay.model)[[4]][2],
                             summary(exponential.model)[[3]]^2 * summary(exponential.model)[[4]][2],
                             summary(two.param)[[3]]^2 * summary(two.param)[[4]][2],
                             summary(three.param)[[3]]^2 * summary(three.param)[[4]][2],
                             summary(five.param)[[3]]^2 * summary(five.param)[[4]][2],
                             summary(extended.model)[[3]]^2 * summary(extended.model)[[4]][2]
                           ),

                           aic = c(
                             # AIC(exponential.decay.model),
                             AIC(exponential.model),
                             AIC(two.param),
                             AIC(three.param),
                             AIC(five.param),
                             AIC(extended.model)
                           ),

                           bic = c(
                             # BIC(exponential.decay.model),
                             BIC(exponential.model),
                             BIC(two.param),
                             BIC(three.param),
                             BIC(five.param),
                             BIC(extended.model)
                           )
  )

  cs.results
}
