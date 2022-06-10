#' Plots for Model Fits, Residuals, and Normality
#' 
#' Provides the user the ability to produce plots that can return model fits, residuals, and normality as 
#' represented by Q-Q plots.
#' 
#' This function calls upon `model.fits` and `model.results` to produce the following plots: 1. critspeed data with the 
#' model fits over-layed, an asymptote that represents the player's critical speed, and the critical speed value (`fit`), 2. 
#' the plotted residuals for each model (`resid`), and 3. quantile-quantile (Q-Q) plot as a test of normality (`qq`). Each plot is 
#' made-up of four panes, each of which represents either the 2 parameter, 3 parameter, 5 parameter, or OmVD models. 
#' These models were selected because they have been validated as part of the dissertation of Eli Mizelman, PhD. 
#' 
#' The user has the option to have one, two, or all plots returned in the `Plots` pane (when using RStudio). The 
#' user can then save the plots manually. 
#' 
#' @note If users would like to produce plots that are representative of different models, they can do so 
#' manually by calling `model.fits` and `model.results` and using the `cs.results.plot` function as a guide. 
#' 
#' If the user is returned an `Error`, please consult the `model.fits` and `model.results` documentation to 
#' troubleshoot. `dur` and `roecker` are typically the main culprits when errors arise and the most-common 
#' issues are outlined in the `model.fits` and `model.results` documentation. 
#' 
#' @seealso model.fits.plot, model.residuals.plot, model.qq.plot 
#'
#' @param player.speed player speed vector
#' @param sample.rate in Hz, default set to 10 Hz
#' @param dur max duration, default set to 600 s
#' @param raw.data data type being either raw (unprocessed, TRUE) or critspeed (processed, FALSE), default set to TRUE
#' @param cv.2 start duration for CV2, default set to 120 s
#' @param d.prime.estim D' estimate, default set to 100 m
#' @param crit.speed.estim critical speed estimate, default set to 3.5 m/s
#' @param max.speed.estim max speed, default set to 12 m/s
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#' @param log.dur should the fitted plots be returned on a log10 scale, default set to FALSE
#' @param plots selection of plots to be returned, includes model fits, residuals, and normality (qq)
#'
#' @export
model.plots <- function(player.speed,
                       raw.data = TRUE,
                       sample.rate = 10,
                       dur = 600,
                       roecker = FALSE,
                       cv.2 = 120,
                       d.prime.estim = 150,
                       crit.speed.estim = 3.5,
                       max.speed.estim = 12,
                       log.dur = FALSE,
                       plots = c("fit", "resid", "qq")) {
  
  cs.results.plot(player.speed = player.speed,
                  raw.data = raw.data,
                  sample.rate = sample.rate,
                  dur = dur,
                  roecker = roecker,
                  cv.2 = cv.2,
                  d.prime.estim = d.prime.estim,
                  crit.speed.estim = crit.speed.estim,
                  max.speed.estim = max.speed.estim,
                  log.dur = log.dur,
                  plots = plots)
  
}

cs.results.plot <- function(player.speed,
                            raw.data = TRUE,
                            sample.rate = 10,
                            dur = 600,
                            roecker = FALSE,
                            cv.2 = 120,
                            d.prime.estim = 150,
                            crit.speed.estim = 3.5,
                            max.speed.estim = 12,
                            log.dur = FALSE,
                            plots = c("fit", "resid", "qq")) {

  # return model coefficients
  cs.coef <- cs.results.model(player.speed = player.speed,
                                raw.data = raw.data,
                                sample.rate = sample.rate,
                                dur = dur,
                                cv.2 = cv.2,
                                d.prime.estim = d.prime.estim,
                                crit.speed.estim = crit.speed.estim,
                                max.speed.estim = max.speed.estim, 
                              roecker = roecker)

  # return model fits
  cs.fit <- cs.results.fitted(player.speed = player.speed,
                                raw.data = raw.data,
                                sample.rate = sample.rate,
                                dur = dur,
                                cv.2 = cv.2,
                                d.prime.estim = d.prime.estim,
                                crit.speed.estim = crit.speed.estim,
                                max.speed.estim = max.speed.estim,
                              roecker = roecker)

  
  # output
  if("fit" %in% plots) { cs.model.fits.plot(dur = dur, 
                                         model.coefs = cs.coef, 
                                         model.fits = cs.fit, 
                                         log.dur = log.dur,
                                         roecker = roecker) }
  
  if(length(plots) > 1) {Sys.sleep(3)}
  
  if("resid" %in% plots) { cs.model.residuals.plot(dur = dur,
                                              log.dur = log.dur,
                                              model.fits = cs.fit,
                                              roecker = roecker) }
  
  if(length(plots) > 2) {Sys.sleep(3)}
  
  if("qq" %in% plots) { cs.model.normality.plot(model.fits = cs.fit) }
  

  par(mfrow = c(1,1))

}
