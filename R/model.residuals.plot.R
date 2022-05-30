#' Model Residuals Plots
#' 
#' A residuals plot for the 2P, 3P, 5P, and OmVD models.
#' 
#' This function is a helper function for `model.plots` when `plots = "resid"`. 
#' 
#' When calling this function explicitly, the argument `model.fits` takes on the data frame that 
#' is returned when calling `model.fits`.
#'
#' @param dur max duration, default set to 600 s
#' @param log.dur should the fitted plots be returned on a log10 scale, default set to FALSE
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#' @param model.fits data set returned when calling critspeed::model.fits
#'
#' @export
model.residuals.plot <- function(dur = 600, 
                                 log.dur = FALSE, 
                                 model.fits,
                                 roecker = FALSE) {
  
  cs.model.residuals.plot(dur = dur,
                          log.dur = log.dur,
                          model.fits = model.fits, 
                          roecker = roecker)
  
}

cs.model.residuals.plot <- function(dur = 600,
                                    log.dur = FALSE,
                                    model.fits, 
                                    roecker = FALSE) {

  cs.fit = model.fits
  
  if(roecker == TRUE) {cs.fit$max.mean.speed = cs.fit$max.med.speed}
  
  # base plot
  p.base <- function(y.limits,
                     log.dur = log.dur) {
    
    if(log.dur == FALSE) {
      
      plot(max.mean.speed ~ duration,
           data = cs.fit,
           ylim = y.limits,
           xlim = c(0, dur),
           ylab = "Residuals (m/s)",
           xlab = "Duration (s)",
           type = "p")
    }
    
    if(log.dur == TRUE) {
      
      ticks <- c(1, 10, 60, 120, 600)
      ticks.log <- log10(ticks)
      
      plot(max.mean.speed ~ dur.log,
           data = cs.fit,
           ylim = y.limits,
           xlim = c(0, log10(dur)),
           ylab = "Residuals (m/s)",
           xlab = "Log(dur) (s)",
           type = "p",
           xaxt = "n")
      
      axis(1,
           at = ticks.log,
           labels = ticks)
      
      }
    
    abline(h = 0,
           col = "grey",
           lty = 1)
    
  }

  
  p.five.param <- function(log.dur = log.dur) {
    
    p.base(y.limits = c(min(cs.fit$five.p.resid, na.rm = TRUE),
                        max(cs.fit$five.p.resid, na.rm = TRUE)),
           log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$five.p.resid ~ cs.fit$duration, col = "red")}
    if(log.dur == TRUE) {lines(cs.fit$five.p.resid ~ cs.fit$dur.log, col = "red")}
    
    title(main = "5P Residuals")
    
  }
  
  p.three.param <- function(log.dur = log.dur) {
    
    p.base(y.limits = c(min(cs.fit$three.p.resid, na.rm = TRUE),
                        max(cs.fit$three.p.resid, na.rm = TRUE)),
           log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$three.p.resid ~ cs.fit$duration, col = "green")}
    if(log.dur == TRUE) {lines(cs.fit$three.p.resid ~ cs.fit$dur.log, col = "green")}
    
    title(main = "3P Residuals")
    
  }
  
  p.two.param <- function(log.dur = log.dur) {
    
    p.base(y.limits = c(min(cs.fit$two.p.resid, na.rm = TRUE),
                        max(cs.fit$two.p.resid, na.rm = TRUE)),
           log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$two.p.resid ~ cs.fit$duration, col = "blue")}
    if(log.dur == TRUE) {lines(cs.fit$two.p.resid ~ cs.fit$dur.log, col = "blue")}
    
    title(main = "2P Residuals")
    
  }
  
  p.omni <- function(log.dur = log.dur) {
    
    p.base(y.limits = c(min(cs.fit$omni.resid, na.rm = TRUE),
                        max(cs.fit$omni.resid, na.rm = TRUE)),
           log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$omni.resid ~ cs.fit$duration, col = "black")}
    if(log.dur == TRUE) {lines(cs.fit$omni.resid ~ cs.fit$dur.log, col = "black")}
    
    title(main = "OmVD Residuals")
    
  }
  
  p.fit <- function(log.dur = log.dur) {
    
    par(mfrow = c(2, 2))
    
    p.two.param(log.dur = log.dur)
    p.three.param(log.dur = log.dur)
    p.five.param(log.dur = log.dur)
    p.omni(log.dur = log.dur)
    
  }
  
  
  p.fit(log.dur = log.dur)
  
  par(mfrow = c(1,1))
  
}