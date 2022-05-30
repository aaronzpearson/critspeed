#' Model Fits Plots 
#' 
#' This function is a helper function for `model.plots` when `plots = "fit"`. 
#' 
#' When calling this function explicitly, the argument `model.fits` and `model.coefs` takes on the data frame that 
#' is returned when calling `model.fits` and `model.results`, respectively.
#'
#' @param dur max duration, default set to 600 s
#' @param log.dur should the fitted plots be returned on a log10 scale, default set to FALSE
#' @param roecker indicates whether multiple durations are provided, default set to FALSE
#' @param model.fits data set returned when calling critspeed::model.fits
#' @param model.coefs data set returned when calling critspeed::model.results
#'
#' @export
model.fits.plot <- function(dur = 600,
                        log.dur = FALSE,
                        model.coefs,
                        model.fits, 
                        roecker = FALSE) {
  
  # a cleaner function call for cs.model.fit.plot
  cs.model.fits.plot(dur = dur,
                    log.dur = log.dur,
                    model.coefs = model.coefs,
                    model.fits = model.fits,
                    roecker = roecker)
  
}


cs.model.fits.plot <- function(dur = 600,
                              log.dur = FALSE,
                              model.coefs,
                              model.fits, 
                              roecker = FALSE) {
  
  cs.coef = model.coefs
  cs.fit = model.fits
  
  y.lab = ifelse(roecker == FALSE, "Max Mean Velocity (m/sec)", "Max Median Velocity (m/sec)")
  
  
  # observations w/ model fit
  p.obs <- function(log.dur = log.dur) {
    
    if(log.dur == FALSE) { plot(max.mean.speed ~ duration, 
                                data = cs.fit,
                                ylim = c(0, 12.5),
                                xlim = c(0, dur),
                                col = "grey",
                                ylab = y.lab,
                                xlab = "Duration (s)") }
    if(log.dur == TRUE) {
      
      ticks <- c(1, 10, 60, 120, 600)
      ticks.log <- log10(ticks)
      
      plot(max.mean.speed ~ dur.log, 
           data = cs.fit,
           ylim = c(0, 12.5),
           xlim = c(0, log10(dur)),
           col = "grey",
           ylab = y.lab,
           xlab = "Log(dur) (s)",
           xaxt = "n")
      
      axis(1,
           at = ticks.log,
           labels = ticks)
      
    }
    
  }
  
  p.five.param <- function(log.dur = log.dur) {
    
    p.obs(log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$five.p.pred ~ cs.fit$duration, col = "red")
      text(x = dur - 50, y = cs.coef$critical.speed[5] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[5], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$five.p.pred ~ cs.fit$dur.log, col = "red")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[5] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[5], 2))))}
    
    title(main = "5P")
    
    abline(h = cs.coef$critical.speed[5])
    
  }
  
  p.three.param <- function(log.dur = log.dur) {
    
    p.obs(log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$three.p.pred ~ cs.fit$duration, col = "green")
      text(x = dur - 50, y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[4], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$three.p.pred ~ cs.fit$dur.log, col = "green")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[4], 2))))}
    
    title(main = "CV3")
    
    abline(h = cs.coef$critical.speed[4])
    
  }
  
  p.two.param <- function(log.dur = log.dur) {
    
    p.obs(log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$two.p.pred ~ cs.fit$duration, col = "blue")
      text(x = dur - 50, y = cs.coef$critical.speed[3] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[3], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$two.p.pred ~ cs.fit$dur.log, col = "blue")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[3] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[3], 2))))}
    
    title(main = "CV2")
    
    abline(h = cs.coef$critical.speed[3])
    
  }
  
  p.omni <- function(log.dur = log.dur) {
    
    p.obs(log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(cs.fit$omni.pred ~ cs.fit$duration, col = "black")
      text(x = dur - 50, y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[6], 2))))}
    if(log.dur == TRUE) {lines(cs.fit$omni.pred ~ cs.fit$dur.log, col = "black")
      text(x = log10(dur - 150), y = cs.coef$critical.speed[4] +  1,
           label = paste("AV =", as.character(round(cs.coef$critical.speed[6], 2))))}
    
    title(main = "OmVd")
    
    abline(h = cs.coef$critical.speed[6])
    
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