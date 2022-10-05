## helper functions ##

# clean residual plot call
residuals.plots <- function(model.fits,
                            model = c("two.param", "three.param", "five.param", "omvd"),
                            log.dur = FALSE,
                            ...) {
  
  callCS("p.resid",
         model.fits,
         model = model,
         log.dur = log.dur,
         ...)
  
}

# extended residual plot call
p.resid <- function(model.fits,
                    model = c("two.param", "three.param", "five.param", "omvd"),
                    log.dur = FALSE,
                    ...) {
  
  p.base <- function(y.limits,
                     log.dur = log.dur) {
    
    if(log.dur == FALSE) {
      
      plot(max.speed ~ duration,
           data = model.fits,
           ylim = y.limits,
           xlim = c(0, max(duration)),
           ylab = "Residuals (m/s)",
           xlab = "Duration (s)",
           type = "p", 
           col = "white")
    }
    
    if(log.dur == TRUE) {
      
      ticks <- c(1, 10, 60, 120, 600)
      ticks.log <- log10(ticks)
      
      plot(max.speed ~ log.duration,
           data = model.fits,
           ylim = y.limits,
           xlim = c(0, log10(max(duration))),
           ylab = "Residuals (m/s)",
           xlab = "Log(dur) (s)",
           type = "p",
           xaxt = "n",
           col = "white")
      
      axis(1,
           at = ticks.log,
           labels = ticks)
      
    }
    
    abline(h = 0,
           col = "grey",
           lty = 1)
    
  }
  
  p.resid <- function(log.dur = log.dur,
                      plot.title = "Residuals Plot") {
    
    p.base(y.limits = c(min(model.fits[[paste0(model, ".resid")]], na.rm = TRUE),
                        max(model.fits[[paste0(model, ".resid")]], na.rm = TRUE)),
           log.dur = log.dur)
    
    if(log.dur == FALSE) {lines(model.fits[[paste0(model, ".resid")]] ~ model.fits$duration, col = "red")}
    if(log.dur == TRUE) {lines(model.fits[[paste0(model, ".resid")]] ~ model.fits$log.duration, col = "red")}
    
    title(main = plot.title)
    
  }
  
  p.fit <- function(log.dur = log.dur,
                    plot.title = "Residuals Plot") {
    
   p.resid(log.dur = log.dur,
           plot.title = plot.title)
    
  }
  
  p.fit(log.dur = log.dur,
        plot.title = ifelse(missing(...), "Residuals Plot", ...))
  
}