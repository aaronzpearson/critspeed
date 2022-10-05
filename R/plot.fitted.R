## helper functions

# clean fitted plot call
fitted.plot <- function(model.fits,
                        model.coefs,
                        model = c("two.param", "three.param", "five.param", "omvd"),
                        log.dur = FALSE,
                        method = c("mean", "median"),
                        ...) {
  
  callCS("p.fit",
         model.fits,
         model.coefs,
         model = model,
         log.dur = log.dur,
         method = method,
         ...)
  
}

# extended fitted plot call
p.fit <- function(model.fits,
                  model.coefs,
                  model = c("two.param", "three.param", "five.param", "omvd"),
                  log.dur = FALSE,
                  method = c("mean", "median"),
                  ...) {
  
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  
  if(length(method) != 1) {method = "mean"}
  y.lab = paste("Max", .simpleCap(method), "Velocity (m/sec)")
  
  # observations w/ model fit
  p.obs <- function(log.dur = log.dur,
                    y.lab) {
    
    if(log.dur == FALSE) { plot(max.speed ~ duration, 
                                data = model.fits,
                                ylim = c(0, 12.5),
                                xlim = c(0, max(model.fits$duration, na.rm = TRUE)),
                                col = "grey",
                                ylab = y.lab,
                                xlab = "Duration (s)") }
    if(log.dur == TRUE) {
      
      ticks <- c(1, 10, 60, 120, 600)
      ticks.log <- log10(ticks)
      
      plot(max.speed ~ log.duration, 
           data = model.fits,
           ylim = c(0, 12.5),
           xlim = c(0, log10(max(model.fits$duration, na.rm = TRUE))),
           col = "grey",
           ylab = y.lab,
           xlab = "Log(dur) (s)",
           xaxt = "n")
      
      axis(1,
           at = ticks.log,
           labels = ticks)
      
    }
    
  }
  
  p.fitted <- function(log.dur = log.dur,
                       plot.title = "Fitted Plot",
                       y.lab) {
    
    p.obs(log.dur = log.dur,
          y.lab)
    
    if(log.dur == FALSE) {lines(model.fits[[paste0(model, ".fit")]] ~ model.fits$duration, col = "black")
      text(x = max(model.fits$duration, na.rm = TRUE) - 150, y = model.coefs$crit.speed +  1,
           label = paste("AV =", as.character(round(model.coefs$crit.speed, 2))))}
    
    if(log.dur == TRUE) {lines(model.fits[[paste0(model, ".fit")]] ~ model.fits$log.duration, col = "black")
      text(x = log10(max(model.fits$duration, na.rm = TRUE) - 150), y = model.coefs$crit.speed +  1,
           label = paste("AV =", as.character(round(model.coefs$crit.speed, 2))))}
    
    title(main = plot.title)
    
    abline(h = model.coefs$crit.speed)
    
  }
  
  p <- function(log.dur = log.dur,
                plot.title = "Fitted Plot",
                y.lab) {
    
    p.fitted(log.dur = log.dur,
             plot.title = plot.title,
             y.lab)
    
  }
  
  
  p(log.dur = log.dur,
    plot.title = ifelse(missing(...), "Fitted Plot", ...),
    y.lab = y.lab)
  
}