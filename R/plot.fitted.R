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


# plot.model.fit <- function(dur = 600,
#                            log.dur = FALSE,
#                            model.coefs,
#                            model.fits, 
#                            roecker = FALSE) {
#   
#   cs.coef = model.coefs
#   cs.fit = model.fits
#   
#   y.lab = ifelse(roecker == FALSE, 
#                  "Max Mean Velocity (m/sec)", 
#                  "Max Median Velocity (m/sec)")
#   
#   
#   # observations w/ model fit
#   p.obs <- function(log.dur = log.dur) {
#     
#     if(log.dur == FALSE) { plot(max.mean.speed ~ duration, 
#                                 data = cs.fit,
#                                 ylim = c(0, 12.5),
#                                 xlim = c(0, dur),
#                                 col = "grey",
#                                 ylab = y.lab,
#                                 xlab = "Duration (s)") }
#     if(log.dur == TRUE) {
#       
#       ticks <- c(1, 10, 60, 120, 600)
#       ticks.log <- log10(ticks)
#       
#       plot(max.mean.speed ~ dur.log, 
#            data = cs.fit,
#            ylim = c(0, 12.5),
#            xlim = c(0, log10(dur)),
#            col = "grey",
#            ylab = y.lab,
#            xlab = "Log(dur) (s)",
#            xaxt = "n")
#       
#       axis(1,
#            at = ticks.log,
#            labels = ticks)
#       
#     }
#     
#   }
#   
#   p.five.param <- function(log.dur = log.dur) {
#     
#     p.obs(log.dur = log.dur)
#     
#     if(log.dur == FALSE) {lines(cs.fit$five.p.pred ~ cs.fit$duration, col = "red")
#       text(x = dur - 50, y = cs.coef$critical.speed[3] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[3], 2))))}
#     if(log.dur == TRUE) {lines(cs.fit$five.p.pred ~ cs.fit$dur.log, col = "red")
#       text(x = log10(dur - 150), y = cs.coef$critical.speed[3] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[3], 2))))}
#     
#     title(main = "5P")
#     
#     abline(h = cs.coef$critical.speed[3])
#     
#   }
#   
#   p.three.param <- function(log.dur = log.dur) {
#     
#     p.obs(log.dur = log.dur)
#     
#     if(log.dur == FALSE) {lines(cs.fit$three.p.pred ~ cs.fit$duration, col = "green")
#       text(x = dur - 50, y = cs.coef$critical.speed[2] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[2], 2))))}
#     if(log.dur == TRUE) {lines(cs.fit$three.p.pred ~ cs.fit$dur.log, col = "green")
#       text(x = log10(dur - 150), y = cs.coef$critical.speed[2] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[2], 2))))}
#     
#     title(main = "CV3")
#     
#     abline(h = cs.coef$critical.speed[2])
#     
#   }
#   
#   p.two.param <- function(log.dur = log.dur) {
#     
#     p.obs(log.dur = log.dur)
#     
#     if(log.dur == FALSE) {lines(cs.fit$two.p.pred ~ cs.fit$duration, col = "blue")
#       text(x = dur - 50, y = cs.coef$critical.speed[1] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[1], 2))))}
#     if(log.dur == TRUE) {lines(cs.fit$two.p.pred ~ cs.fit$dur.log, col = "blue")
#       text(x = log10(dur - 150), y = cs.coef$critical.speed[1] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[1], 2))))}
#     
#     title(main = "CV2")
#     
#     abline(h = cs.coef$critical.speed[1])
#     
#   }
#   
#   p.omvd <- function(log.dur = log.dur) {
#     
#     p.obs(log.dur = log.dur)
#     
#     if(log.dur == FALSE) {lines(cs.fit$omvd.pred ~ cs.fit$duration, col = "black")
#       text(x = dur - 50, y = cs.coef$critical.speed[4] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[4], 2))))}
#     if(log.dur == TRUE) {lines(cs.fit$omvd.pred ~ cs.fit$dur.log, col = "black")
#       text(x = log10(dur - 150), y = cs.coef$critical.speed[4] +  1,
#            label = paste("AV =", as.character(round(cs.coef$critical.speed[4], 2))))}
#     
#     title(main = "OmVd")
#     
#     abline(h = cs.coef$critical.speed[4])
#     
#   }
#   
#   p.fit <- function(log.dur = log.dur) {
#     
#     par(mfrow = c(2, 2))
#     
#     p.two.param(log.dur = log.dur)
#     p.three.param(log.dur = log.dur)
#     p.five.param(log.dur = log.dur)
#     p.omvd(log.dur = log.dur)
#     
#   }
#   
#   
#   p.fit(log.dur = log.dur)
#   
#   par(mfrow = c(1,1))
#   
# }
