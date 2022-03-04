cs.results.observed <- function(player.speed,
                                global.data = FALSE,
                                sample.rate = 10,
                                cv.2 = 120,
                                dur = 600,
                                d.prime.estim = 100,
                                crit.speed.estim = 3.5,
                                max.speed.estim = 12) {
  
  if(global.data == FALSE) { 
    
    dat = max.mean.speed.df(player.speed = player.speed,
                            sample.rate = sample.rate,
                            dur = dur)
    
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
  
  exponential.decay.model <- cs.exponential.decay(data = dat)
  
  dat.fit <- dat
  dat.fit$dur.log = log(dat.fit$duration)
  
  dat.fit$omni.pred <- predict(extended.model)
  dat.fit$omni.resid <- as.numeric(residuals(extended.model))
  
  dat.fit$five.p.pred <- predict(five.param)
  dat.fit$five.p.resid <- as.numeric(residuals(five.param))
  
  dat.fit$three.p.pred <- predict(three.param)
  dat.fit$three.p.resid <- as.numeric(residuals(three.param))
  
  dat.fit$two.p.pred <- predict(two.param, newdata = dat)
  dat.fit$two.p.resid <- dat$max.mean.speed - predict(two.param, newdata = dat)
  
  dat.fit$exp.pred <- predict(exponential.model)
  dat.fit$exp.resid <- as.numeric(residuals(exponential.model))
  
  return(dat.fit)
  
}
