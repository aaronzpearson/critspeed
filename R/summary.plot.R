## helper functions ##

# clean summary plot call
summary.plot <- function(model.fits,
                         model.coefs,
                         model = c("two.param", "three.param", "five.param", "omvd"),
                         log.dur = FALSE,
                         method = c("mean", "median")) {
  
  callCS("p.summary",
         model.fits = model.fits,
         model.coef = model.coefs,
         model = model,
         log.dur = log.dur,
         method = method)
  
}

# extended summary plot call
p.summary <- function(model.fits,
                         model.coefs,
                         model = c("two.param", "three.param", "five.param", "omvd"),
                         log.dur = FALSE,
                         method = c("mean", "median")) {
  
  par(mfrow = c(2, 2))
  
  callCS("p.fit",
         model.fits = model.fits,
         model.coefs = model.coefs,
         model = model,
         log.dur = log.dur,
         method = method)
  
  callCS("p.resid",
         model.fits = model.fits,
         model = model,
         log.dur = log.dur)
  
  callCS("p.qq",
         model.fits = model.fits,
         model = model)
  
  callCS("p.density",
         model.fits = model.fits,
         model = model)
  
  par(mfrow = c(1, 1))
  
}