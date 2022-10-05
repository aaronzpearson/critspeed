## helper functions

# clean qq plot call
qq.plot <- function(model.fits,
                    model = c("two.param", "three.param", "five.param", "omvd"),
                    ...) {
  
  callCS("p.qq",
         model.fits = model.fits,
         model = model,
         ...)
  
}

# extended qq plot call
p.qq <- function(model.fits,
                 model = c("two.param", "three.param", "five.param", "omvd"),
                 ...) {
  
  qqplot(model.fits[[paste0(model, ".fit")]], model.fits$max.speed,
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles")
  
  qqline(model.fits[[paste0(model, ".fit")]], col = "grey")
  
  
  title(main = ifelse(missing(...), "Q-Q Plot", ...))
  
  
  
}