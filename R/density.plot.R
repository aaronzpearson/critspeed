## helper functions

# clean density plot call
density.plot <- function(model.fits,
                    model = c("two.param", "three.param", "five.param", "omvd"),
                    ...) {
  
  callCS("p.density",
         model.fits = model.fits,
         model = model,
         ...)
  
}

# extended density plot call
p.density <- function(model.fits,
                      model = c("two.param", "three.param", "five.param", "omvd"),
                      ...) {
  
  x <- na.omit(model.fits[[paste0(model, ".resid")]])
  
  plot(
    density(
     x
    ),
    main = ifelse(missing(...), "Density Plot", ...)
  )
  
}