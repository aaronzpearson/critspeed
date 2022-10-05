#' Fitted Model Values
#' 
#' A `data.frame` that contains the fitted model values. 
#' 
#' `fitted.model()` is similar to the `stats` `fitted()` and `residuals()` functions. This function is more complex in that 
#' it returns the original data, and the fitted and residual values for the selected models. This gives users the ability to generate 
#' `data.frame`s that best suit their goals. 
#' 
#' The `data.frame` that is returned will always include `duration`, `log.duration`, and `max.speed` variables. It was decided that `max.speed` should 
#' represent maximal mean speed and maximal median speed. Discriminating between the two is impractical for the intended uses of this function. 
#'
#' @param data data set containing at least two variables (see notes)
#' @param model critical speed model name as either `two.param`, `three.param`, `five.param`, or `omvd`
#' @param cv.2 minimum duration to fit the two parameter function, default set to 120 s (see notes)
#' @param lower lower bounds for parameter fits (see notes)
#' @param upper upper bounds for parameter fits (see notes)
#' @param start starting values for parameter fits (see notes)
#' 
#' @note 
#' 
#' Users must be aware of the following requirements for the following arguments:    
#' 
#' \enumerate{
#' \item `data` is a `data.frame` that must satisfy the following criteria:
#' \itemize{
#' \item The first column is the duration variable
#' \item The second column is the maximal mean speed or maximal median speed variable
#' \item Subsequent columns will be disregarded
#' }
#' 
#' Users can generate appropriate `data.frame`s using the `critspeed()` function
#' 
#' \item `cv.2` is the minimum duration used to fit the two parameter model. The default is set to 120 s as suggested by the prevailing 
#' literature.
#' 
#' \item `lower`, `upper`, and `start` are the lower and upper bounds for the parameter fits, and initial parameter values, respectively 
#' The default values are set based on various considerations including the prevailing literature and trial-and-error. The defaults are 
#' outlined below in the form of `parameter: lower bound, start value, upper bound`:
#' \itemize{
#' \item d prime: 10, 150, 800 in meters
#' \item critical speed: 1, 4, 5.8 in meters per second
#' \item maximal speed: 4, 9, 12 in meters per second
#' \item a: NA, -2.71, NA and is unitless
#' \item b: NA, 2.195, NA and is unitless
#' \item f: NA, 0.2, NA and is unitless
#' }
#' 
#' }
#'
#' @export fitted.model
#' @rdname fitted.model
#' 
#' @seealso [model], [model.parameters], [critspeed]
fitted.model <- function(data,
                         model = c("two.param", "three.param", "five.param", "omvd"),
                         cv.2 = 120,
                         lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
                         upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
                         start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
  
  callCS("fit.model",
         data = data,
         model = model,
         cv.2 = cv.2, 
         lower = lower,
         upper = upper,
         start = start)
  
}

## helper function ##

# extended nls fitted values call
fit.model <- function(data,
                         model = c("two.param", "three.param", "five.param", "omvd"),
                         cv.2 = 120,
                         lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
                         upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
                         start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
  

  colnames(data) <- c("duration", "max.mean.speed")
  
  fitted.data <- data.frame(duration = data$duration,
                            log.duration = log10(data$duration),
                            max.speed = data$max.mean.speed)
  
  for(i in seq_along(model)) {
    
    fit <- callCS("models",
                  model = model[i],
                  data = data,
                  cv.2 = cv.2,
                  lower = lower,
                  upper = upper,
                  start = start)
    
    model.fit <- fit$m$fitted()
    model.resid <- fit$m$resid()
    
    n.na <- nrow(fitted.data) - length(model.fit)
    
    if(n.na > 0) {model.fit <- c(rep(NA, n.na), model.fit)}
    if(n.na > 0) {model.resid <- c(rep(NA, n.na), model.resid)}
    
    fitted.data[, ncol(fitted.data) + 1] <- model.fit
    fitted.data[, ncol(fitted.data) + 1] <- model.resid
    
  }
  
  cols <- c()
  for(i in seq_along(model)) {
    
    col.fitted <- paste0(model[i], ".fit")
    col.resid <- paste0(model[i], ".resid")
    
    cols[length(cols) + 1] <- col.fitted
    cols[length(cols) + 1] <- col.resid
    
  }
  
  colnames(fitted.data) <- c("duration", "log.duration", "max.speed", 
                             cols)
  
  fitted.data
  
}
