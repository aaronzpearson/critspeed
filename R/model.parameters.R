#' Summary of the Non-Linear Critical Speed Model Regressions
#'
#' A \code{data.frame} that contains the model parameters and either model goodness-of-fit or parameter fits.
#'
#' @param data data set containing at least two variables (see notes)
#' @param model critical speed model name as either \code{"two.param"}, \code{"three.param"}, \code{"five.param"}, or \code{"omvd"}
#' @param output either \code{model.fit} which returns parameters and model goodness-of-fit or \code{parameter.fit} which returns extensive parameter fit information 
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
#' \item \code{data} is a \code{data.frame} must satisfy the following criteria:
#' \itemize{
#' \item The first column is the duration variable
#' \item The second column is the maximal mean speed or maximal median speed variable
#' \item Subsequent columns will be disregarded
#' }
#' 
#' Users can generate appropriate \code{data.frame}s using the \code{critspeed()} function
#' 
#' \item \code{cv.2} is the minimum duration used to fit the two parameter model. The default is set to 120 s as suggested by the prevailing 
#' literature.
#' 
#' \item \code{lower}, \code{upper}, and \code{start} are the lower and upper bounds for the parameter fits, and initial parameter values, respectively 
#' The default values are set based on various considerations including the prevailing literature and trial-and-error. The defaults are 
#' outlined below in the form of \code{parameter: lower bound, start value, upper bound}:
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
#' @seealso \code{\link{model}}, \code{\link{fitted.model}}, \code{\link{critspeed}}
#'
#' @export
#' 
#' @examples 
#' 
#' data(sessionMaxMeanSpeed)
#' data(sessionMaxMedianSpeed)
#' 
#' # Max Mean Model Parameters
#' 
#' max.mean.parameters <- model.parameters(data = sessionMaxMeanSpeed,
#' model = c("two.param", "three.param", "five.param", "omvd"),
#' output = "model.fit",
#' cv.2 = 0,
#' lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
#' upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
#' start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2))
#' 
#' max.mean.parameters
#' 
#' # Max Median Model Parameters
#' 
#' max.median.parameters <- model.parameters(data = sessionMaxMedianSpeed,
#' model = c("two.param", "three.param", "five.param", "omvd"),
#' output = "parameter.fit",
#' cv.2 = 120,
#' lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
#' upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
#' start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2))
#' 
#' max.median.parameters 
model.parameters <- function(data,
                             model = c("two.param", "three.param", "five.param", "omvd"),
                             output = c("model.fit", "parameter.fit"),
                             cv.2 = 120,
                             lower = c(cs = 1, dp = 10, v0 = 5.8, a = NA, b = NA, f = NA),
                             upper = c(cs = 5.8, dp = 800, v0 = 12, a = NA, b = NA, f = NA),
                             start = c(cs = 4, dp = 150, v0 = 9, a = -2.71, b = 2.195, f = 0.2)) {
  
  
  colnames(data) <- c("duration", "max.mean.speed")
  
  if(length(output) > 1L) {output = "model.fit"}
  
  
  if(output == "model.fit") {
    
    fits <- data.frame(model = NA,
                       crit.speed = NA,
                       d.prime = NA,
                       max.speed = NA)
    
    for(i in seq_along(model)) {
      
      fitted.model <-   callCS("models",
                               model = model[i],
                               data = data,
                               cv.2 = cv.2,
                               lower = lower,
                               upper = upper,
                               start = start)
      
      model.fit <-   callCS("model.return",
                            model.fit = fitted.model,
                            model = model[i],
                            output = output)
      
      fits <- data.table:::rbind.data.table(fits, model.fit, fill = TRUE)
      
      
    }
    
    return(fits[-1, ])
    
  }
  
  
  if(output == "parameter.fit") {
    
    fits <- NULL
    
    for(i in seq_along(model)) {
      
      fitted.model <-   callCS("models",
                               model = model[i],
                               data = data,
                               cv.2 = cv.2,
                               lower = lower,
                               upper = upper,
                               start = start)
      
      model.fit <-   callCS("model.return",
                            model.fit = fitted.model,
                            model = model[i],
                            output = output)
      
      model.fit <- cbind(
        data.frame(parameter = row.names(model.fit)), 
        model.fit)
      
      model.name <- data.frame(model = model[i])
      
      model.fit <- cbind(model.name, model.fit)
      
      fits <- rbind(fits, model.fit)
      
      row.names(fits) <- 1:nrow(fits)
      
    }
    
    fits
    
  }
  
  
}

## helper functions ##

# nls return for *one* model 
# when called explicitly, extends \code{model.parameters()} by also returning nls summary
# removed from model.parameters because it is redundant with \code{model()}
model.return <- function(model.fit, 
                         model = c("two.param", "three.param", "five.param", "omvd"),
                         output = c("model.fit", "parameter.fit", "model")) {
  
  fit <- switch(output,
                model.fit = callCS("model.summary.fit",
                                   model = model,
                                   model.fit = model.fit),
                parameter.fit = callCS("model.parameter.fit",
                                       model.fit = model.fit),
                model = callCS("model.summary",
                               model.fit = model.fit)
  )
  
  fit
  
}

# nls summary
model.summary <- function(model.fit) {
  
  summary(model.fit)
  
}

# nls parameter fits
model.parameter.fit <- function(model.fit) {
  
  summary(model.fit)$coefficients
  
}

# nls goodness of fit 
goodness.of.fit <- function(model.fit) {
  
  data.frame(rss = model.fit$m$deviance(),
             rse = summary(model.fit)$sigma,
             AIC = AIC(model.fit),
             BIC = BIC(model.fit))
  
}

# nls parameters & goodness of fit
model.summary.fit <- function(model.fit,
                              model = c("two.param", "three.param", "five.param", "omvd")) {
  
  summary.abridged <- data.frame(model = model) # model name
  summary.abridged <- cbind(summary.abridged, t(data.frame(coef(model.fit)))) # model name & model parameters
  
  summary.abridged <- cbind(summary.abridged, 
                            goodness.of.fit(model.fit)) # model name, model parameters, model goodness of fit
  
  row.names(summary.abridged) <- 1:nrow(summary.abridged)  # set names 
  
  summary.abridged
  
}